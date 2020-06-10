use crate::common::{Error, ModPath, Result};
use crate::parse::{ObjCExpr, ObjCMacroReceiver, ObjCMethodParam, ObjCMethodParams};
use crate::read::ModContent;
use crate::skel::{Index, ObjCEntity, ObjCOrigin, Overview, RustEntityPath};
use chocolatier_objc_parser::{ast as objc_ast, index as objc_index};
use objc_ast::ObjCMethodKind;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use std::borrow::Cow;
use std::path::{Path, PathBuf};
use syn::parse_quote;
use syn::spanned::Spanned;
use syn::visit_mut::VisitMut;

enum ObjCMethodReceiverRef<'a> {
    Interface(&'a str),
    Protocol(&'a str),
}

impl<'a> ObjCMethodReceiverRef<'a> {
    fn to_owned(&self) -> ObjCMethodReceiver {
        match *self {
            Self::Interface(interf) => ObjCMethodReceiver::Interface(interf.to_owned()),
            Self::Protocol(protoc) => ObjCMethodReceiver::Protocol(protoc.to_owned()),
        }
    }
}

#[derive(Debug)]
enum ObjCMethodReceiver {
    Interface(String),
    Protocol(String),
}

impl ObjCMethodReceiver {
    fn to_ref(&'_ self) -> ObjCMethodReceiverRef<'_> {
        match self {
            Self::Interface(interf) => ObjCMethodReceiverRef::Interface(interf),
            Self::Protocol(protoc) => ObjCMethodReceiverRef::Protocol(protoc),
        }
    }
}

#[derive(Debug)]
struct ResolvedMethod {
    receiver: ObjCMethodReceiver,
    method: objc_ast::ObjCMethod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PropertyAccess {
    Read,
    Write,
}

#[derive(Debug)]
struct ResolvedProperty {
    receiver: ObjCMethodReceiver,
    property: objc_ast::Property,
    access: PropertyAccess,
}

#[derive(Debug)]
enum ResolvedCallable {
    Method(ResolvedMethod),
    Property(ResolvedProperty),
}

impl ResolvedCallable {
    fn receiver(&self) -> &ObjCMethodReceiver {
        match self {
            ResolvedCallable::Method(method) => &method.receiver,
            ResolvedCallable::Property(property) => &property.receiver,
        }
    }

    fn method_kind(&self) -> ObjCMethodKind {
        match self {
            ResolvedCallable::Method(method) => method.method.kind,
            ResolvedCallable::Property(property) => match property.property.is_class {
                true => ObjCMethodKind::Class,
                false => ObjCMethodKind::Instance,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SelfKind {
    Class,
    Instance,
}

struct ObjCResolver<'a> {
    objc_index: &'a objc_index::TypeIndex,
    base_dir: &'a Path,
    file_rel_path: &'a Path,
}

impl<'a> ObjCResolver<'a> {
    fn new(
        objc_index: &'a objc_index::TypeIndex,
        base_dir: &'a Path,
        file_rel_path: &'a Path,
    ) -> Self {
        Self {
            objc_index,
            base_dir,
            file_rel_path,
        }
    }

    fn full_file_path(&self) -> PathBuf {
        self.base_dir.join(self.file_rel_path)
    }

    /// Trying to find the method in the oldest ancestor of receiver.
    fn resolve_method(
        &self,
        receiver: &ObjCMethodReceiverRef<'_>,
        selector: &str,
        method_kind: ObjCMethodKind,
    ) -> Option<ResolvedMethod> {
        match *receiver {
            ObjCMethodReceiverRef::Interface(interf) => {
                // Here we should not be given an non existing type, so just panic if we cannot find it.
                let def = match self.objc_index.interfaces.get(interf) {
                    Some(def) => def,
                    None => panic!("Could not find interface {}", interf),
                };
                if let Some(resolved) = def.superclass.as_ref().and_then(|superclass| {
                    self.resolve_method(
                        &ObjCMethodReceiverRef::Interface(superclass),
                        selector,
                        method_kind,
                    )
                }) {
                    return Some(resolved);
                }
                if let Some(resolved) =
                    def.adopted_protocols
                        .iter()
                        .chain(self.objc_index.categories.get(interf).iter().flat_map(
                            |categories| {
                                categories
                                    .iter()
                                    .flat_map(|categ_def| categ_def.adopted_protocols.iter())
                            },
                        ))
                        .find_map(|protoc| {
                            self.resolve_method(
                                &ObjCMethodReceiverRef::Protocol(protoc),
                                selector,
                                method_kind,
                            )
                        })
                {
                    return Some(resolved);
                }

                def.methods
                    .iter()
                    .chain(
                        self.objc_index
                            .categories
                            .get(interf)
                            .iter()
                            .flat_map(|categories| {
                                categories
                                    .iter()
                                    .flat_map(|categ_def| categ_def.methods.iter())
                            }),
                    )
                    .find(|method| method.kind == method_kind && method.name == selector)
                    .map(|method| ResolvedMethod {
                        receiver: receiver.to_owned(),
                        method: method.clone(),
                    })
            }
            ObjCMethodReceiverRef::Protocol(protoc) => {
                // Here we should not be given an non existing type, so just panic if we cannot find it.
                let def = match self.objc_index.protocols.get(protoc) {
                    Some(def) => def,
                    None => panic!("Could not find protocol {}", protoc),
                };

                if let Some(resolved) = def.inherited_protocols.iter().find_map(|protoc| {
                    self.resolve_method(
                        &ObjCMethodReceiverRef::Protocol(protoc),
                        selector,
                        method_kind,
                    )
                }) {
                    return Some(resolved);
                }

                def.methods
                    .iter()
                    .find(|method| {
                        method.method.kind == method_kind && method.method.name == selector
                    })
                    .map(|method| ResolvedMethod {
                        receiver: receiver.to_owned(),
                        method: method.method.clone(),
                    })
            }
        }
    }

    /// Trying to find the property in the oldest ancestor of receiver.
    fn resolve_property(
        &self,
        receiver: &ObjCMethodReceiverRef<'_>,
        name: &str,
        method_kind: ObjCMethodKind,
        access: PropertyAccess,
    ) -> Option<ResolvedProperty> {
        let must_be_writable = match access {
            PropertyAccess::Read => false,
            PropertyAccess::Write => true,
        };
        let is_class_property = match method_kind {
            ObjCMethodKind::Class => true,
            ObjCMethodKind::Instance => false,
        };

        match *receiver {
            ObjCMethodReceiverRef::Interface(interf) => {
                // Here we should not be given an non existing type, so just panic if we cannot find it.
                let def = match self.objc_index.interfaces.get(interf) {
                    Some(def) => def,
                    None => panic!("Could not find interface {}", interf),
                };
                if let Some(resolved) = def.superclass.as_ref().and_then(|superclass| {
                    self.resolve_property(
                        &ObjCMethodReceiverRef::Interface(superclass),
                        name,
                        method_kind,
                        access,
                    )
                }) {
                    return Some(resolved);
                }
                if let Some(resolved) =
                    def.adopted_protocols
                        .iter()
                        .chain(self.objc_index.categories.get(interf).iter().flat_map(
                            |categories| {
                                categories
                                    .iter()
                                    .flat_map(|categ_def| categ_def.adopted_protocols.iter())
                            },
                        ))
                        .find_map(|protoc| {
                            self.resolve_property(
                                &ObjCMethodReceiverRef::Protocol(protoc),
                                name,
                                method_kind,
                                access,
                            )
                        })
                {
                    return Some(resolved);
                }

                def.properties
                    .iter()
                    .chain(
                        self.objc_index
                            .categories
                            .get(interf)
                            .iter()
                            .flat_map(|categories| {
                                categories
                                    .iter()
                                    .flat_map(|categ_def| categ_def.properties.iter())
                            }),
                    )
                    .find(|prop| {
                        prop.is_class == is_class_property
                            && (!must_be_writable || prop.is_writable)
                            && prop.name == name
                    })
                    .map(|prop| ResolvedProperty {
                        receiver: receiver.to_owned(),
                        property: prop.clone(),
                        access,
                    })
            }
            ObjCMethodReceiverRef::Protocol(protoc) => {
                // Here we should not be given an non existing type, so just panic if we cannot find it.
                let def = match self.objc_index.protocols.get(protoc) {
                    Some(def) => def,
                    None => panic!("Could not find protocol {}", protoc),
                };

                if let Some(resolved) = def.inherited_protocols.iter().find_map(|protoc| {
                    self.resolve_property(
                        &ObjCMethodReceiverRef::Protocol(protoc),
                        name,
                        method_kind,
                        access,
                    )
                }) {
                    return Some(resolved);
                }

                def.properties
                    .iter()
                    .find(|prop| {
                        prop.property.is_class == is_class_property
                            && (!must_be_writable || prop.property.is_writable)
                            && prop.property.name == name
                    })
                    .map(|prop| ResolvedProperty {
                        receiver: receiver.to_owned(),
                        property: prop.property.clone(),
                        access,
                    })
            }
        }
    }

    fn resolve(
        &self,
        objc_expr: &ObjCExpr,
        objc_entity: Option<&ObjCEntity>,
        self_kind: Option<SelfKind>,
    ) -> Result<ResolvedCallable> {
        let method_kind = match objc_expr.receiver() {
            ObjCMacroReceiver::SelfValue(token) => match self_kind {
                Some(self_kind) => match self_kind {
                    SelfKind::Class => ObjCMethodKind::Class,
                    SelfKind::Instance => ObjCMethodKind::Instance,
                },
                None => {
                    return Err(Error::at_loc(
                        self.full_file_path(),
                        token,
                        "`self` can only be used inside methods",
                    ))
                }
            },
            ObjCMacroReceiver::Class(_) => ObjCMethodKind::Class,
            ObjCMacroReceiver::SelfAlloc(_, call) => match self_kind {
                // [self alloc] is an instance
                Some(SelfKind::Class) => ObjCMethodKind::Instance,
                _ => {
                    return Err(Error::at_loc(
                        self.full_file_path(),
                        call.span(),
                        "alloc should only be used on a class",
                    ))
                }
            },
            ObjCMacroReceiver::ClassAlloc(_, _) => ObjCMethodKind::Instance,
            ObjCMacroReceiver::SelfClass(_, _) => ObjCMethodKind::Class,
            ObjCMacroReceiver::SelfClassAlloc(_, _) => ObjCMethodKind::Instance,
        };
        let receiver = match objc_expr.receiver() {
            ObjCMacroReceiver::SelfValue(self_token)
            | ObjCMacroReceiver::SelfAlloc(self_token, _)
            | ObjCMacroReceiver::SelfClass(self_token, _)
            | ObjCMacroReceiver::SelfClassAlloc(self_token, _) => match objc_entity {
                Some(entity) => match entity {
                    ObjCEntity::Protocol(protoc) => {
                        ObjCMethodReceiver::Protocol(protoc.to_string())
                    }
                    ObjCEntity::Interface(interf) => {
                        ObjCMethodReceiver::Interface(interf.to_string())
                    }
                    ObjCEntity::Enum(_) => {
                        return Err(Error::at_loc(
                            self.full_file_path(),
                            self_token,
                            "methods cannot be called on enums",
                         ))
                    }
                },
                None => {
                    return Err(Error::at_loc(
                        self.full_file_path(),
                        self_token,
                        "`self` can only be used in an impl or trait linked to an ObjC interface or protocol",
                    ))
                }
            },
            ObjCMacroReceiver::Class(name) => ObjCMethodReceiver::Interface(name.to_string()),
            ObjCMacroReceiver::ClassAlloc(interf, _) => {
                ObjCMethodReceiver::Interface(interf.to_string())
            }
        };
        match objc_expr {
            ObjCExpr::MethodCall(call) => {
                let sel = call.selector();
                let resolved = match self.resolve_method(&receiver.to_ref(), &sel, method_kind) {
                    Some(resolved) => resolved,
                    None => {
                        let kind = match method_kind {
                            ObjCMethodKind::Class => "+",
                            ObjCMethodKind::Instance => "-",
                        };
                        let receiver_name = match receiver {
                            ObjCMethodReceiver::Interface(name) => name,
                            ObjCMethodReceiver::Protocol(name) => format!("id<{}>", name),
                        };
                        return Err(Error::at_loc(
                            self.full_file_path(),
                            call.span(),
                            format!(
                                "could not find method {kind}[{receiver} {sel}]",
                                kind = kind,
                                receiver = receiver_name,
                                sel = sel,
                            ),
                        ));
                    }
                };
                Ok(ResolvedCallable::Method(resolved))
            }
            ObjCExpr::PropertyGet(_) | ObjCExpr::PropertySet(_) => {
                let (property_name, access) = match objc_expr {
                    ObjCExpr::MethodCall(_) => unreachable!(),
                    ObjCExpr::PropertyGet(get) => (&get.property_name, PropertyAccess::Read),
                    ObjCExpr::PropertySet(set) => (&set.property_name, PropertyAccess::Write),
                };
                let name = property_name.to_string();
                let span = property_name.span();

                let resolved =
                    match self.resolve_property(&receiver.to_ref(), &name, method_kind, access) {
                        Some(resolved) => resolved,
                        None => {
                            let kind = match method_kind {
                                ObjCMethodKind::Class => "class",
                                ObjCMethodKind::Instance => "instance",
                            };
                            let receiver_name = match &receiver {
                                ObjCMethodReceiver::Interface(name) => name.clone(),
                                ObjCMethodReceiver::Protocol(name) => format!("id<{}>", name),
                            };

                            if access == PropertyAccess::Write
                                && self
                                    .resolve_property(
                                        &receiver.to_ref(),
                                        &name,
                                        method_kind,
                                        PropertyAccess::Read,
                                    )
                                    .is_some()
                            {
                                return Err(Error::at_loc(
                                    self.full_file_path(),
                                    span,
                                    format!(
                                        "{kind} property \"{name}\" on {receiver} is not writable",
                                        kind = kind,
                                        receiver = receiver_name,
                                        name = name,
                                    ),
                                ));
                            } else {
                                return Err(Error::at_loc(
                                    self.full_file_path(),
                                    span,
                                    format!(
                                        "could not find {kind} property \"{name}\" on {receiver}",
                                        kind = kind,
                                        receiver = receiver_name,
                                        name = name,
                                    ),
                                ));
                            }
                        }
                    };
                Ok(ResolvedCallable::Property(resolved))
            }
        }
    }
}

fn rust_param_conv(
    objc_index: &objc_index::TypeIndex,
    core_mod_path: &ModPath,
    rust_objc_param: &ObjCMethodParam,
    objc_param: &objc_ast::ObjCParam,
    ty: &objc_ast::AttributedType,
) -> syn::Expr {
    let expr = &rust_objc_param.expr;
    match &ty.ty {
        objc_ast::Type::ObjPtr(ptr) => {
            let mut restric: Vec<TokenStream> = Vec::new();
            restric.push(quote! { #core_mod_path::ObjCPtr });
            match &ptr.kind {
                objc_ast::ObjPtrKind::Class => todo!(),
                objc_ast::ObjPtrKind::Id(id) => {
                    restric.extend(
                        id.protocols
                            .iter()
                            .map(|protoc| format_ident!("{}Protocol", protoc).to_token_stream()),
                    );
                }
                objc_ast::ObjPtrKind::SomeInstance(desc) => {
                    restric.push(format_ident!("{}Interface", desc.interface).to_token_stream());
                    restric.extend(
                        desc.protocols
                            .iter()
                            .map(|protoc| format_ident!("{}Protocol", protoc).to_token_stream()),
                    );
                }
                objc_ast::ObjPtrKind::Block(_) => todo!(),
                objc_ast::ObjPtrKind::TypeParam(_) => todo!(),
            };
            let is_consumed = objc_param.attrs.contains(&objc_ast::Attr::NSConsumed);
            match ptr.nullability {
                Some(objc_ast::Nullability::NonNull) => {
                    let received_ty = if is_consumed {
                        quote! { T }
                    } else {
                        quote! { &T }
                    };
                    parse_quote! {
                        ({fn conv<T: #(#restric)+*>(ptr: #received_ty) -> std::ptr::NonNull<#core_mod_path::ObjCObject> {
                            ptr.as_raw()
                        }; conv})(#expr)
                    }
                }
                Some(objc_ast::Nullability::Nullable) | None => {
                    let received_ty = if is_consumed {
                        quote! { Option<T> }
                    } else {
                        quote! { Option<&T> }
                    };
                    parse_quote! {
                        ({fn conv<T: #(#restric)+*>(ptr: #received_ty) -> *mut #core_mod_path::ObjCObject {
                            ptr.map_or(std::ptr::null_mut(), |ptr| ptr.as_raw().as_ptr())
                        }; conv})(#expr)
                    }
                }
            }
        }
        objc_ast::Type::Num(_) => expr.clone(),
        objc_ast::Type::Typedef(typedef) => {
            let decl = objc_index.typedefs.get(&typedef.name).unwrap();
            rust_param_conv(
                objc_index,
                core_mod_path,
                rust_objc_param,
                objc_param,
                &decl.underlying,
            )
        }
        _ => todo!("unsupported param type {:?}", ty),
    }
}

fn rust_ret_conv(
    index: &Index,
    objc_index: &objc_index::TypeIndex,
    core_mod_path: &ModPath,
    func_call: syn::Expr,
    ty: &objc_ast::AttributedType,
    base_dir: &Path,
    file_rel_path: &Path,
    span: proc_macro2::Span,
) -> Result<syn::Expr> {
    let conv: syn::Expr = match &ty.ty {
        objc_ast::Type::Void => func_call,
        objc_ast::Type::Typedef(typedef) => match typedef.name.as_str() {
            "BOOL" => parse_quote!(#func_call != 0),
            "instancetype" => parse_quote! {
                <Self as #core_mod_path::ObjCPtr>::from_raw_unchecked(#func_call)
            },
            _ => {
                let decl = objc_index.typedefs.get(&typedef.name).unwrap();
                rust_ret_conv(
                    index,
                    objc_index,
                    core_mod_path,
                    func_call,
                    &decl.underlying,
                    base_dir,
                    file_rel_path,
                    span,
                )?
            }
        },
        objc_ast::Type::ObjPtr(ptr) => {
            use objc_ast::ObjPtrKind;
            match &ptr.kind {
                ObjPtrKind::Class => func_call,
                ObjPtrKind::Id(_) => todo!(),
                ObjPtrKind::SomeInstance(instance) => {
                    match index.interf_structs.get(&instance.interface) {
                        Some(loc) => {
                            let path = &loc.path;
                            parse_quote! {
                                <#path as #core_mod_path::ObjCPtr>::from_raw_unchecked(#func_call)
                            }
                        }
                        None => {
                            let file_path = base_dir.join(file_rel_path);
                            let message = format!(
                                "could not find Rust concrete type for return value @interface {}",
                                instance.interface
                            );
                            return Err(Error::at_loc(file_path, span, message));
                        }
                    }
                }
                ObjPtrKind::Block(_) => todo!(),
                ObjPtrKind::TypeParam(_) => todo!(),
            }
        }
        objc_ast::Type::Pointer(ptr) => match &ptr.pointee.ty {
            objc_ast::Type::Num(_) => func_call,
            _ => todo!(),
        },
        objc_ast::Type::Num(_) => func_call,
        _ => todo!("unhandled type {:?}", ty),
    };
    Ok(conv)
}

const UNKNOWN_ORIGIN: &str = "_";
const ORIGIN_CORE: &str = "core";
const ORIGIN_SYSTEM: &str = "system";

fn c_func_name(objc_index: &objc_index::TypeIndex, callable: &ResolvedCallable) -> String {
    use objc_ast::Origin;

    let (receiver_name, origin, receiver_kind_name) = match callable.receiver() {
        ObjCMethodReceiver::Interface(name) => {
            let origin = objc_index.interfaces.get(name).unwrap().origin.clone();
            (name, origin, "Protocol")
        }
        ObjCMethodReceiver::Protocol(name) => {
            let origin = objc_index.protocols.get(name).unwrap().origin.clone();
            (name, origin, "Interface")
        }
    };
    let method_kind = callable.method_kind();
    let origin_name = match &origin {
        None => UNKNOWN_ORIGIN,
        Some(Origin::ObjCCore) => ORIGIN_CORE,
        Some(Origin::System) => ORIGIN_SYSTEM,
        Some(Origin::Framework(framework)) => framework.as_str(),
        Some(Origin::Library(lib)) => lib.as_str(),
    };
    let method_kind_name = match method_kind {
        ObjCMethodKind::Class => "class",
        ObjCMethodKind::Instance => "instance",
    };
    let escaped_sel = match callable {
        ResolvedCallable::Method(method) => Cow::Owned(method.method.name.replace(":", "_")),
        ResolvedCallable::Property(property) => Cow::Borrowed(&property.property.name),
    };

    // TODO: Also add the name of the crate generating the code.

    format!(
        "choco_{origin_name}_{receiver_name}{receiver_kind_name}_{method_kind_name}_{escaped_sel}",
        origin_name = origin_name,
        receiver_name = receiver_name,
        receiver_kind_name = receiver_kind_name,
        method_kind_name = method_kind_name,
        escaped_sel = escaped_sel
    )
}

impl ToTokens for ModPath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use proc_macro2::{Ident, Span};
        let segments = self
            .0
            .iter()
            .map(|segment| Ident::new(segment, Span::call_site()));
        tokens.extend(quote!(crate::#(#segments)::*))
    }
}

fn replacement_expr(
    index: &Index,
    objc_index: &objc_index::TypeIndex,
    objc_expr: &ObjCExpr,
    base_dir: &Path,
    file_rel_path: &Path,
    callable: &ResolvedCallable,
    self_kind: Option<SelfKind>,
) -> Result<syn::Expr> {
    let core_mod_path = index.mod_per_objc_origin.get(&ObjCOrigin::Core).unwrap();
    let c_func_name = c_func_name(objc_index, callable);
    let c_func_name_span = match objc_expr {
        ObjCExpr::MethodCall(call) => call.span(),
        ObjCExpr::PropertyGet(get) => get.property_name.span(),
        ObjCExpr::PropertySet(set) => set.property_name.span(),
    };
    let c_func_ident = proc_macro2::Ident::new(&c_func_name, c_func_name_span);

    let mut params: Vec<syn::Expr> = Vec::new();

    match callable.method_kind() {
        ObjCMethodKind::Class => match &objc_expr.receiver() {
            ObjCMacroReceiver::SelfValue(_) => match self_kind {
                Some(SelfKind::Class) => {
                    params.push(parse_quote!(<Self as #core_mod_path::ObjCPtr>::class()))
                }
                Some(SelfKind::Instance) => todo!(),
                None => unreachable!(),
            },
            ObjCMacroReceiver::Class(_) => todo!(),
            ObjCMacroReceiver::SelfAlloc(_, _) => todo!(),
            ObjCMacroReceiver::ClassAlloc(_, _) => todo!(),
            ObjCMacroReceiver::SelfClass(_, _) => todo!(),
            ObjCMacroReceiver::SelfClassAlloc(_, _) => todo!(),
        },
        ObjCMethodKind::Instance => {
            params.push(parse_quote!(<Self as #core_mod_path::ObjCPtr>::as_raw(self)))
        }
    }

    match &objc_expr {
        ObjCExpr::MethodCall(call) => match &call.params {
            ObjCMethodParams::Without(_) => {}
            ObjCMethodParams::With(mac_params) => match callable {
                ResolvedCallable::Method(method) => {
                    assert_eq!(mac_params.len(), method.method.params.len());
                    params.extend(mac_params.iter().zip(method.method.params.iter()).map(
                        |(rust_objc_param, objc_param)| {
                            rust_param_conv(
                                objc_index,
                                core_mod_path,
                                rust_objc_param,
                                objc_param,
                                &objc_param.ty,
                            )
                        },
                    ));
                }
                ResolvedCallable::Property(_) => todo!(),
            },
        },
        ObjCExpr::PropertyGet(_) => {}
        ObjCExpr::PropertySet(_) => todo!(),
    }

    let func_call: syn::Expr = parse_quote! {
       #c_func_ident(#(#params),*)
    };

    let result_ty = match &callable {
        ResolvedCallable::Method(method) => Some(&method.method.result),
        ResolvedCallable::Property(property) => match property.access {
            PropertyAccess::Read => Some(&property.property.value),
            PropertyAccess::Write => None,
        },
    };

    let converted_ret: syn::Expr = match result_ty {
        Some(ty) => rust_ret_conv(
            index,
            objc_index,
            core_mod_path,
            func_call,
            &ty,
            base_dir,
            file_rel_path,
            objc_expr.call_span(),
        )?,
        None => func_call,
    };

    Ok(converted_ret)
}

struct ObjCMacroVisit<'a> {
    base_dir: &'a Path,
    file_rel_path: &'a Path,
    mod_path: &'a ModPath,
    err: Option<Error>,
    objc_entity: Option<&'a ObjCEntity>,
    rust_method_has_receiver: Option<bool>,
    index: &'a Index,
    objc_index: &'a objc_index::TypeIndex,
}

impl ObjCMacroVisit<'_> {
    fn syn_err(&self, err: syn::Error) -> Error {
        Error::syn_err_rel(err, &self.base_dir, self.file_rel_path)
    }

    fn objc_macro_replacement(&mut self, expr_mac: &syn::ExprMacro) -> Result<syn::Expr> {
        assert!(expr_mac.mac.path.is_ident("objc"));

        let objc_expr: ObjCExpr = expr_mac.mac.parse_body().map_err(|err| self.syn_err(err))?;

        let resolver = ObjCResolver::new(self.objc_index, self.base_dir, self.file_rel_path);
        let self_kind = self
            .rust_method_has_receiver
            .map(|has_receiver| match has_receiver {
                true => SelfKind::Instance,
                false => SelfKind::Class,
            });
        let resolved = resolver.resolve(&objc_expr, self.objc_entity, self_kind)?;

        replacement_expr(
            self.index,
            self.objc_index,
            &objc_expr,
            self.base_dir,
            self.file_rel_path,
            &resolved,
            self_kind,
        )
    }
}

impl VisitMut for ObjCMacroVisit<'_> {
    fn visit_item_mod_mut(&mut self, item_mod: &mut syn::ItemMod) {
        if self.err.is_some() {
            return;
        }

        let new_mod_path = self.mod_path.child(item_mod.ident.to_string());
        let mut child = ObjCMacroVisit {
            mod_path: &new_mod_path,
            err: None,
            ..*self
        };
        syn::visit_mut::visit_item_mod_mut(&mut child, item_mod);
        self.err = child.err;
    }

    fn visit_item_impl_mut(&mut self, item_impl: &mut syn::ItemImpl) {
        if self.err.is_some() {
            return;
        }

        let entity_path = match &*item_impl.self_ty {
            syn::Type::Path(path) => RustEntityPath::from_type_path(&self.mod_path, path),
            _ => None,
        };
        let objc_entity =
            entity_path.and_then(|path| self.index.entity_objc_mapping.get(&path).cloned());

        let mut child = ObjCMacroVisit {
            err: None,
            objc_entity: objc_entity.as_ref(),
            ..*self
        };
        syn::visit_mut::visit_item_impl_mut(&mut child, item_impl);
        self.err = child.err;
    }

    fn visit_item_trait_mut(&mut self, item_trait: &mut syn::ItemTrait) {
        if self.err.is_some() {
            return;
        }

        let entity_path = RustEntityPath {
            mod_path: self.mod_path.clone(),
            name: item_trait.ident.to_string(),
        };
        let objc_entity = self.index.entity_objc_mapping.get(&entity_path).cloned();

        let mut child = ObjCMacroVisit {
            err: None,
            objc_entity: objc_entity.as_ref(),
            ..*self
        };
        syn::visit_mut::visit_item_trait_mut(&mut child, item_trait);
        self.err = child.err;
    }

    fn visit_impl_item_method_mut(&mut self, item_method: &mut syn::ImplItemMethod) {
        if self.err.is_some() {
            return;
        }

        let mut child = ObjCMacroVisit {
            err: None,
            rust_method_has_receiver: Some(item_method.sig.receiver().is_some()),
            ..*self
        };
        syn::visit_mut::visit_impl_item_method_mut(&mut child, item_method);
        self.err = child.err;
    }

    fn visit_trait_item_method_mut(&mut self, item_method: &mut syn::TraitItemMethod) {
        if self.err.is_some() {
            return;
        }

        let mut child = ObjCMacroVisit {
            err: None,
            rust_method_has_receiver: Some(item_method.sig.receiver().is_some()),
            ..*self
        };
        syn::visit_mut::visit_trait_item_method_mut(&mut child, item_method);
        self.err = child.err;
    }

    fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
        if self.err.is_some() {
            return;
        }

        match expr {
            syn::Expr::Macro(expr_mac) if expr_mac.mac.path.is_ident("objc") => {
                match self.objc_macro_replacement(expr_mac) {
                    Err(err) => self.err = Some(err),
                    Ok(new_expr) => *expr = new_expr,
                }
            }
            _ => syn::visit_mut::visit_expr_mut(self, expr),
        }
    }
}

pub fn generate(overview: &Overview) -> Result<Vec<ModContent>> {
    let mut content = overview.rust.crate_content.content.clone();
    for mod_content in content.iter_mut() {
        let mut visit = ObjCMacroVisit {
            base_dir: &overview.rust.crate_content.base_dir,
            file_rel_path: &mod_content.file_rel_path,
            mod_path: &mod_content.mod_path,
            err: None,
            objc_entity: None,
            rust_method_has_receiver: None,
            index: &overview.rust.index,
            objc_index: &overview.objc_index,
        };
        syn::visit_mut::visit_file_mut(&mut visit, &mut mod_content.file);
        if let Some(err) = visit.err {
            return Err(err);
        }
    }

    Ok(content)
}
