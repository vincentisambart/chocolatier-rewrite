use crate::common::{Error, ModPath, Result};
use crate::parse::{ObjCExpr, ObjCMacroReceiver};
use crate::read::ModContent;
use crate::skel::{Index, ObjCEntity, Overview, RustEntityPath};
use chocolatier_objc_parser::{ast as objc_ast, index as objc_index};
use objc_ast::ObjCMethodKind;
use std::path::{Path, PathBuf};
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
struct ResolvedObjCMethod {
    receiver: ObjCMethodReceiver,
    method: objc_ast::ObjCMethod,
}

#[derive(Debug)]
struct ResolvedObjCProperty {
    receiver: ObjCMethodReceiver,
    property: objc_ast::Property,
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
    ) -> Option<ResolvedObjCMethod> {
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
                if let Some(resolved) = def.adopted_protocols.iter().find_map(|protoc| {
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
                    .find(|method| method.kind == method_kind && method.name == selector)
                    .map(|method| ResolvedObjCMethod {
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
                    .map(|method| ResolvedObjCMethod {
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
        is_class: bool,
        must_be_writable: bool,
    ) -> Option<ResolvedObjCProperty> {
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
                        is_class,
                        must_be_writable,
                    )
                }) {
                    return Some(resolved);
                }
                if let Some(resolved) = def.adopted_protocols.iter().find_map(|protoc| {
                    self.resolve_property(
                        &ObjCMethodReceiverRef::Protocol(protoc),
                        name,
                        is_class,
                        must_be_writable,
                    )
                }) {
                    return Some(resolved);
                }

                def.properties
                    .iter()
                    .find(|prop| {
                        prop.is_class == is_class
                            && (!must_be_writable || prop.is_writable)
                            && prop.name == name
                    })
                    .map(|prop| ResolvedObjCProperty {
                        receiver: receiver.to_owned(),
                        property: prop.clone(),
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
                        is_class,
                        must_be_writable,
                    )
                }) {
                    return Some(resolved);
                }

                def.properties
                    .iter()
                    .find(|prop| {
                        prop.property.is_class == is_class
                            && (!must_be_writable || prop.property.is_writable)
                            && prop.property.name == name
                    })
                    .map(|prop| ResolvedObjCProperty {
                        receiver: receiver.to_owned(),
                        property: prop.property.clone(),
                    })
            }
        }
    }

    fn resolve(
        &self,
        objc_expr: &ObjCExpr,
        objc_entity: Option<&ObjCEntity>,
        rust_method_has_receiver: Option<bool>,
    ) -> Result<()> {
        let method_kind = match objc_expr.receiver() {
            ObjCMacroReceiver::SelfValue(token) => match rust_method_has_receiver {
                Some(has_receiver) => {
                    if has_receiver {
                        ObjCMethodKind::Instance
                    } else {
                        ObjCMethodKind::Class
                    }
                }
                None => {
                    return Err(Error::at_loc(
                        self.full_file_path(),
                        token,
                        "`self` can only be used inside methods",
                    ))
                }
            },
            ObjCMacroReceiver::Class(_) => ObjCMethodKind::Class,
            ObjCMacroReceiver::MethodCall(_) => todo!(),
        };
        let receiver = match objc_expr.receiver() {
            ObjCMacroReceiver::SelfValue(self_token) => match objc_entity {
                Some(entity) => match entity {
                    ObjCEntity::Protocol(protoc) => ObjCMethodReceiver::Protocol(protoc.to_string()),
                    ObjCEntity::Interface(interf) => ObjCMethodReceiver::Interface(interf.to_string()),
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
            ObjCMacroReceiver::MethodCall(_) => todo!(),
        };
        match objc_expr {
            ObjCExpr::MethodCall(call) => {
                let sel = call.selector();
                let _resolved = match self.resolve_method(&receiver.to_ref(), &sel, method_kind) {
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
            }
            ObjCExpr::PropertyGet(_) | ObjCExpr::PropertySet(_) => {
                let (property_name, must_be_writable) = match objc_expr {
                    ObjCExpr::MethodCall(_) => unreachable!(),
                    ObjCExpr::PropertyGet(get) => (&get.property_name, false),
                    ObjCExpr::PropertySet(set) => (&set.property_name, true),
                };
                let name = property_name.to_string();
                let is_class_property = match method_kind {
                    ObjCMethodKind::Class => true,
                    ObjCMethodKind::Instance => false,
                };
                let span = property_name.span();

                let _resolved = match self.resolve_property(
                    &receiver.to_ref(),
                    &name,
                    is_class_property,
                    must_be_writable,
                ) {
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

                        if must_be_writable
                            && self
                                .resolve_property(
                                    &receiver.to_ref(),
                                    &name,
                                    is_class_property,
                                    false,
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
            }
        }
        Ok(())
    }
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
        resolver.resolve(&objc_expr, self.objc_entity, self.rust_method_has_receiver)?;

        Ok(syn::Expr::Macro(expr_mac.clone()))
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
