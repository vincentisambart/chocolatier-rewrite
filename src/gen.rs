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
    fn to_ref<'a>(&'a self) -> ObjCMethodReceiverRef<'a> {
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

    /// Trying to find the method it the oldest ancestor of receiver.
    fn resolve_method(
        &self,
        receiver: &ObjCMethodReceiverRef<'_>,
        selector: &str,
        method_kind: ObjCMethodKind,
        span: proc_macro2::Span,
    ) -> Result<Option<ResolvedObjCMethod>> {
        match *receiver {
            ObjCMethodReceiverRef::Interface(interf) => {
                let def = match self.objc_index.interfaces.get(interf) {
                    Some(def) => def,
                    None => {
                        return Err(Error::at_loc(
                            self.full_file_path(),
                            span,
                            format!("Could not find interface {}", interf),
                        ))
                    }
                };
                if let Some(ref superclass) = def.superclass {
                    if let Some(resolved) = self.resolve_method(
                        &ObjCMethodReceiverRef::Interface(superclass),
                        selector,
                        method_kind,
                        span,
                    )? {
                        return Ok(Some(resolved));
                    }
                }
                for protoc in &def.adopted_protocols {
                    if let Some(resolved) = self.resolve_method(
                        &ObjCMethodReceiverRef::Protocol(protoc),
                        selector,
                        method_kind,
                        span,
                    )? {
                        return Ok(Some(resolved));
                    }
                }

                for method in &def.methods {
                    if method.kind == method_kind && method.name == selector {
                        return Ok(Some(ResolvedObjCMethod {
                            receiver: receiver.to_owned(),
                            method: method.clone(),
                        }));
                    }
                }
            }
            ObjCMethodReceiverRef::Protocol(protoc) => {
                let def = match self.objc_index.protocols.get(protoc) {
                    Some(def) => def,
                    None => {
                        return Err(Error::at_loc(
                            self.full_file_path(),
                            span,
                            format!("Could not find protocol {}", protoc),
                        ))
                    }
                };

                for protoc in &def.inherited_protocols {
                    if let Some(resolved) = self.resolve_method(
                        &ObjCMethodReceiverRef::Protocol(protoc),
                        selector,
                        method_kind,
                        span,
                    )? {
                        return Ok(Some(resolved));
                    }
                }

                for method in &def.methods {
                    if method.method.kind == method_kind && method.method.name == selector {
                        return Ok(Some(ResolvedObjCMethod {
                            receiver: receiver.to_owned(),
                            method: method.method.clone(),
                        }));
                    }
                }
            }
        }
        Ok(None)
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
        match objc_expr {
            ObjCExpr::MethodCall(call) => {
                let sel = call.selector();
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
                let _resolved =
                    self.resolve_method(&receiver.to_ref(), &sel, method_kind, call.span());
            }
            ObjCExpr::PropertyGet(_) => todo!(),
            ObjCExpr::PropertySet(_) => todo!(),
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
