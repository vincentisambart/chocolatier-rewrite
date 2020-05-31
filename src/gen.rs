use crate::common::{Error, ModPath, Result};
use crate::read::ModContent;
use crate::skel::{Index, ObjCEntity, Overview, RustEntityPath};
use std::path::{Path, PathBuf};
use syn::visit_mut::VisitMut;

struct ObjCMacroVisit<'a> {
    base_dir: &'a Path,
    file_rel_path: &'a Path,
    mod_path: &'a ModPath,
    err: Option<Error>,
    objc_entity: Option<&'a ObjCEntity>,
    rust_method_has_receiver: Option<bool>,
    index: &'a Index,
}

impl ObjCMacroVisit<'_> {
    fn full_file_path(&self) -> PathBuf {
        self.base_dir.join(self.file_rel_path)
    }

    fn syn_err(&self, err: syn::Error) -> Error {
        Error::syn_err_rel(err, &self.base_dir, self.file_rel_path)
    }

    fn err_at_loc<Spanned, IntoString>(&self, spanned: Spanned, message: IntoString) -> Error
    where
        IntoString: Into<String>,
        Spanned: syn::spanned::Spanned,
    {
        Error::at_loc(self.full_file_path(), spanned, message)
    }

    fn do_visit_expr_macro(&mut self, expr_mac: &'_ mut syn::ExprMacro) -> Result<()> {
        if !expr_mac.mac.path.is_ident("objc") {
            syn::visit_mut::visit_expr_macro_mut(self, expr_mac);
            return Ok(());
        }

        let _objc_expr: crate::parse::ObjCExpr =
            expr_mac.mac.parse_body().map_err(|err| self.syn_err(err))?;

        // TODO

        Ok(())
    }
}

impl VisitMut for ObjCMacroVisit<'_> {
    fn visit_item_mod_mut(&mut self, item_mod: &'_ mut syn::ItemMod) {
        if self.err.is_some() {
            return;
        }

        let new_mod_path = self.mod_path.child(item_mod.ident.to_string());
        let mut child = ObjCMacroVisit {
            mod_path: &new_mod_path,
            err: None,
            index: self.index,
            ..*self
        };
        syn::visit_mut::visit_item_mod_mut(&mut child, item_mod);
        self.err = child.err;
    }

    fn visit_item_impl_mut(&mut self, item_impl: &'_ mut syn::ItemImpl) {
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
            index: self.index,
            ..*self
        };
        syn::visit_mut::visit_item_impl_mut(&mut child, item_impl);
        self.err = child.err;
    }

    fn visit_item_trait_mut(&mut self, item_trait: &'_ mut syn::ItemTrait) {
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
            index: self.index,
            ..*self
        };
        syn::visit_mut::visit_item_trait_mut(&mut child, item_trait);
        self.err = child.err;
    }

    fn visit_impl_item_method_mut(&mut self, item_method: &'_ mut syn::ImplItemMethod) {
        if self.err.is_some() {
            return;
        }

        let mut child = ObjCMacroVisit {
            err: None,
            rust_method_has_receiver: Some(item_method.sig.receiver().is_some()),
            index: self.index,
            ..*self
        };
        syn::visit_mut::visit_impl_item_method_mut(&mut child, item_method);
        self.err = child.err;
    }

    fn visit_trait_item_method_mut(&mut self, item_method: &'_ mut syn::TraitItemMethod) {
        if self.err.is_some() {
            return;
        }

        let mut child = ObjCMacroVisit {
            err: None,
            rust_method_has_receiver: Some(item_method.sig.receiver().is_some()),
            index: self.index,
            ..*self
        };
        syn::visit_mut::visit_trait_item_method_mut(&mut child, item_method);
        self.err = child.err;
    }

    fn visit_expr_macro_mut(&mut self, expr_mac: &'_ mut syn::ExprMacro) {
        if self.err.is_some() {
            return;
        }

        match self.do_visit_expr_macro(expr_mac) {
            Err(err) => self.err = Some(err),
            Ok(()) => {}
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
        };
        syn::visit_mut::visit_file_mut(&mut visit, &mut mod_content.file);
        if let Some(err) = visit.err {
            return Err(err);
        }
    }

    Ok(content)
}
