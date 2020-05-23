use crate::common::{Error, ModPath, Result};
use crate::crate_read::CrateContent;
use chocolatier_objc_parser::{ast as objc_ast, index as objc_index, xcode};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use syn::visit::Visit;

#[derive(Debug)]
enum ModObjCAttr {
    ObjCCore(syn::Ident),
    ObjCFramework {
        ident: syn::Ident,
        eq_token: syn::token::Eq,
        lit_str: syn::LitStr,
    },
}

impl ModObjCAttr {
    fn to_objc_origin(&self) -> ObjCOrigin {
        match self {
            ModObjCAttr::ObjCCore(_) => ObjCOrigin::Core,
            ModObjCAttr::ObjCFramework { lit_str, .. } => ObjCOrigin::Framework(lit_str.value()),
        }
    }
}

static MOD_ATTR_NAMES: &[&str] = &["objc_core", "objc_framework"];
static TRAIT_ATTR_NAMES: &[&str] = &["objc_interface", "objc_protocol"];
static STRUCT_ATTR_NAMES: &[&str] = &["objc_interface"];

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ObjCOrigin {
    Core,
    Framework(String),
}

#[derive(Debug)]
pub struct ObjCTypeRustLoc {
    pub rust_name: String,
    pub span: proc_macro2::Span,
    pub file_rel_path: PathBuf,
    pub mod_path: ModPath,
}

impl ObjCTypeRustLoc {
    fn same_module_and_name(&self, other: &Self) -> bool {
        self.rust_name == other.rust_name && self.mod_path == other.mod_path
    }
}

pub struct ModContent {
    pub file_rel_path: PathBuf,
    pub file: syn::File,
}

impl std::fmt::Debug for ModContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModContent")
            .field("file_rel_path", &self.file_rel_path)
            // The file field is so displaying it doesn't bring anything.
            .field("file", &format_args!("(...)"))
            .finish()
    }
}

// Until the str_strip feature becomes stable.
fn strip_suffix<'a>(text: &'a str, suffix: &str) -> Option<&'a str> {
    if text.ends_with(suffix) {
        Some(&text[..text.len() - suffix.len()])
    } else {
        None
    }
}

struct FileVisit<'a> {
    base_dir: &'a Path,
    file_rel_path: &'a Path,
    mod_path: &'a ModPath,
    err: Option<Error>,
    mod_for_objc_origin: &'a mut HashMap<ObjCOrigin, ModPath>,
    interf_traits: &'a mut HashMap<String, ObjCTypeRustLoc>,
    interf_structs: &'a mut HashMap<String, ObjCTypeRustLoc>,
    protocols: &'a mut HashMap<String, ObjCTypeRustLoc>,
}

impl<'a> FileVisit<'a> {
    fn full_file_path(&self) -> PathBuf {
        self.base_dir.join(self.file_rel_path)
    }

    fn syn_err(&self, err: syn::Error) -> Error {
        Error::syn_err_rel(err, &self.base_dir, self.file_rel_path)
    }

    fn loc_err<Spanned, IntoString>(&self, spanned: Spanned, message: IntoString) -> Error
    where
        IntoString: Into<String>,
        Spanned: syn::spanned::Spanned,
    {
        Error::loc_err(self.full_file_path(), spanned, message)
    }

    fn do_visit_item_mod(&mut self, item_mod: &'_ syn::ItemMod) -> Result<()> {
        let objc_attrs: Vec<_> = item_mod
            .attrs
            .iter()
            .filter(|attr| MOD_ATTR_NAMES.iter().any(|name| attr.path.is_ident(name)))
            .collect();

        if objc_attrs.len() > 1 {
            return Err(self.loc_err(
                objc_attrs[1],
                "there should only be one objc_* attribute on a module",
            ));
        }

        let ident = &item_mod.ident;
        let new_mod_path = self.mod_path.child(ident.to_string());

        if let Some(attr) = objc_attrs.first() {
            let meta = attr.parse_meta().map_err(|err| self.syn_err(err))?;
            let res_attr = match meta {
                syn::Meta::Path(path) if path.is_ident("objc_core") => {
                    ModObjCAttr::ObjCCore(path.get_ident().unwrap().clone())
                }
                syn::Meta::NameValue(syn::MetaNameValue {
                    path,
                    eq_token,
                    lit: syn::Lit::Str(lit_str),
                }) if path.is_ident("objc_framework") => ModObjCAttr::ObjCFramework {
                    ident: path.get_ident().unwrap().clone(),
                    eq_token,
                    lit_str,
                },
                _ => {
                    return Err(self.loc_err(attr, "invalid attribute"));
                }
            };

            let origin = res_attr.to_objc_origin();
            if let Some(dup) = self.mod_for_objc_origin.remove(&origin) {
                return Err(self.loc_err(
                    attr,
                    format!("{} already stands for the same Objective-C source", dup),
                ));
            }
            self.mod_for_objc_origin
                .insert(origin, new_mod_path.clone());
        }

        if item_mod.content.is_some() {
            let mut child = FileVisit {
                base_dir: self.base_dir,
                file_rel_path: self.file_rel_path,
                mod_path: &new_mod_path,
                err: None,
                mod_for_objc_origin: self.mod_for_objc_origin,
                interf_traits: self.interf_traits,
                interf_structs: self.interf_structs,
                protocols: self.protocols,
            };
            syn::visit::visit_item_mod(&mut child, item_mod);
            match child.err {
                Some(err) => Err(err),
                None => Ok(()),
            }
        } else {
            Ok(())
        }
    }

    fn do_visit_item_trait(&mut self, item_trait: &'_ syn::ItemTrait) -> Result<()> {
        let objc_attrs: Vec<_> = item_trait
            .attrs
            .iter()
            .filter(|attr| TRAIT_ATTR_NAMES.iter().any(|name| attr.path.is_ident(name)))
            .collect();

        if objc_attrs.len() > 1 {
            return Err(self.loc_err(
                objc_attrs[1],
                "there should only be one objc_* attribute on a trait",
            ));
        }

        if let Some(attr) = objc_attrs.first() {
            let rust_name = item_trait.ident.to_string();
            if attr.path.is_ident("objc_interface") {
                let objc_name = match strip_suffix(&rust_name, "Interface") {
                    Some(objc_name) => objc_name,
                    None => {
                        return Err(Error::loc_err(
                            self.full_file_path(),
                            attr,
                            format!(
                                "\"{}\" is tagged #[objc_interface] so its name should end with \"Interface\"",
                                rust_name
                            ),
                        ))
                    }
                };

                let loc = ObjCTypeRustLoc {
                    rust_name: rust_name.clone(),
                    mod_path: self.mod_path.clone(),
                    span: item_trait.ident.span(),
                    file_rel_path: self.file_rel_path.to_owned(),
                };
                if let Some(existing_loc) = self.interf_traits.get(objc_name) {
                    if loc.same_module_and_name(existing_loc) {
                        return Err(self.loc_err(
                            &item_trait.ident,
                            format!(
                                "{} is already mapped to {}::{}",
                                objc_name, existing_loc.mod_path, existing_loc.rust_name
                            ),
                        ));
                    }
                } else {
                    self.interf_traits.insert(objc_name.to_owned(), loc);
                }
            } else if attr.path.is_ident("objc_protocol") {
                let objc_name = match strip_suffix(&rust_name, "Protocol") {
                    Some(without_suffix) => without_suffix,
                    None => {
                        return Err(Error::loc_err(
                            self.full_file_path(),
                            attr,
                            format!(
                                "\"{}\" is tagged #[objc_protocol] so its name should end with \"Protocol\"",
                                rust_name
                            ),
                        ))
                    }
                };

                let loc = ObjCTypeRustLoc {
                    rust_name: rust_name.clone(),
                    mod_path: self.mod_path.clone(),
                    span: item_trait.ident.span(),
                    file_rel_path: self.file_rel_path.to_owned(),
                };
                if let Some(existing_loc) = self.protocols.get(objc_name) {
                    if loc.same_module_and_name(existing_loc) {
                        return Err(self.loc_err(
                            &item_trait.ident,
                            format!(
                                "{} is already mapped to {}::{}",
                                objc_name, existing_loc.mod_path, existing_loc.rust_name
                            ),
                        ));
                    }
                } else {
                    self.protocols.insert(objc_name.to_owned(), loc);
                }
            } else {
                unreachable!()
            };
        }

        syn::visit::visit_item_trait(self, item_trait);
        Ok(())
    }

    fn do_visit_item_struct(&mut self, item_struct: &'_ syn::ItemStruct) -> Result<()> {
        let objc_attrs: Vec<_> = item_struct
            .attrs
            .iter()
            .filter(|attr| {
                STRUCT_ATTR_NAMES
                    .iter()
                    .any(|name| attr.path.is_ident(name))
            })
            .collect();

        if objc_attrs.len() > 1 {
            return Err(self.loc_err(
                objc_attrs[1],
                "there should only be one objc_* attribute on a trait",
            ));
        }

        if let Some(attr) = objc_attrs.first() {
            // For Rust structs, the Rust name is the same as the ObjC one.
            let name = item_struct.ident.to_string();
            if attr.path.is_ident("objc_interface") {
                let loc = ObjCTypeRustLoc {
                    rust_name: name.clone(),
                    mod_path: self.mod_path.clone(),
                    span: item_struct.ident.span(),
                    file_rel_path: self.file_rel_path.to_owned(),
                };
                if let Some(existing_loc) = self.interf_structs.get(&name) {
                    if loc.same_module_and_name(existing_loc) {
                        return Err(self.loc_err(
                            &item_struct.ident,
                            format!(
                                "{} is already mapped to {}::{}",
                                name, existing_loc.mod_path, existing_loc.rust_name
                            ),
                        ));
                    }
                } else {
                    self.interf_structs.insert(name, loc);
                }
            } else {
                unreachable!()
            };
        }

        syn::visit::visit_item_struct(self, item_struct);
        Ok(())
    }
}

impl Visit<'_> for FileVisit<'_> {
    fn visit_item_mod(&mut self, item_mod: &'_ syn::ItemMod) {
        if self.err.is_some() {
            return;
        }

        match self.do_visit_item_mod(item_mod) {
            Err(err) => self.err = Some(err),
            Ok(()) => {}
        }
    }

    fn visit_item_trait(&mut self, item_trait: &'_ syn::ItemTrait) {
        if self.err.is_some() {
            return;
        }

        match self.do_visit_item_trait(item_trait) {
            Err(err) => self.err = Some(err),
            Ok(()) => {}
        }
    }

    fn visit_item_struct(&mut self, item_struct: &'_ syn::ItemStruct) {
        if self.err.is_some() {
            return;
        }

        match self.do_visit_item_struct(item_struct) {
            Err(err) => self.err = Some(err),
            Ok(()) => {}
        }
    }
}

#[derive(Debug)]
pub struct RustOverview {
    pub crate_content: CrateContent,
    pub mod_for_objc_origin: HashMap<ObjCOrigin, ModPath>,
    pub interf_traits: HashMap<String, ObjCTypeRustLoc>,
    pub interf_structs: HashMap<String, ObjCTypeRustLoc>,
    pub protocols: HashMap<String, ObjCTypeRustLoc>,
}

impl RustOverview {
    fn parse(crate_content: CrateContent) -> Result<Self> {
        let mut mod_for_objc_origin = HashMap::new();
        let mut interf_traits = HashMap::new();
        let mut interf_structs = HashMap::new();
        let mut protocols = HashMap::new();

        for mod_content in &crate_content.content {
            let mut visit = FileVisit {
                base_dir: &crate_content.base_dir,
                file_rel_path: &mod_content.file_rel_path,
                mod_path: &mod_content.mod_path,
                err: None,
                mod_for_objc_origin: &mut mod_for_objc_origin,
                interf_traits: &mut interf_traits,
                interf_structs: &mut interf_structs,
                protocols: &mut protocols,
            };
            syn::visit::visit_file(&mut visit, &mod_content.file);
            if let Some(err) = visit.err {
                return Err(err);
            }
        }

        Ok(Self {
            crate_content,
            mod_for_objc_origin,
            interf_traits,
            interf_structs,
            protocols,
        })
    }
}

pub struct Overview {
    pub rust: RustOverview,
    pub objc_index: objc_index::TypeIndex,
}

fn parse_objc_needed(overview: &RustOverview) -> Result<objc_index::TypeIndex> {
    use std::fmt::Write;

    let mut objc_code = String::new();
    for (origin, _) in &overview.mod_for_objc_origin {
        match origin {
            ObjCOrigin::Core => writeln!(&mut objc_code, "#import <objc/NSObject.h>").unwrap(),
            ObjCOrigin::Framework(name) => {
                writeln!(&mut objc_code, "#import <{name}/{name}.h>", name = name).unwrap()
            }
        }
    }

    // TODO: Should use multiple targets (should probably be configurable)
    assert!(cfg!(all(target_os = "macos", target_arch = "x86_64")));
    let target = xcode::Target::MacOsX86_64;
    let ast = objc_ast::ast_from_str(target, &objc_code)?;
    let index = objc_index::TypeIndex::new(&ast);

    Ok(index)
}

fn make_rust_parse_error(overview: &Overview, loc: &ObjCTypeRustLoc, message: String) -> Error {
    Error::loc_err(
        overview
            .rust
            .crate_content
            .base_dir
            .join(&loc.file_rel_path),
        loc.span,
        message,
    )
}

fn check_origin(
    overview: &Overview,
    kind: &str,
    objc_name: &str,
    loc: &ObjCTypeRustLoc,
    objc_origin: &Option<objc_ast::Origin>,
) -> Result<()> {
    let make_error = |message: String| make_rust_parse_error(overview, loc, message);

    let origin = match objc_origin {
        None | Some(objc_ast::Origin::System) => return Ok(()),
        Some(objc_ast::Origin::ObjCCore) => ObjCOrigin::Core,
        Some(objc_ast::Origin::Framework(name)) => ObjCOrigin::Framework(name.clone()),
        Some(objc_ast::Origin::Library(_)) => unimplemented!(),
    };
    let mod_path_expected = match overview.rust.mod_for_objc_origin.get(&origin) {
        Some(mod_path) => mod_path,
        None => {
            let origin_text = match origin {
                ObjCOrigin::Core => "by the Objective-C runtime".to_owned(),
                ObjCOrigin::Framework(name) => format!("in the Objective-C framework {}", name),
            };
            return Err(make_error(format!(
                "{} {} is defined {}, but could not find the module where it should go",
                kind, objc_name, origin_text
            )));
        }
    };
    if mod_path_expected != &loc.mod_path {
        let origin_text = match origin {
            ObjCOrigin::Core => "by the Objective-C runtime".to_owned(),
            ObjCOrigin::Framework(name) => format!("in the Objective-C framework {}", name),
        };
        return Err(make_error(format!(
            "expected {}::{} to be declared in module {}, as the {} {} is declared {}",
            loc.mod_path, loc.rust_name, mod_path_expected, kind, objc_name, origin_text
        )));
    }

    Ok(())
}

fn check_validity(overview: &Overview) -> Result<()> {
    for (objc_name, loc) in &overview.rust.protocols {
        let make_error = |message: String| make_rust_parse_error(overview, loc, message);

        let def = match overview.objc_index.protocols.get(objc_name) {
            Some(def) => def,
            None => {
                return Err(make_error(format!(
                    "could not find @protocol {} in Objective-C",
                    objc_name
                )))
            }
        };
        check_origin(overview, "@protocol", &objc_name, &loc, &def.origin)?;
    }

    for (objc_name, loc) in &overview.rust.interf_traits {
        let make_error = |message: String| make_rust_parse_error(overview, loc, message);

        let def = match overview.objc_index.interfaces.get(objc_name) {
            Some(def) => def,
            None => {
                return Err(make_error(format!(
                    "could not find @interface {} in Objective-C",
                    objc_name
                )))
            }
        };
        check_origin(overview, "@interface", &objc_name, &loc, &def.origin)?;
    }

    for (objc_name, loc) in &overview.rust.interf_structs {
        let make_error = |message: String| make_rust_parse_error(overview, loc, message);

        let def = match overview.objc_index.interfaces.get(objc_name) {
            Some(def) => def,
            None => {
                return Err(make_error(format!(
                    "could not find @interface {} in Objective-C",
                    objc_name
                )))
            }
        };
        check_origin(overview, "@interface", &objc_name, &loc, &def.origin)?;
    }

    Ok(())
}

pub fn read_project(main_file: &Path) -> Result<Overview> {
    let crate_content = crate::crate_read::CrateContent::read_crate(main_file)?;
    let rust = RustOverview::parse(crate_content)?;
    let objc_index = parse_objc_needed(&rust)?;
    let overview = Overview { rust, objc_index };
    check_validity(&overview)?;
    Ok(overview)
}
