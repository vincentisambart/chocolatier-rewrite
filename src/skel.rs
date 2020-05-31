use crate::common::{Error, ModPath, Result};
use crate::read::CrateContent;
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
static STRUCT_ATTR_NAMES: &[&str] = &["objc_interface", "objc_enum"];

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ObjCOrigin {
    Core,
    Framework(String),
}

#[derive(Debug)]
pub struct ObjCTypeRustLoc {
    pub path: RustEntityPath,
    pub span: proc_macro2::Span,
    pub file_rel_path: PathBuf,
}

impl ObjCTypeRustLoc {
    fn err_in_dir(&self, base_dir: &Path, message: String) -> Error {
        Error::at_loc(base_dir.join(&self.file_rel_path), self.span, message)
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

#[derive(Debug, Clone)]
pub enum ObjCEntity {
    Protocol(String),
    Interface(String),
    Enum(String),
}

struct FileVisit<'a> {
    base_dir: &'a Path,
    file_rel_path: &'a Path,
    mod_path: &'a ModPath,
    err: Option<Error>,
    index: &'a mut Index,
}

impl<'a> FileVisit<'a> {
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

    fn do_visit_item_mod(&mut self, item_mod: &'_ syn::ItemMod) -> Result<()> {
        let objc_attrs: Vec<_> = item_mod
            .attrs
            .iter()
            .filter(|attr| MOD_ATTR_NAMES.iter().any(|name| attr.path.is_ident(name)))
            .collect();

        if objc_attrs.len() > 1 {
            return Err(self.err_at_loc(
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
                    return Err(self.err_at_loc(attr, "invalid attribute"));
                }
            };

            let origin = res_attr.to_objc_origin();
            if let Some(dup) = self.index.mod_per_objc_origin.remove(&origin) {
                return Err(self.err_at_loc(
                    attr,
                    format!("{} already stands for the same Objective-C source", dup),
                ));
            }
            self.index
                .mod_per_objc_origin
                .insert(origin, new_mod_path.clone());
        }

        if item_mod.content.is_some() {
            let mut child = FileVisit {
                mod_path: &new_mod_path,
                err: None,
                index: self.index,
                ..*self
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
            return Err(self.err_at_loc(
                objc_attrs[1],
                "there should only be one objc_* attribute on a trait",
            ));
        }

        if let Some(attr) = objc_attrs.first() {
            let rust_name = item_trait.ident.to_string();
            let path = RustEntityPath {
                mod_path: self.mod_path.clone(),
                name: rust_name.clone(),
            };
            let loc = ObjCTypeRustLoc {
                path: path.clone(),
                span: item_trait.ident.span(),
                file_rel_path: self.file_rel_path.to_owned(),
            };

            if attr.path.is_ident("objc_interface") {
                let objc_name = match strip_suffix(&rust_name, "Interface") {
                    Some(objc_name) => objc_name,
                    None => {
                        return Err(self.err_at_loc(
                            attr,
                            format!(
                                "\"{}\" is tagged #[objc_interface] so its name should end with \"Interface\"",
                                rust_name
                            ),
                        ))
                    }
                };

                if let Some(existing_loc) = self.index.interf_traits.get(objc_name) {
                    return Err(self.err_at_loc(
                        &item_trait.ident,
                        format!("{} is already mapped to {}", objc_name, existing_loc.path),
                    ));
                } else {
                    self.index.interf_traits.insert(objc_name.to_owned(), loc);
                    self.index
                        .entity_objc_mapping
                        .insert(path, ObjCEntity::Interface(objc_name.to_owned()));
                }
            } else if attr.path.is_ident("objc_protocol") {
                let objc_name = match strip_suffix(&rust_name, "Protocol") {
                    Some(without_suffix) => without_suffix,
                    None => {
                        return Err(self.err_at_loc(
                            attr,
                            format!(
                                "\"{}\" is tagged #[objc_protocol] so its name should end with \"Protocol\"",
                                rust_name
                            ),
                        ))
                    }
                };

                if let Some(existing_loc) = self.index.protocols.get(objc_name) {
                    return Err(self.err_at_loc(
                        &item_trait.ident,
                        format!("{} is already mapped to {}", objc_name, existing_loc.path),
                    ));
                } else {
                    self.index.protocols.insert(objc_name.to_owned(), loc);
                    self.index
                        .entity_objc_mapping
                        .insert(path, ObjCEntity::Protocol(objc_name.to_owned()));
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
            return Err(self.err_at_loc(
                objc_attrs[1],
                "there should only be one objc_* attribute on a trait",
            ));
        }

        if let Some(attr) = objc_attrs.first() {
            // For Rust structs, the Rust name is the same as the ObjC one.
            let name = item_struct.ident.to_string();

            if !matches!(&item_struct.fields, syn::Fields::Unit) {
                return Err(self.err_at_loc(
                    &item_struct.ident,
                    format!(
                        "{} and all other Objective-C mapped structs should not declare any field",
                        name
                    ),
                ));
            }

            let path = RustEntityPath {
                mod_path: self.mod_path.clone(),
                name: name.clone(),
            };
            let loc = ObjCTypeRustLoc {
                path: path.clone(),
                span: item_struct.ident.span(),
                file_rel_path: self.file_rel_path.to_owned(),
            };

            if attr.path.is_ident("objc_interface") {
                if let Some(existing_loc) = self.index.interf_structs.get(&name) {
                    return Err(self.err_at_loc(
                        &item_struct.ident,
                        format!("{} is already mapped to {}", name, existing_loc.path),
                    ));
                } else {
                    self.index.interf_structs.insert(name.clone(), loc);
                    self.index
                        .entity_objc_mapping
                        .insert(path, ObjCEntity::Interface(name));
                }
            } else if attr.path.is_ident("objc_enum") {
                if let Some(existing_loc) = self.index.enum_structs.get(&name) {
                    return Err(self.err_at_loc(
                        &item_struct.ident,
                        format!("{} is already mapped to {}", name, existing_loc.path),
                    ));
                } else {
                    self.index.enum_structs.insert(name.clone(), loc);
                    self.index
                        .entity_objc_mapping
                        .insert(path, ObjCEntity::Enum(name));
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct RustEntityPath {
    pub mod_path: ModPath,
    pub name: String,
}

impl RustEntityPath {
    pub fn from_type_path(current_mod_path: &ModPath, path: &syn::TypePath) -> Option<Self> {
        // Only care about simple paths.
        if path.qself.is_some() {
            return None;
        }

        let mut vec: Vec<_> = path
            .path
            .segments
            .iter()
            .map(|seg| seg.ident.to_string())
            .collect();

        let name = vec.pop().expect("the path should not be empty");

        assert!(vec.is_empty(), "relative paths are not supported yet");

        vec.splice(0..0, current_mod_path.0.clone());

        Some(Self {
            mod_path: ModPath(vec),
            name,
        })
    }
}

impl std::fmt::Display for RustEntityPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.mod_path, self.name)
    }
}

#[derive(Debug, Default)]
pub struct Index {
    pub mod_per_objc_origin: HashMap<ObjCOrigin, ModPath>,
    pub entity_objc_mapping: HashMap<RustEntityPath, ObjCEntity>,
    pub interf_traits: HashMap<String, ObjCTypeRustLoc>,
    pub interf_structs: HashMap<String, ObjCTypeRustLoc>,
    pub enum_structs: HashMap<String, ObjCTypeRustLoc>,
    pub protocols: HashMap<String, ObjCTypeRustLoc>,
}

#[derive(Debug)]
pub struct RustOverview {
    pub crate_content: CrateContent,
    pub index: Index,
}

impl RustOverview {
    fn parse(crate_content: CrateContent) -> Result<Self> {
        let mut index: Index = Default::default();

        for mod_content in &crate_content.content {
            let mut visit = FileVisit {
                base_dir: &crate_content.base_dir,
                file_rel_path: &mod_content.file_rel_path,
                mod_path: &mod_content.mod_path,
                err: None,
                index: &mut index,
            };
            syn::visit::visit_file(&mut visit, &mod_content.file);
            if let Some(err) = visit.err {
                return Err(err);
            }
        }

        Ok(Self {
            crate_content,
            index,
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
    for origin in overview.index.mod_per_objc_origin.keys() {
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

fn check_origin(
    overview: &Overview,
    kind: &str,
    objc_name: &str,
    loc: &ObjCTypeRustLoc,
    objc_origin: &Option<objc_ast::Origin>,
) -> Result<()> {
    let base_dir = &overview.rust.crate_content.base_dir;

    let origin = match objc_origin {
        None | Some(objc_ast::Origin::System) => return Ok(()),
        Some(objc_ast::Origin::ObjCCore) => ObjCOrigin::Core,
        Some(objc_ast::Origin::Framework(name)) => ObjCOrigin::Framework(name.clone()),
        Some(objc_ast::Origin::Library(_)) => unimplemented!(),
    };
    let mod_path_expected = match overview.rust.index.mod_per_objc_origin.get(&origin) {
        Some(mod_path) => mod_path,
        None => {
            let origin_text = match origin {
                ObjCOrigin::Core => "by the Objective-C runtime".to_owned(),
                ObjCOrigin::Framework(name) => format!("in the Objective-C framework {}", name),
            };
            return Err(loc.err_in_dir(
                base_dir,
                format!(
                    "{} {} is defined {}, but could not find the module where it should go",
                    kind, objc_name, origin_text
                ),
            ));
        }
    };
    if mod_path_expected != &loc.path.mod_path {
        let origin_text = match origin {
            ObjCOrigin::Core => "by the Objective-C runtime".to_owned(),
            ObjCOrigin::Framework(name) => format!("in the Objective-C framework {}", name),
        };
        return Err(loc.err_in_dir(
            base_dir,
            format!(
                "expected {} to be declared in module {}, as the {} {} is declared {}",
                loc.path, mod_path_expected, kind, objc_name, origin_text
            ),
        ));
    }

    Ok(())
}

fn check_validity(overview: &Overview) -> Result<()> {
    let base_dir = &overview.rust.crate_content.base_dir;

    for (objc_name, loc) in &overview.rust.index.protocols {
        let def = match overview.objc_index.protocols.get(objc_name) {
            Some(def) => def,
            None => {
                return Err(loc.err_in_dir(
                    base_dir,
                    format!("could not find @protocol {} in Objective-C", objc_name),
                ))
            }
        };
        check_origin(overview, "@protocol", &objc_name, &loc, &def.origin)?;
    }

    for (objc_name, loc) in &overview.rust.index.interf_traits {
        let def = match overview.objc_index.interfaces.get(objc_name) {
            Some(def) => def,
            None => {
                return Err(loc.err_in_dir(
                    base_dir,
                    format!("could not find @interface {} in Objective-C", objc_name),
                ))
            }
        };
        check_origin(overview, "@interface", &objc_name, &loc, &def.origin)?;
    }

    for (objc_name, loc) in &overview.rust.index.interf_structs {
        let def = match overview.objc_index.interfaces.get(objc_name) {
            Some(def) => def,
            None => {
                return Err(loc.err_in_dir(
                    base_dir,
                    format!("could not find @interface {} in Objective-C", objc_name),
                ))
            }
        };
        check_origin(overview, "@interface", &objc_name, &loc, &def.origin)?;

        if overview.rust.index.interf_traits.get(objc_name).is_none() {
            return Err(loc.err_in_dir(
                base_dir,
                format!(
                    "could not find any `#[objc_interface] trait {}Interface` definition",
                    objc_name
                ),
            ));
        }
    }

    for (objc_name, loc) in &overview.rust.index.enum_structs {
        use objc_ast::{TagId, TagKind, TagRef, Type};

        let objc_origin = match overview.objc_index.resolve_typedef(objc_name) {
            Some(def) => match def.underlying.ty {
                Type::Tag(TagRef {
                    kind: TagKind::Enum,
                    ..
                })
                | Type::Num(_) => {
                    // The resolved typedef might be multi-level deep (NSStringEncoding->NSUInteger->unsigned long),
                    // but we want the origin of the first level.
                    overview
                        .objc_index
                        .typedefs
                        .get(objc_name)
                        .unwrap()
                        .origin
                        .clone()
                }
                _ => {
                    return Err(loc.err_in_dir(
                        base_dir,
                        format!("{} is not an enum in Objective-C - {:?}", objc_name, def),
                    ));
                }
            },
            None => {
                let tag_id = TagId::Named(objc_name.clone());
                match overview.objc_index.enums.get(&tag_id) {
                    Some(def) => def.origin.clone(),
                    None => {
                        return Err(loc.err_in_dir(
                            base_dir,
                            format!("could not find enum {} in Objective-C", objc_name),
                        ))
                    }
                }
            }
        };

        check_origin(overview, "enum", &objc_name, &loc, &objc_origin)?;
    }

    Ok(())
}

pub fn read_skeleton(main_file: &Path) -> Result<Overview> {
    let crate_content = CrateContent::read_crate(main_file)?;
    let rust = RustOverview::parse(crate_content)?;
    let objc_index = parse_objc_needed(&rust)?;
    let overview = Overview { rust, objc_index };
    check_validity(&overview)?;
    Ok(overview)
}
