use chocolatier_objc_parser::{ast as objc_ast, index as objc_index, xcode};
use proc_macro2::Ident;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use syn::visit::Visit;
use thiserror::Error;

#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct ModPath(Vec<String>);

impl ModPath {
    fn child(&self, name: String) -> Self {
        let mut new = self.0.clone();
        new.push(name);
        ModPath(new)
    }
}

impl std::fmt::Display for ModPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            f.write_str("$crate")
        } else {
            f.write_str(&self.0.join("::"))
        }
    }
}

impl std::fmt::Debug for ModPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ModPath({})", self)
    }
}

fn format_parse_err(file_path: &Path, err: &syn::Error) -> String {
    let start = err.span().start();
    format!(
        "parsing error at {}:{}:{}: {}",
        file_path.display(),
        start.line,
        start.column,
        err
    )
}

#[derive(Error, Debug)]
pub enum ReadError {
    #[error("error reading file {}: {}", .0.display(), .1)]
    IoError(PathBuf, std::io::Error),
    #[error("{}", format_parse_err(.0, .1))]
    RustParseError(PathBuf, syn::Error),
    #[error("{}", .0)]
    ObjCParseError(objc_ast::ParseError),
}

impl From<objc_ast::ParseError> for ReadError {
    fn from(err: objc_ast::ParseError) -> Self {
        ReadError::ObjCParseError(err)
    }
}

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
    fn to_objc_repr(&self) -> ObjCRepresented {
        match self {
            ModObjCAttr::ObjCCore(_) => ObjCRepresented::Core,
            ModObjCAttr::ObjCFramework { lit_str, .. } => {
                ObjCRepresented::Framework(lit_str.value())
            }
        }
    }
}

static MOD_ATTR_NAMES: &[&str] = &["objc_core", "objc_framework"];
static TRAIT_ATTR_NAMES: &[&str] = &["objc_interface", "objc_protocol"];
static STRUCT_ATTR_NAMES: &[&str] = &["objc_interface"];

fn read_file_content(path: &Path) -> std::io::Result<String> {
    use std::io::Read;

    let mut file = std::fs::File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok(content)
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ObjCRepresented {
    Core,
    Framework(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjCTypeRustLoc {
    pub rust_name: String,
    pub mod_path: ModPath,
}

pub struct ModContent {
    pub rel_path: PathBuf,
    pub file: syn::File,
}

impl std::fmt::Debug for ModContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModContent")
            .field("rel_path", &self.rel_path)
            // The file field is so displaying it doesn't bring anything.
            .field("file", &format_args!("(...)"))
            .finish()
    }
}

#[derive(Debug)]
pub struct RustOverview {
    pub base_dir: PathBuf,
    pub mods_content: HashMap<ModPath, ModContent>,
    pub objc_repr: HashMap<ObjCRepresented, ModPath>,
    pub interf_traits: HashMap<String, ObjCTypeRustLoc>,
    pub interf_structs: HashMap<String, ObjCTypeRustLoc>,
    pub protocols: HashMap<String, ObjCTypeRustLoc>,
}

impl RustOverview {
    fn new(base_dir: PathBuf) -> Self {
        Self {
            base_dir,
            mods_content: HashMap::new(),
            objc_repr: HashMap::new(),
            interf_traits: HashMap::new(),
            interf_structs: HashMap::new(),
            protocols: HashMap::new(),
        }
    }
}

struct FileVisit<'a> {
    mod_path: ModPath,
    rel_path: &'a Path,
    errs: Vec<ReadError>,
    overview: &'a mut RustOverview,
}

impl<'a> FileVisit<'a> {
    fn parse_file(
        overview: &'_ mut RustOverview,
        rel_path: &'_ Path,
        mod_path: ModPath,
    ) -> Result<(), ReadError> {
        assert!(rel_path.is_relative());
        let full_path = overview.base_dir.join(rel_path);
        let src = match read_file_content(&full_path) {
            Ok(src) => src,
            Err(err) => {
                return Err(ReadError::IoError(full_path.to_owned(), err));
            }
        };
        let parsed_file = match syn::parse_file(&src) {
            Ok(parsed_file) => parsed_file,
            Err(err) => {
                return Err(ReadError::RustParseError(full_path.to_owned(), err));
            }
        };
        overview.mods_content.insert(
            mod_path.clone(),
            ModContent {
                rel_path: rel_path.to_owned(),
                file: parsed_file.clone(),
            },
        );

        let mut visit = FileVisit {
            mod_path,
            rel_path,
            errs: Vec::new(),
            overview,
        };
        visit.visit_file(&parsed_file);
        match visit.errs.into_iter().next() {
            Some(err) => Err(err),
            None => Ok(()),
        }
    }

    fn parse_main_file(path: &'a Path) -> Result<RustOverview, ReadError> {
        let base_dir = path.parent().unwrap().to_owned();
        let file_rel_path = Path::new(path.file_name().unwrap());
        let mut overview = RustOverview::new(base_dir);
        Self::parse_file(&mut overview, file_rel_path, Default::default())?;
        Ok(overview)
    }

    fn parse_error(&self, err: syn::Error) -> ReadError {
        let full_path = self.overview.base_dir.join(self.rel_path);
        ReadError::RustParseError(full_path, err)
    }

    fn do_visit_item_mod(&mut self, item_mod: &'_ syn::ItemMod) -> Result<(), ReadError> {
        let objc_attrs: Vec<_> = item_mod
            .attrs
            .iter()
            .filter(|attr| MOD_ATTR_NAMES.iter().any(|name| attr.path.is_ident(name)))
            .collect();

        if objc_attrs.len() > 1 {
            return Err(self.parse_error(syn::Error::new_spanned(
                objc_attrs[1],
                "there should only be one objc_* attribute on a module",
            )));
        }

        let ident = &item_mod.ident;
        let new_mod_path = self.mod_path.child(ident.to_string());

        if let Some(attr) = objc_attrs.first() {
            let meta = attr.parse_meta().map_err(|err| self.parse_error(err))?;
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
                    return Err(self.parse_error(syn::Error::new_spanned(attr, "invalid attribute")))
                }
            };

            let repr = res_attr.to_objc_repr();
            if let Some(dup) = self.overview.objc_repr.remove(&repr) {
                return Err(self.parse_error(syn::Error::new_spanned(
                    attr,
                    format!("{} already stands for the same Objective-C source", dup),
                )));
            }
            self.overview.objc_repr.insert(repr, new_mod_path.clone());
        }

        if item_mod.content.is_some() {
            let mut child = FileVisit {
                mod_path: new_mod_path,
                rel_path: self.rel_path,
                errs: Vec::new(),
                overview: self.overview,
            };
            syn::visit::visit_item_mod(&mut child, item_mod);
            match child.errs.into_iter().next() {
                Some(err) => Err(err),
                None => Ok(()),
            }
        } else {
            let mod_file_rel_path =
                match resolve_mod_file_path(&self.overview.base_dir, self.rel_path, &ident) {
                    Some(path) => path,
                    None => {
                        return Err(self.parse_error(syn::Error::new_spanned(
                            item_mod,
                            format!("could not find file for {}", ident),
                        )))
                    }
                };

            FileVisit::parse_file(self.overview, &mod_file_rel_path, new_mod_path)
        }
    }

    fn try_inserting(
        ident: &Ident,
        mod_path: &ModPath,
        suffix: &str,
        mapping: &mut HashMap<String, ObjCTypeRustLoc>,
    ) -> Result<(), syn::Error> {
        let rust_name = ident.to_string();
        if !rust_name.ends_with(suffix) {
            return Err(syn::Error::new_spanned(
                ident.clone(),
                "objc_protocol traits must have their name ending with Protocol",
            ));
        }
        let objc_name = rust_name[..rust_name.len() - suffix.len()].to_owned();
        let loc = ObjCTypeRustLoc {
            rust_name,
            mod_path: mod_path.clone(),
        };
        if let Some(existing_loc) = mapping.get(&objc_name) {
            if &loc != existing_loc {
                return Err(syn::Error::new_spanned(
                    ident.clone(),
                    format!(
                        "{} is already mapped to {} in {}",
                        objc_name, existing_loc.rust_name, existing_loc.mod_path
                    ),
                ));
            }
        } else {
            mapping.insert(objc_name, loc);
        }
        Ok(())
    }

    fn do_visit_item_trait(&mut self, item_trait: &'_ syn::ItemTrait) -> Result<(), syn::Error> {
        let objc_attrs: Vec<_> = item_trait
            .attrs
            .iter()
            .filter(|attr| TRAIT_ATTR_NAMES.iter().any(|name| attr.path.is_ident(name)))
            .collect();

        if objc_attrs.len() > 1 {
            return Err(syn::Error::new_spanned(
                objc_attrs[1],
                "there should only be one objc_* attribute on a trait",
            ));
        }

        if let Some(attr) = objc_attrs.first() {
            if attr.path.is_ident("objc_interface") {
                Self::try_inserting(
                    &item_trait.ident,
                    &self.mod_path,
                    "Interface",
                    &mut self.overview.interf_traits,
                )?;
            } else if attr.path.is_ident("objc_protocol") {
                Self::try_inserting(
                    &item_trait.ident,
                    &self.mod_path,
                    "Protocol",
                    &mut self.overview.protocols,
                )?;
            } else {
                unreachable!()
            };
        }

        syn::visit::visit_item_trait(self, item_trait);
        Ok(())
    }

    fn do_visit_item_struct(&mut self, item_struct: &'_ syn::ItemStruct) -> Result<(), syn::Error> {
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
            return Err(syn::Error::new_spanned(
                objc_attrs[1],
                "there should only be one objc_* attribute on a trait",
            ));
        }

        if let Some(attr) = objc_attrs.first() {
            if attr.path.is_ident("objc_interface") {
                Self::try_inserting(
                    &item_struct.ident,
                    &self.mod_path,
                    "",
                    &mut self.overview.interf_structs,
                )?;
            } else {
                unreachable!()
            };
        }

        syn::visit::visit_item_struct(self, item_struct);
        Ok(())
    }
}

/// Tries to find the file that `mod [mod_ident];` references to in referencing file `ref_file` in `base_dir`.
fn resolve_mod_file_path(base_dir: &Path, ref_file: &Path, mod_ident: &Ident) -> Option<PathBuf> {
    let owned_dir_path;
    let dir_path = match ref_file.file_name() {
        Some(name) if name == "lib.rs" || name == "main.rs" => ref_file.parent().unwrap(),
        _ => {
            owned_dir_path = ref_file.with_extension("");
            &owned_dir_path
        }
    };

    let mod_dir_path = dir_path.join(mod_ident.to_string());
    let mod_file_path = if base_dir.join(&mod_dir_path).is_dir() {
        mod_dir_path.join("mod.rs")
    } else {
        mod_dir_path.with_extension("rs")
    };
    if base_dir.join(&mod_file_path).is_file() {
        Some(mod_file_path)
    } else {
        None
    }
}

impl Visit<'_> for FileVisit<'_> {
    fn visit_item_mod(&mut self, item_mod: &'_ syn::ItemMod) {
        match self.do_visit_item_mod(item_mod) {
            Err(err) => self.errs.push(err),
            Ok(()) => {}
        }
    }

    fn visit_item_trait(&mut self, item_trait: &'_ syn::ItemTrait) {
        match self.do_visit_item_trait(item_trait) {
            Err(err) => self.errs.push(self.parse_error(err)),
            Ok(()) => {}
        }
    }

    fn visit_item_struct(&mut self, item_struct: &'_ syn::ItemStruct) {
        match self.do_visit_item_struct(item_struct) {
            Err(err) => self.errs.push(self.parse_error(err)),
            Ok(()) => {}
        }
    }
}

pub struct Overview {
    pub rust: RustOverview,
    pub objc_index: objc_index::TypeIndex,
}

fn parse_objc_needed(overview: &RustOverview) -> Result<objc_index::TypeIndex, ReadError> {
    use std::fmt::Write;

    let mut objc_code = String::new();
    for (repr, _) in &overview.objc_repr {
        match repr {
            ObjCRepresented::Core => writeln!(&mut objc_code, "#import <objc/NSObject.h>").unwrap(),
            ObjCRepresented::Framework(name) => {
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

pub fn read_project(main_file: &Path) -> Result<Overview, ReadError> {
    let rust = FileVisit::parse_main_file(main_file)?;
    let objc_index = parse_objc_needed(&rust)?;
    Ok(Overview { rust, objc_index })
}
