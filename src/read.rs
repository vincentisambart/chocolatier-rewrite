use proc_macro2::Ident;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use syn::visit::Visit;
use thiserror::Error;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
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
    #[error("could not find module \"{0}\"")]
    ModNotFound(ModPath),
    #[error("error reading file {}: {}", .0.display(), .1)]
    IoError(PathBuf, std::io::Error),
    // TODO: Use ParseError instead of DupMap
    #[error("module \"{0}\" and \"{0}\" map to the same Objective-C origin")]
    DupMap(ModPath, ModPath),
    #[error("{}", format_parse_err(.0, .1))]
    ParseError(PathBuf, syn::Error),
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
    fn mod_kind(&self) -> ModKind {
        match self {
            ModObjCAttr::ObjCCore(_) => ModKind::ObjCCore,
            ModObjCAttr::ObjCFramework { ident, .. } => ModKind::ObjCFramework(ident.to_string()),
        }
    }
}

static MOD_ATTR_NAMES: &[&'static str] = &["objc_core", "objc_framework"];
static TRAIT_ATTR_NAMES: &[&'static str] = &["objc_interface", "objc_protocol"];
static STRUCT_ATTR_NAMES: &[&'static str] = &["objc_interface"];

fn read_file_content(path: &Path) -> std::io::Result<String> {
    use std::io::Read;

    let mut file = std::fs::File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok(content)
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum ModKind {
    ObjCCore,
    ObjCFramework(String),
}

#[derive(Debug, PartialEq, Eq)]
struct ObjCTypeRustLoc {
    rust_name: String,
    mod_path: ModPath,
}

#[derive(Debug, Default)]
struct Mappings {
    mods: HashMap<ModKind, ModPath>,
    interf_traits: HashMap<String, ObjCTypeRustLoc>,
    interf_structs: HashMap<String, ObjCTypeRustLoc>,
    protocols: HashMap<String, ObjCTypeRustLoc>,
}

impl Mappings {
    fn merge(&mut self, other: Mappings) -> Result<(), ReadError> {
        for (kind, mod_path) in other.mods {
            if let Some(dup) = self.mods.remove(&kind) {
                return Err(ReadError::DupMap(dup, mod_path));
            }
            self.mods.insert(kind, mod_path);
        }

        fn merge_locs(
            dst: &mut HashMap<String, ObjCTypeRustLoc>,
            src: HashMap<String, ObjCTypeRustLoc>,
        ) -> Result<(), ReadError> {
            for (name, loc) in src {
                if let Some(dup) = dst.remove(&name) {
                    return Err(ReadError::DupMap(dup.mod_path, loc.mod_path));
                }
                dst.insert(name, loc);
            }
            Ok(())
        }
        merge_locs(&mut self.interf_structs, other.interf_structs)?;
        merge_locs(&mut self.interf_traits, other.interf_traits)?;
        merge_locs(&mut self.protocols, other.protocols)?;
        Ok(())
    }
}

struct FileVisit<'a> {
    mod_path: ModPath,
    file_path: &'a Path,
    errs: Vec<ReadError>,
    mappings: Mappings,
}

impl<'a> FileVisit<'a> {
    fn parse_file(path: &'a Path, mod_path: ModPath) -> Self {
        let mut visit = FileVisit {
            mod_path,
            file_path: path,
            errs: Vec::new(),
            mappings: Default::default(),
        };
        let src = match read_file_content(path) {
            Ok(src) => src,
            Err(err) => {
                visit.errs.push(ReadError::IoError(path.to_owned(), err));
                return visit;
            }
        };
        let parsed_file = match syn::parse_file(&src) {
            Ok(parsed_file) => parsed_file,
            Err(err) => {
                visit.errs.push(ReadError::ParseError(path.to_owned(), err));
                return visit;
            }
        };

        visit.visit_file(&parsed_file);
        visit
    }

    fn parse_main_file(path: &'a Path) -> Result<Mappings, ReadError> {
        let mut visit = Self::parse_file(path, Default::default());
        let first_err = visit.errs.drain(..).next();
        if let Some(first_err) = first_err {
            Err(first_err)
        } else {
            Ok(visit.mappings)
        }
    }

    fn merge<'b>(&mut self, other: FileVisit<'b>) {
        self.errs.extend(other.errs);
        if let Err(err) = self.mappings.merge(other.mappings) {
            self.errs.push(err);
        }
    }

    fn do_visit_item_mod(&mut self, item_mod: &'_ syn::ItemMod) -> Result<(), ReadError> {
        let objc_attrs: Vec<_> = item_mod
            .attrs
            .iter()
            .filter(|attr| MOD_ATTR_NAMES.iter().any(|name| attr.path.is_ident(name)))
            .collect();

        if objc_attrs.len() > 1 {
            return Err(ReadError::ParseError(
                self.file_path.to_owned(),
                syn::Error::new_spanned(
                    objc_attrs[1],
                    "there should only be one objc_* attribute on a module",
                ),
            ));
        }

        let ident = &item_mod.ident;
        let new_mod_path = self.mod_path.child(ident.to_string());

        if let Some(attr) = objc_attrs.first() {
            let meta = attr
                .parse_meta()
                .map_err(|err| ReadError::ParseError(self.file_path.to_owned(), err))?;
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
                    return Err(ReadError::ParseError(
                        self.file_path.to_owned(),
                        syn::Error::new_spanned(attr, "invalid attribute"),
                    ))
                }
            };

            let mod_kind = res_attr.mod_kind();
            if let Some(dup) = self.mappings.mods.remove(&mod_kind) {
                return Err(ReadError::DupMap(dup, new_mod_path));
            }
            self.mappings.mods.insert(mod_kind, new_mod_path.clone());
        }

        let mod_file_path;
        let child = if item_mod.content.is_some() {
            let mut child = FileVisit {
                mod_path: new_mod_path,
                file_path: self.file_path,
                errs: Vec::new(),
                mappings: Default::default(),
            };
            syn::visit::visit_item_mod(&mut child, item_mod);
            child
        } else {
            mod_file_path = match resolve_mod_file_path(self.file_path, &ident) {
                Some(path) => path,
                None => return Err(ReadError::ModNotFound(new_mod_path)),
            };

            FileVisit::parse_file(&mod_file_path, new_mod_path)
        };
        self.merge(child);
        Ok(())
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
                    &mut self.mappings.interf_traits,
                )?;
            } else if attr.path.is_ident("objc_protocol") {
                Self::try_inserting(
                    &item_trait.ident,
                    &self.mod_path,
                    "Protocol",
                    &mut self.mappings.protocols,
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
                    &mut self.mappings.interf_structs,
                )?;
            } else {
                unreachable!()
            };
        }

        syn::visit::visit_item_struct(self, item_struct);
        Ok(())
    }
}

/// Tries to find the file that `mod [mod_ident];` references to in referencing file `ref_file`.
fn resolve_mod_file_path(ref_file: &Path, mod_ident: &Ident) -> Option<PathBuf> {
    let owned_dir_path;
    let dir_path = match ref_file.file_name() {
        Some(name) if name == "lib.rs" || name == "main.rs" => ref_file.parent().unwrap(),
        _ => {
            owned_dir_path = ref_file.with_extension("");
            &owned_dir_path
        }
    };

    let mod_dir_path = dir_path.join(mod_ident.to_string());
    let mod_file_path = if mod_dir_path.is_dir() {
        mod_dir_path.join("mod.rs")
    } else {
        mod_dir_path.with_extension("rs")
    };
    if mod_file_path.is_file() {
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
            Err(err) => self
                .errs
                .push(ReadError::ParseError(self.file_path.to_owned(), err)),
            Ok(()) => {}
        }
    }

    fn visit_item_struct(&mut self, item_struct: &'_ syn::ItemStruct) {
        match self.do_visit_item_struct(item_struct) {
            Err(err) => self
                .errs
                .push(ReadError::ParseError(self.file_path.to_owned(), err)),
            Ok(()) => {}
        }
    }
}

pub fn read_file(path: &Path) -> Result<(), ReadError> {
    let mappings = FileVisit::parse_main_file(path)?;
    dbg!(mappings);
    Ok(())
}
