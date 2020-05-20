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

fn read_file_content(path: &Path) -> std::io::Result<String> {
    use std::io::Read;

    let mut file = std::fs::File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok(content)
}

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

#[derive(Debug)]
pub struct RustOverview {
    pub base_dir: PathBuf,
    pub mods_content: HashMap<ModPath, ModContent>,
    pub mod_for_objc_origin: HashMap<ObjCOrigin, ModPath>,
    pub interf_traits: HashMap<String, ObjCTypeRustLoc>,
    pub interf_structs: HashMap<String, ObjCTypeRustLoc>,
    pub protocols: HashMap<String, ObjCTypeRustLoc>,
}

impl RustOverview {
    fn new(base_dir: PathBuf) -> Self {
        Self {
            base_dir,
            mods_content: HashMap::new(),
            mod_for_objc_origin: HashMap::new(),
            interf_traits: HashMap::new(),
            interf_structs: HashMap::new(),
            protocols: HashMap::new(),
        }
    }
}

struct FileVisit<'a> {
    mod_path: ModPath,
    file_rel_path: &'a Path,
    errs: Vec<ReadError>,
    overview: &'a mut RustOverview,
}

impl<'a> FileVisit<'a> {
    fn parse_file(
        overview: &'_ mut RustOverview,
        file_rel_path: &'_ Path,
        mod_path: ModPath,
    ) -> Result<(), ReadError> {
        assert!(file_rel_path.is_relative());
        let full_path = overview.base_dir.join(file_rel_path);
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
                file_rel_path: file_rel_path.to_owned(),
                file: parsed_file.clone(),
            },
        );

        let mut visit = FileVisit {
            mod_path,
            file_rel_path,
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
        let full_path = self.overview.base_dir.join(self.file_rel_path);
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

            let origin = res_attr.to_objc_origin();
            if let Some(dup) = self.overview.mod_for_objc_origin.remove(&origin) {
                return Err(self.parse_error(syn::Error::new_spanned(
                    attr,
                    format!("{} already stands for the same Objective-C source", dup),
                )));
            }
            self.overview
                .mod_for_objc_origin
                .insert(origin, new_mod_path.clone());
        }

        if item_mod.content.is_some() {
            let mut child = FileVisit {
                mod_path: new_mod_path,
                file_rel_path: self.file_rel_path,
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
                match resolve_mod_file_path(&self.overview.base_dir, self.file_rel_path, &ident) {
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
        file_rel_path: &Path,
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
            span: ident.span(),
            file_rel_path: file_rel_path.to_owned(),
        };
        if let Some(existing_loc) = mapping.get(&objc_name) {
            if loc.same_module_and_name(existing_loc) {
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
                    self.file_rel_path,
                    "Interface",
                    &mut self.overview.interf_traits,
                )?;
            } else if attr.path.is_ident("objc_protocol") {
                Self::try_inserting(
                    &item_trait.ident,
                    &self.mod_path,
                    self.file_rel_path,
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
                    self.file_rel_path,
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

fn make_rust_parse_error(overview: &Overview, loc: &ObjCTypeRustLoc, message: String) -> ReadError {
    let file_path = overview.rust.base_dir.join(&loc.file_rel_path);
    let err = syn::Error::new(loc.span, message);
    ReadError::RustParseError(file_path, err)
}

fn check_origin(
    overview: &Overview,
    kind: &str,
    objc_name: &str,
    loc: &ObjCTypeRustLoc,
    objc_origin: &Option<objc_ast::Origin>,
) -> Result<(), ReadError> {
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

fn check_origins(overview: &Overview) -> Result<(), ReadError> {
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

pub fn read_project(main_file: &Path) -> Result<Overview, ReadError> {
    let rust = FileVisit::parse_main_file(main_file)?;
    let objc_index = parse_objc_needed(&rust)?;
    let overview = Overview { rust, objc_index };
    check_origins(&overview)?;
    Ok(overview)
}
