use crate::common::{Error, ModPath, Result};
use std::path::{Path, PathBuf};
use syn::visit::Visit;

pub struct ModContent {
    pub mod_path: ModPath,
    pub file_rel_path: PathBuf,
    pub file: syn::File,
}

impl std::fmt::Debug for ModContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModContent")
            .field("mod_path", &self.mod_path)
            .field("file_rel_path", &self.file_rel_path)
            // The file field is so displaying it doesn't bring anything.
            .field("file", &format_args!("(...)"))
            .finish()
    }
}

fn read_whole_file(path: &Path) -> std::io::Result<String> {
    use std::io::Read;

    let mut file = std::fs::File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok(content)
}

struct FileVisit<'a> {
    mod_path: ModPath,
    file_rel_path: &'a Path,
    err: Option<Error>,
    crate_content: &'a mut CrateContent,
}

#[derive(Debug)]
pub struct CrateContent {
    /// Keep content in a Vec to keep the read order.
    pub content: Vec<ModContent>,
    pub base_dir: PathBuf,
}

impl CrateContent {
    fn read_file(&mut self, file_rel_path: &'_ Path, mod_path: ModPath) -> Result<()> {
        assert!(file_rel_path.is_relative());
        let full_path = self.base_dir.join(file_rel_path);
        if !full_path.exists() {
            return Err(Error::file_err(
                full_path,
                format!(
                    "should exist and contain the content for module {}",
                    mod_path
                ),
            ));
        }
        let src = read_whole_file(&full_path).map_err(|err| Error::io_err(err, &full_path))?;
        let parsed_file = syn::parse_file(&src)
            .map_err(|err| Error::syn_err_rel(err, &self.base_dir, file_rel_path))?;
        self.content.push(ModContent {
            mod_path: mod_path.clone(),
            file_rel_path: file_rel_path.to_owned(),
            file: parsed_file.clone(),
        });

        let mut visit = FileVisit {
            mod_path,
            file_rel_path,
            err: None,
            crate_content: self,
        };
        visit.visit_file(&parsed_file);
        match visit.err {
            Some(err) => Err(err),
            None => Ok(()),
        }
    }

    pub fn read_crate(path: &Path) -> Result<Self> {
        if !path.exists() {
            return Err(Error::file_err(path, "not found"));
        }
        let extended_path;
        let parent_dir;
        let (base_dir, file_path) = if path.is_dir() {
            extended_path = path.join("lib.rs");
            if !extended_path.exists() {
                return Err(Error::file_err(
                    extended_path,
                    "expected to exist and contain the root of the crate",
                ));
            }
            (path.to_owned(), extended_path.as_path())
        } else {
            match path.file_name() {
                Some(file_name) if file_name == "lib.rs" => {}
                _ => {
                    return Err(Error::file_err(
                        path,
                        "should be the root file (lib.rs) of the crate",
                    ))
                }
            }
            parent_dir = path.parent().unwrap().to_owned();
            (parent_dir, path)
        };

        let file_rel_path = Path::new(file_path.file_name().unwrap());
        let mut crate_content = Self {
            content: Vec::new(),
            base_dir,
        };
        crate_content.read_file(file_rel_path, ModPath::new())?;
        Ok(crate_content)
    }
}

/// Tries to find the file that `mod <mod_ident>;` references to in `base_dir`.
fn resolve_mod_file_path(base_dir: &Path, mod_path: &ModPath) -> PathBuf {
    assert!(!mod_path.0.is_empty());
    let mod_dir_path = mod_path.0.join("/");
    let mod_dir_path = Path::new(&mod_dir_path);
    if base_dir.join(&mod_dir_path).is_dir() {
        mod_dir_path.join("mod.rs")
    } else {
        mod_dir_path.with_extension("rs")
    }
}

impl Visit<'_> for FileVisit<'_> {
    fn visit_item_mod(&mut self, item_mod: &'_ syn::ItemMod) {
        if self.err.is_some() {
            return;
        }

        let ident = &item_mod.ident;
        let new_mod_path = self.mod_path.child(ident.to_string());

        if item_mod.content.is_some() {
            let mut child = FileVisit {
                mod_path: new_mod_path,
                file_rel_path: self.file_rel_path,
                err: None,
                crate_content: self.crate_content,
            };
            syn::visit::visit_item_mod(&mut child, item_mod);
            self.err = child.err;
        } else {
            let mod_file_path = resolve_mod_file_path(&self.crate_content.base_dir, &new_mod_path);
            self.err = self
                .crate_content
                .read_file(&mod_file_path, new_mod_path)
                .err();
        }
    }
}
