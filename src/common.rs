use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct ErrorLoc {
    pub path: PathBuf,
    pub span: Option<proc_macro2::Span>,
}

impl std::fmt::Display for ErrorLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.span {
            Some(span) => {
                let start = span.start();
                write!(f, "{}:{}:{}", self.path.display(), start.line, start.column,)
            }
            None => write!(f, "{}", self.path.display()),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub loc: Option<ErrorLoc>,
    pub message: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.loc {
            Some(loc) => write!(f, "{}: {}", loc, self.message),
            None => f.write_str(&self.message),
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    pub fn syn_err<P>(err: syn::Error, path: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            loc: Some(ErrorLoc {
                path: path.into(),
                span: Some(err.span()),
            }),
            message: err.to_string(),
        }
    }

    pub fn syn_err_rel(err: syn::Error, base_dir: &Path, file_rel_path: &Path) -> Self {
        Self::syn_err(err, base_dir.join(file_rel_path))
    }

    pub fn io_err<P>(err: std::io::Error, path: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            loc: Some(ErrorLoc {
                path: path.into(),
                span: None,
            }),
            message: err.to_string(),
        }
    }

    pub fn expected_dir<P>(path: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            loc: Some(ErrorLoc {
                path: path.into(),
                span: None,
            }),
            message: "expected to be a directory".to_owned(),
        }
    }

    pub fn file_err<IntoPathBuf, IntoString>(path: IntoPathBuf, message: IntoString) -> Self
    where
        IntoPathBuf: Into<PathBuf>,
        IntoString: Into<String>,
    {
        Self {
            loc: Some(ErrorLoc {
                path: path.into(),
                span: None,
            }),
            message: message.into(),
        }
    }

    pub fn loc_err<IntoPathBuf, Spanned, IntoString>(
        path: IntoPathBuf,
        spanned: Spanned,
        message: IntoString,
    ) -> Self
    where
        IntoPathBuf: Into<PathBuf>,
        Spanned: syn::spanned::Spanned,
        IntoString: Into<String>,
    {
        Self {
            loc: Some(ErrorLoc {
                path: path.into(),
                span: Some(spanned.span()),
            }),
            message: message.into(),
        }
    }
}

impl From<chocolatier_objc_parser::ast::ParseError> for Error {
    fn from(err: chocolatier_objc_parser::ast::ParseError) -> Self {
        Self {
            loc: None,
            message: err.to_string(),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ModPath(pub Vec<String>);

impl ModPath {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn child(&self, name: String) -> Self {
        let mut new = self.0.clone();
        new.push(name);
        Self(new)
    }
}

impl std::fmt::Display for ModPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            f.write_str("crate")
        } else {
            write!(f, "crate::{}", self.0.join("::"))
        }
    }
}

impl std::fmt::Debug for ModPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ModPath({})", self)
    }
}
