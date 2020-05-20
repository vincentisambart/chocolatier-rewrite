#![warn(rust_2018_idioms)]

use std::path::{Path, PathBuf};
use thiserror::Error;

mod read;

#[derive(Error, Debug)]
pub enum RunError {
    #[error("could not find file {}", .0.display())]
    FileNotExisting(PathBuf),
    #[error("{0}")]
    ReadError(read::ReadError),
    #[error("expected that {} would be a directory", .0.display())]
    ExpectingDir(PathBuf),
}

impl From<read::ReadError> for RunError {
    fn from(err: read::ReadError) -> Self {
        RunError::ReadError(err)
    }
}

fn run() -> Result<(), RunError> {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() != 3 {
        eprintln!("syntax: {} source destination", args[0].to_string_lossy());
        std::process::exit(1);
    }

    let mut src_path = Path::new(&args[1]);
    let dst_path = Path::new(&args[2]);

    if !src_path.exists() {
        return Err(RunError::FileNotExisting(src_path.to_owned()));
    }
    let extended_path;
    if src_path.is_dir() {
        extended_path = src_path.join("lib.rs");
        if !extended_path.exists() {
            return Err(RunError::FileNotExisting(extended_path));
        }
        src_path = &extended_path;
    }

    if !dst_path.exists() {
        std::fs::create_dir_all(dst_path).expect("could not create destination directory");
    } else if !dst_path.is_dir() {
        return Err(RunError::ExpectingDir(dst_path.to_owned()));
    }

    read::read_project(&src_path)?;

    Ok(())
}

fn main() {
    match run() {
        Ok(()) => {}
        Err(err) => {
            eprintln!("error: {}", err);
            std::process::exit(1);
        }
    }
}
