#![warn(rust_2018_idioms)]
#![allow(clippy::assertions_on_constants)]

mod common;
mod crate_read;
mod read;

use common::{Error, Result};
use std::path::Path;

fn run() -> Result<()> {
    let args: Vec<_> = std::env::args_os().collect();
    if args.len() != 3 {
        eprintln!("syntax: {} source destination", args[0].to_string_lossy());
        std::process::exit(1);
    }

    let src_path = Path::new(&args[1]);
    let dst_path = Path::new(&args[2]);

    if !dst_path.exists() {
        std::fs::create_dir_all(dst_path).expect("could not create destination directory");
    } else if !dst_path.is_dir() {
        return Err(Error::expected_dir(dst_path));
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
