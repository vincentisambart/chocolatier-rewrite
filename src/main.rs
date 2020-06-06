#![warn(rust_2018_idioms)]
#![allow(clippy::assertions_on_constants)]
#![allow(clippy::match_bool)] // already downgraded to pedantic on clippy master

mod common;
mod gen;
mod parse;
mod read;
mod skel;
mod write;

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

    if dst_path.exists() && !dst_path.is_dir() {
        return Err(Error::expected_dir(dst_path));
    }

    let skeleton = skel::read_skeleton(&src_path)?;
    let generated = gen::generate(&skeleton)?;
    write::write_crate(dst_path, &generated)?;

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
