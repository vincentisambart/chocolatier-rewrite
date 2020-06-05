use crate::common::{Error, Result};
use crate::read::ModContent;
use quote::ToTokens;
use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

pub fn write_crate(dst_path: &Path, content: &[ModContent]) -> Result<()> {
    let file_paths: Vec<_> = content
        .iter()
        .map(|mod_content| dst_path.join(&mod_content.file_rel_path))
        .collect();
    for (mod_content, file_path) in content.iter().zip(&file_paths) {
        let dir_path = file_path.parent().unwrap();
        std::fs::create_dir_all(dir_path).map_err(|err| Error::io_err(err, dir_path))?;
        std::fs::write(&file_path, mod_content.file.to_token_stream().to_string())
            .map_err(|err| Error::io_err(err, file_path))?;
    }

    let mut args: Vec<&OsStr> = vec!["--edition=2018".as_ref()];
    args.extend(file_paths.iter().map(|path| path.as_os_str()));
    // Ignore errors
    let _status = Command::new("rustfmt").args(&args).status();

    Ok(())
}
