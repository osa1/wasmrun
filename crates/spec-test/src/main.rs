mod cli;

use cli::Args;

use std::fs;
use std::path::PathBuf;
use std::process::{exit, Command};

fn main() {
    let Args { file } = cli::parse();
    match run_spec_test(file) {
        Ok(()) => {}
        Err(err) => {
            println!("{}", err);
            exit(1);
        }
    }
}

fn run_spec_test(file: String) -> Result<(), String> {
    let path: PathBuf = file.into();

    match path.extension() {
        Some(ext) => {
            if ext != "wast" {
                return Err(format!(
                    "Spec test extension should be .wast, found: {:?}",
                    ext
                ));
            }
        }
        None => {
            return Err("Spec file should have .wast extension".to_string());
        }
    }

    let stem = path.file_stem().unwrap().to_str().unwrap();
    let dir_path = format!("{}-spec", stem);
    let _ = fs::create_dir(&dir_path);

    let cmd_ret = Command::new("wast2json")
        .arg(path)
        .arg("-o")
        .arg(format!("{}/test.json", dir_path))
        .output()
        .map_err(|err| err.to_string())?;

    if !cmd_ret.status.success() {
        return Err("wast2json failed".to_string());
    }

    Ok(())
}
