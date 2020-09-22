mod cli;
mod spec;

use cli::Args;
use libwasmrun::exec::store::ModuleIdx;
use libwasmrun::exec::{self, Runtime};

use std::fs;
use std::path::PathBuf;
use std::process::{exit, Command};

use parity_wasm::elements as wasm;

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

    let spec_json_path = format!("{}/test.json", dir_path);

    let cmd_ret = Command::new("wast2json")
        .arg(path)
        .arg("-o")
        .arg(&spec_json_path)
        .output()
        .map_err(|err| err.to_string())?;

    if !cmd_ret.status.success() {
        return Err("wast2json failed".to_string());
    }

    let spec = spec::parse_test_spec(&spec_json_path);
    println!("{:#?}", spec);

    let mut runtime = Runtime::new();
    let mut module_idx: Option<ModuleIdx> = None;

    for command in spec.commands {
        match command {
            spec::Command::Module { line, filename } => {
                let file_path = format!("{}/{}", dir_path, filename);
                match wasm::deserialize_file(file_path) {
                    Err(err) => {
                        println!("Error while parsing module at line {}: {}", line, err);
                        module_idx = None;
                        continue;
                    }
                    Ok(module) => {
                        module_idx = Some(exec::allocate_module(&mut runtime, module));
                    }
                }
            }

            spec::Command::AssertReturn {
                line,
                func,
                args,
                expected,
            } => {
                let module_idx = match module_idx {
                    None => {
                        println!("Module not available; skipping test at line {}", line);
                        continue;
                    }
                    Some(module_idx) => module_idx,
                };
            }
        }
    }

    Ok(())
}
