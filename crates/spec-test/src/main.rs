mod cli;
mod spec;

use cli::Args;
use libwasmrun::exec::store::ModuleIdx;
use libwasmrun::exec::{self, Runtime, Value};

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
        .arg("--debug-names")
        .arg(path)
        .arg("-o")
        .arg(&spec_json_path)
        .output()
        .map_err(|err| err.to_string())?;

    if !cmd_ret.status.success() {
        return Err("wast2json failed".to_string());
    }

    let spec = spec::parse_test_spec(&spec_json_path);
    // println!("{:#?}", spec);

    let mut rt = Runtime::new();
    let mut module_idx: Option<ModuleIdx> = None;

    for command in spec.commands {
        match command {
            spec::Command::Module { line, filename } => {
                print!("line {}: ", line);

                rt = Runtime::new();
                let file_path = format!("{}/{}", dir_path, filename);
                match wasm::deserialize_file(file_path) {
                    Err(err) => {
                        println!("Error while parsing module: {}", err);
                        module_idx = None;
                        continue;
                    }
                    Ok(module) => {
                        println!("OK");
                        let module = match module.parse_names() {
                            Err((_, module)) => {
                                println!("Unable parse names");
                                module
                            }
                            Ok(module) => module,
                        };
                        module_idx = Some(exec::allocate_module(&mut rt, module));
                    }
                }
            }

            spec::Command::AssertReturn {
                line,
                func,
                args,
                expected,
            } => {
                print!("line {}: ", line);

                let module_idx = match module_idx {
                    None => {
                        println!("module not available; skipping");
                        continue;
                    }
                    Some(module_idx) => module_idx,
                };

                for arg in args {
                    let val = match arg {
                        spec::Value::I32(i) => Value::I32(i),
                        spec::Value::I64(i) => Value::I64(i),
                        spec::Value::F32(_) | spec::Value::F64(_) => {
                            todo!("Float values are not supported yet");
                        }
                    };
                    rt.push_value(val);
                }

                exec::invoke_by_name(&mut rt, module_idx, &func);
                exec::finish(&mut rt);

                let expected = expected
                    .into_iter()
                    .map(|val| match val {
                        spec::Value::I32(i) => Value::I32(i),
                        spec::Value::I64(i) => Value::I64(i),
                        spec::Value::F32(_) | spec::Value::F64(_) => {
                            todo!("Float values are not supported yet");
                        }
                    })
                    .collect::<Vec<_>>();

                let n_expected = expected.len();
                let mut found = Vec::with_capacity(n_expected);

                for _ in 0..n_expected {
                    found.push(rt.pop_value().unwrap());
                }

                if expected == found {
                    println!("OK");
                } else {
                    println!(
                        "expected != found. Expected: {:?}, Found: {:?}",
                        expected, found
                    );
                }
            }
        }
    }

    Ok(())
}
