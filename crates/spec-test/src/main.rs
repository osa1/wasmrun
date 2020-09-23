mod cli;
mod spec;

use cli::Args;
use libwasmrun::exec::store::ModuleIdx;
use libwasmrun::exec::{self, Runtime, Value};

use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::{exit, Command};

use parity_wasm::elements as wasm;

fn main() {
    let Args { file, accept } = cli::parse();

    let ret = match fs::read_dir(&file) {
        Ok(dir_contents) => run_spec_dir(dir_contents, accept),
        Err(_) => {
            let mut out = Output { file: None };
            run_spec_test(file.into(), &mut out)
        }
    };

    let exit_code = match ret {
        Err(err) => {
            println!("{}", err);
            1
        }
        Ok(exit_code) => exit_code,
    };

    exit(exit_code)
}

struct Output {
    file: Option<fs::File>,
}

impl Write for Output {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match &mut self.file {
            None => io::stdout().lock().write(buf),
            Some(file) => match file.write(buf) {
                Ok(n_written) => {
                    io::stdout().lock().write(&buf[..n_written]).unwrap();
                    Ok(n_written)
                }
                Err(err) => {
                    panic!("Unable to write to file: {}", err);
                }
            },
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        io::stdout().lock().flush().unwrap();
        match &mut self.file {
            Some(file) => file.flush(),
            None => Ok(()),
        }
    }
}

fn run_spec_dir(dir: fs::ReadDir, accept: bool) -> Result<i32, String> {
    let out_file = fs::File::create("test_output").unwrap();
    let mut out = Output {
        file: Some(out_file),
    };

    let mut exit_code = 0;

    for file in dir {
        let file = file.unwrap();
        let file_path = file.path();
        if let Some(ext) = file_path.extension() {
            if ext == "wast" {
                writeln!(
                    &mut out,
                    "{}",
                    file_path.file_name().unwrap().to_str().unwrap()
                )
                .unwrap();
                if run_spec_test(file_path, &mut out)? != 0 {
                    exit_code = 1;
                }
            }
        }
    }

    Ok(exit_code)
}

fn run_spec_test(path: PathBuf, out: &mut Output) -> Result<i32, String> {
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

    let mut exit_code = 0;
    let mut failing_lines = vec![];

    let mut rt = Runtime::new();
    let mut module_idx: Option<ModuleIdx> = None;

    for command in spec.commands {
        match command {
            spec::Command::Module { line, filename } => {
                write!(out, "\tline {}: ", line).unwrap();

                rt = Runtime::new();
                let file_path = format!("{}/{}", dir_path, filename);
                match wasm::deserialize_file(file_path) {
                    Err(err) => {
                        writeln!(out, "Error while parsing module: {}", err).unwrap();
                        module_idx = None;
                        exit_code = 1;
                        failing_lines.push(line);
                        continue;
                    }
                    Ok(module) => {
                        writeln!(out, "OK").unwrap();
                        let module = match module.parse_names() {
                            Err((_, module)) => {
                                writeln!(out, "Unable parse names").unwrap();
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
                write!(out, "\tline {}: ", line).unwrap();

                let module_idx = match module_idx {
                    None => {
                        writeln!(out, "module not available; skipping").unwrap();
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
                    writeln!(out, "OK").unwrap();
                } else {
                    writeln!(
                        out,
                        "expected != found. Expected: {:?}, Found: {:?}",
                        expected, found
                    )
                    .unwrap();
                    exit_code = 1;
                    failing_lines.push(line);
                }
            }
        }
    }

    if !failing_lines.is_empty() {
        writeln!(out, "Failing lines: {:?}", failing_lines).unwrap();
    }

    Ok(exit_code)
}