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
    let Args { file } = cli::parse();

    let ret = match fs::read_dir(&file) {
        Ok(dir_contents) => run_spec_dir(dir_contents),
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

fn run_spec_dir(dir: fs::ReadDir) -> Result<i32, String> {
    let out_file = fs::File::create("test_output").unwrap();
    let mut out = Output {
        file: Some(out_file),
    };

    let mut exit_code = 0;

    for file in dir {
        println!("############ {:?}", file);
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
    let dir_path = format!("specs/{}-spec", stem);
    let _ = fs::create_dir_all(&dir_path);

    let spec_json_path = format!("{}/test.json", dir_path);

    let cmd_ret = Command::new("wast2json")
        .arg("--debug-names")
        .arg(path)
        .arg("-o")
        .arg(&spec_json_path)
        .output();

    match cmd_ret {
        Ok(output) => {
            if !output.status.success() {
                let stderr = output.stderr;
                writeln!(
                    out,
                    "wast2json failed: {}",
                    String::from_utf8_lossy(&stderr)
                )
                .unwrap();
                return Ok(1);
            }
        }
        Err(err) => {
            writeln!(out, "wast2json failed: {}", err).unwrap();
            return Ok(1);
        }
    }

    let spec = spec::parse_test_spec(&spec_json_path);

    let mut failing_lines = vec![];

    let mut rt = Runtime::new();
    let mut module_idx: Option<ModuleIdx> = None;

    for cmd in spec.commands {
        run_spec_cmd(
            cmd,
            &dir_path,
            out,
            &mut rt,
            &mut module_idx,
            &mut failing_lines,
        );
    }

    if !failing_lines.is_empty() {
        writeln!(out, "Failing lines: {:?}", failing_lines).unwrap();
    }

    Ok(if failing_lines.is_empty() { 0 } else { 1 })
}

fn run_spec_cmd(
    cmd: spec::Command,
    dir_path: &str,
    out: &mut Output,
    rt: &mut Runtime,
    module_idx: &mut Option<ModuleIdx>,
    failing_lines: &mut Vec<usize>,
) {
    match cmd {
        spec::Command::Module { line, filename } => {
            write!(out, "\tline {}: ", line).unwrap();
            // Flush the line number now so that we'll see it in case of a loop or hang
            out.flush().unwrap();

            *rt = Runtime::new();
            let file_path = format!("{}/{}", dir_path, filename);
            match wasm::deserialize_file(file_path) {
                Err(err) => {
                    writeln!(out, "Error while parsing module: {}", err).unwrap();
                    *module_idx = None;
                    failing_lines.push(line);
                    return;
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
                    match exec::allocate_module(rt, module) {
                        Ok(module_idx_) => {
                            *module_idx = Some(module_idx_);
                        }
                        Err(err) => {
                            writeln!(out, "Unable to allocate module: {}", err).unwrap();
                            *module_idx = None;
                        }
                    }
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
                    return;
                }
                Some(module_idx) => module_idx,
            };

            rt.clear_stack();
            for arg in args {
                let val = match arg {
                    spec::Value::I32(i) => Value::I32(i),
                    spec::Value::I64(i) => Value::I64(i),
                    spec::Value::F32(_) | spec::Value::F64(_) => {
                        writeln!(out, "Float values are not supported yet").unwrap();
                        return;
                    }
                };
                rt.push_value(val);
            }

            if let Err(err) = exec::invoke_by_name(rt, *module_idx, &func) {
                writeln!(out, "Error while calling function {}: {}", func, err).unwrap();
                failing_lines.push(line);
                return;
            }
            if let Err(err) = exec::finish(rt) {
                writeln!(out, "Error while running function {}: {}", func, err).unwrap();
                failing_lines.push(line);
                return;
            }

            let mut expected_ = Vec::with_capacity(expected.len());
            for val in expected.into_iter() {
                match val {
                    spec::Value::I32(i) => {
                        expected_.push(Value::I32(i));
                    }
                    spec::Value::I64(i) => {
                        expected_.push(Value::I64(i));
                    }
                    spec::Value::F32(_) | spec::Value::F64(_) => {
                        writeln!(out, "Float values are not supported yet").unwrap();
                        failing_lines.push(line);
                        return;
                    }
                }
            }

            let n_expected = expected_.len();
            let mut found = Vec::with_capacity(n_expected);

            for i in 0..n_expected {
                match rt.pop_value() {
                    Some(val) => {
                        found.push(val);
                    }
                    None => {
                        writeln!(out, "Can't pop return value {}", i + 1).unwrap();
                        failing_lines.push(line);
                        return;
                    }
                }
            }

            if expected_ == found {
                writeln!(out, "OK").unwrap();
            } else {
                writeln!(
                    out,
                    "expected != found. Expected: {:?}, Found: {:?}",
                    expected_, found
                )
                .unwrap();
                failing_lines.push(line);
            }
        }
    }
}
