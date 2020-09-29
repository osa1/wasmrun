mod cli;
mod spec;

use cli::Args;
use libwasmrun::exec::{self, Runtime, Value};
use libwasmrun::store::ModuleIdx;

use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{exit, Command};

use parity_wasm::elements as wasm;

fn main() {
    let Args { file } = cli::parse();

    let exit_code = match fs::read_dir(&file) {
        Ok(dir_contents) => {
            let out_file = fs::File::create("test_output").unwrap();
            let mut out = Output {
                file: Some(out_file),
            };

            let mut dir_files: Vec<PathBuf> = vec![];

            for file in dir_contents {
                let file = file.unwrap();
                let file_path = file.path();
                if let Some(ext) = file_path.extension() {
                    if ext == "wast" {
                        dir_files.push(file_path);
                    }
                }
            }

            dir_files.sort();

            let fails = run_spec_dir(&dir_files, &mut out);
            let ret = if fails.is_empty() { 0 } else { 1 };
            for (file, lines) in fails.into_iter() {
                writeln!(&mut out, "{}: {:?}", file.to_string_lossy(), lines).unwrap();
            }
            ret
        }
        Err(_) => {
            let mut out = Output { file: None };
            let file_path: PathBuf = file.into();
            match run_spec_test(&file_path, &mut out) {
                Ok(fails) => {
                    if fails.is_empty() {
                        0
                    } else {
                        writeln!(&mut out, "{:?}", fails).unwrap();
                        1
                    }
                }
                Err(err) => {
                    writeln!(&mut out, "{}", err).unwrap();
                    1
                }
            }
        }
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

/// Run all .wast files in the given directory
fn run_spec_dir(dir: &[PathBuf], out: &mut Output) -> Vec<(PathBuf, Vec<usize>)> {
    let mut fails: Vec<(PathBuf, Vec<usize>)> = Default::default();

    for file_path in dir {
        if let Some(ext) = file_path.extension() {
            if ext == "wast" {
                writeln!(out, "{}", file_path.file_name().unwrap().to_str().unwrap()).unwrap();

                match run_spec_test(&file_path, out) {
                    Ok(failing_lines) => {
                        if !failing_lines.is_empty() {
                            fails.push((file_path.to_owned(), failing_lines));
                        }
                    }
                    Err(err) => {
                        writeln!(out, "{}", err).unwrap();
                    }
                }
            }
        }
    }

    fails
}

/// Run a single file
fn run_spec_test(path: &Path, out: &mut Output) -> Result<Vec<usize>, String> {
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
                return Err(format!(
                    "wast2json failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                ));
            }
        }
        Err(err) => {
            return Err(format!("wast2json failed: {}", err)).unwrap();
        }
    }

    let spec = spec::parse_test_spec(&spec_json_path);

    let mut failing_lines = vec![];

    let mut rt = Default::default();
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

    Ok(failing_lines)
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

            *rt = Default::default();
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
                    spec::Value::F32(f) => Value::F32(f),
                    spec::Value::F64(f) => Value::F64(f),
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

            // println!("rt after execution:");
            // println!("stack={:?}, frames={:?}", rt.stack, rt.frames);

            let mut expected_ = Vec::with_capacity(expected.len());
            for val in expected.into_iter() {
                match val {
                    spec::Value::I32(i) => {
                        expected_.push(Value::I32(i));
                    }
                    spec::Value::I64(i) => {
                        expected_.push(Value::I64(i));
                    }
                    spec::Value::F32(f) => {
                        expected_.push(Value::F32(f));
                    }
                    spec::Value::F64(f) => {
                        expected_.push(Value::F64(f));
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

            found.reverse();

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
