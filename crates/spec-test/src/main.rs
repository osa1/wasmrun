mod cli;
mod spec;

use libwasmrun::exec::{self, Runtime, Trap};
use libwasmrun::store::ModuleAddr;
use libwasmrun::{ExecError, Value};

use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{exit, Command};

use fxhash::FxHashMap;
use libwasmrun_syntax as wasm;

fn main() {
    let cli::Args { file } = cli::parse();
    let file_path = file.as_ref().map(|s| s.as_str()).unwrap_or("tests/spec");
    let file_meta = fs::metadata(file_path).expect("Unable to get input file or dir metadata");

    if file_meta.is_dir() {
        run_dir(file_path);
    } else if file_meta.is_file() {
        run_file(file_path);
    } else {
        eprintln!("Input file {} is not a directory or file", file_path);
        exit(1);
    }
}

fn run_dir(dir_path: &str) {
    let dir_contents = fs::read_dir(dir_path).expect("Unable to get dir contents");

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
    exit(ret)
}

fn run_file(file_path: &str) {
    let mut out = Output { file: None };
    let file_path: PathBuf = file_path.into();

    match file_path.extension() {
        Some(ext) => {
            if ext != "wast" {
                writeln!(out, "Spec test extension should be .wast, found: {:?}", ext).unwrap();
                exit(1);
            }
        }
        None => {
            writeln!(out, "Spec file should have .wast extension").unwrap();
            exit(1);
        }
    }

    let ret = match run_spec_test(&file_path, &mut out) {
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
    };
    exit(ret)
}

fn test_eq_val(v1: Value, v2: Value) -> bool {
    match (v1, v2) {
        (Value::I32(i1), Value::I32(i2)) => i1 == i2,
        (Value::I64(i1), Value::I64(i2)) => i1 == i2,
        (Value::F32(f1), Value::F32(f2)) => f1.to_bits() == f2.to_bits(),
        (Value::F64(f1), Value::F64(f2)) => f1.to_bits() == f2.to_bits(),
        _ => false,
    }
}

fn test_eq_vals(vs1: &[Value], vs2: &[Value]) -> bool {
    vs1.len() == vs2.len()
        && vs1
            .iter()
            .zip(vs2.iter())
            .all(|(v1, v2)| test_eq_val(*v1, *v2))
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
                        fails.push((file_path.to_owned(), vec![]));
                    }
                }
            }
        }
    }

    fails
}

/// Run a single file
fn run_spec_test(path: &Path, out: &mut Output) -> Result<Vec<usize>, String> {
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

    let spec = match spec::parse_test_spec(&spec_json_path) {
        Ok(spec) => spec,
        Err(lines) => return Ok(lines),
    };

    let mut failing_lines = vec![];
    let mut rt = Runtime::new_test();
    let mut module_addr: Option<ModuleAddr> = None;
    let mut modules = Default::default();

    for cmd in spec.commands {
        run_spec_cmd(
            cmd,
            &dir_path,
            out,
            &mut rt,
            &mut module_addr,
            &mut modules,
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
    module_addr: &mut Option<ModuleAddr>,
    modules: &mut FxHashMap<String, ModuleAddr>,
    failing_lines: &mut Vec<usize>,
) {
    match cmd {
        spec::Command::Module {
            line,
            name,
            filename,
        } => {
            // println!("module name={:?}", name);

            write!(out, "\tline {}: ", line).unwrap();
            // Flush the line number now so that we'll see it in case of a loop or hang
            out.flush().unwrap();

            let file_path = format!("{}/{}", dir_path, filename);
            match wasm::deserialize_file(&file_path) {
                Err(err) => {
                    writeln!(out, "Error while parsing module: {} ({})", err, file_path).unwrap();
                    *module_addr = None;
                    failing_lines.push(line);
                }
                Ok(module) => match exec::instantiate(rt, module) {
                    Ok(module_addr_) => {
                        writeln!(out, "OK").unwrap();
                        *module_addr = Some(module_addr_);
                        if let Some(name) = name {
                            modules.insert(name, module_addr_);
                        }
                    }
                    Err(err) => {
                        writeln!(out, "Unable to allocate module: {} ({})", err, file_path)
                            .unwrap();
                        *module_addr = None;
                    }
                },
            }
        }

        spec::Command::AssertUninstantiable {
            line,
            filename,
            text: _,
        } => {
            write!(out, "\tline {}: ", line).unwrap();
            out.flush().unwrap();

            let file_path = format!("{}/{}", dir_path, filename);
            match wasm::deserialize_file(file_path) {
                Err(err) => {
                    writeln!(out, "Error while parsing module: {}", err).unwrap();
                    failing_lines.push(line);
                }
                Ok(module) => match exec::instantiate(rt, module) {
                    Ok(_) => {
                        writeln!(out, "Successfully allocated module").unwrap();
                        failing_lines.push(line);
                    }
                    Err(_err) => {
                        writeln!(out, "OK").unwrap();
                    }
                },
            }
        }

        spec::Command::Register {
            line,
            name,
            register_as,
        } => {
            // println!("register name={:?}, register_as={}", name, register_as);

            write!(out, "\tline {}: ", line).unwrap();
            // Flush the line number now so that we'll see it in case of a loop or hang
            out.flush().unwrap();

            let module_addr = match name {
                Some(name) => match modules.get(&name) {
                    Some(module_addr) => module_addr,
                    None => {
                        writeln!(out, "module not available; skipping").unwrap();
                        return;
                    }
                },
                None => match module_addr {
                    Some(module_addr) => module_addr,
                    None => {
                        writeln!(out, "module not available; skipping").unwrap();
                        return;
                    }
                },
            };

            rt.register_module(register_as, *module_addr);

            writeln!(out, "OK").unwrap();
        }

        spec::Command::AssertReturn {
            line,
            kind: spec::ActionKind::GetGlobal,
            module,
            func,
            args,
            expected,
            err_msg: _,
        } => {
            assert!(args.is_empty());

            write!(out, "\tline {}: ", line).unwrap();

            let module_addr = match module {
                Some(module_name) => match modules.get(&module_name) {
                    Some(module_addr) => *module_addr,
                    None => {
                        writeln!(out, "can't find registered module {}", module_name).unwrap();
                        failing_lines.push(line);
                        return;
                    }
                },
                None => match module_addr {
                    Some(module_addr) => *module_addr,
                    None => {
                        writeln!(out, "module not available; skipping").unwrap();
                        return;
                    }
                },
            };

            match rt.get_global(module_addr, &func) {
                None => {
                    writeln!(out, "can't find global {:?}", func).unwrap();
                    failing_lines.push(line);
                    return;
                }
                Some(val) => {
                    assert_eq!(expected.len(), 1);
                    let expected = expected[0];

                    if test_eq_val(expected, val) {
                        writeln!(out, "OK").unwrap();
                    } else {
                        writeln!(
                            out,
                            "expected != found. Expected: {:?}, Found: {:?}",
                            expected, val
                        )
                        .unwrap();
                        failing_lines.push(line);
                    }
                }
            }
        }

        spec::Command::AssertReturn {
            line,
            kind,
            module,
            func,
            args,
            expected,
            err_msg,
        } => {
            // println!("invoke module={:?}, func={}", module, func);

            write!(out, "\tline {}: ", line).unwrap();

            let module_addr = match module {
                Some(module_name) => match modules.get(&module_name) {
                    Some(module_addr) => *module_addr,
                    None => {
                        writeln!(out, "can't find registered module {}", module_name).unwrap();
                        failing_lines.push(line);
                        return;
                    }
                },
                None => match module_addr {
                    Some(module_addr) => *module_addr,
                    None => {
                        writeln!(out, "module not available; skipping").unwrap();
                        return;
                    }
                },
            };

            rt.clear_stack();
            for arg in args {
                rt.push_value(arg);
            }

            if let Err(err) = exec::invoke_by_name(rt, module_addr, &func) {
                writeln!(out, "Error while calling function {}: {}", func, err).unwrap();
                failing_lines.push(line);
                return;
            }

            let exec_ret = exec::finish(rt);

            match kind {
                spec::ActionKind::Invoke => {
                    if let Err(err) = exec_ret {
                        writeln!(out, "Error while running function {}: {}", func, err).unwrap();
                        failing_lines.push(line);
                        return;
                    }

                    let n_expected = expected.len();
                    let mut found = Vec::with_capacity(n_expected);

                    for i in 0..n_expected {
                        match rt.pop_value() {
                            Some(val) => {
                                found.push(val.into());
                            }
                            None => {
                                writeln!(out, "Can't pop return value {}", i + 1).unwrap();
                                failing_lines.push(line);
                                return;
                            }
                        }
                    }

                    found.reverse();

                    if test_eq_vals(&expected, &found) {
                        writeln!(out, "OK").unwrap();
                    } else {
                        writeln!(
                            out,
                            "expected != found. Expected: {:?}, Found: {:?}",
                            expected, found
                        )
                        .unwrap();
                        failing_lines.push(line);
                    }
                }

                spec::ActionKind::Trap => {
                    let err_msg = err_msg.unwrap();

                    match exec_ret {
                        Ok(()) => {
                            writeln!(
                                out,
                                "assert_trap function succeeded: {}. Expected error: {:?}",
                                func, err_msg
                            )
                            .unwrap();
                            failing_lines.push(line);
                        }
                        Err(ExecError::Panic(msg)) => {
                            writeln!(
                                out,
                                "assert_trap function panicked: {}. Expected error: {:?}",
                                msg, err_msg
                            )
                            .unwrap();
                            failing_lines.push(line);
                        }
                        Err(ExecError::Trap(trap)) => {
                            let trap_msg = trap_expected_msg(trap);

                            // https://github.com/WebAssembly/spec/blob/7526564b56c30250b66504fe795e9c1e88a938af/interpreter/script/run.ml#L368-L376
                            if !err_msg.starts_with(trap_msg) {
                                writeln!(
                                    out,
                                    "Unexpected trap: expected {:?}, found {:?}",
                                    err_msg, trap_msg
                                )
                                .unwrap();
                                failing_lines.push(line);
                            } else {
                                writeln!(out, "OK").unwrap();
                            }
                        }
                        Err(ExecError::WASI(_msg)) => {
                            panic!("WASI function called in a spec test");
                        }
                        Err(ExecError::Exit(_exit)) => {
                            panic!("WASI proc_exit called in a spec test");
                        }
                    }
                }

                spec::ActionKind::GetGlobal => panic!(), // already handled
            }
        }
    }
}

fn trap_expected_msg(trap: Trap) -> &'static str {
    match trap {
        Trap::UndefinedElement => "undefined",
        Trap::UninitializedElement => "uninitialized",
        Trap::IndirectCallTypeMismatch => "indirect call",
        Trap::OOBTableElementIdx => "element out of bounds",
        Trap::OOBMemoryAccess => "out of bounds memory access",
        Trap::IntDivideByZero => "integer divide by zero",
        Trap::IntOverflow => "integer overflow",
        Trap::InvalidConvToInt => "invalid conversion to integer",
        Trap::Unreachable => "unreachable",
        Trap::CallIndirectOnExternRef => "", // TODO
    }
}
