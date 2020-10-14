mod cli;
mod cmd;

use std::fs;
use std::io::Read;
use std::os::unix::io::{FromRawFd, RawFd};
use std::path::{Path, PathBuf};
use std::process::{exit, Command};

use parity_wasm::elements as wasm;

use libwasmrun::{exec, ExecError, File, FileOrDir, Runtime, WasiCtx, WasiCtxBuilder};

static WASMRUN_PATH: &str = "target/debug/wasmrun";

fn main() {
    let cli::Args { file } = cli::parse();
    let file_or_dir = file
        .as_ref()
        .map(|s| s.as_str())
        .unwrap_or("tests/wasi/target/wasm32-wasi/debug");

    print_version("cargo");
    print_version("rustc");
    print_version("wasmtime");

    print_cmd("cargo", &["build"]);
    let _ = Command::new("cargo").arg("build").status().unwrap();

    print_version(WASMRUN_PATH);

    println!("cd tests/wasi; cargo build");
    let status = Command::new("cargo")
        .arg("build")
        .current_dir("tests/wasi")
        .status()
        .unwrap();

    assert!(status.success());

    let fail = run(file_or_dir);
    if fail {
        exit(1);
    }
}

fn print_version(pgm: &str) {
    let cmd_ret = Command::new(pgm).arg("--version").output();
    match cmd_ret {
        Ok(output) => {
            println!(
                "{} version: {}",
                pgm,
                String::from_utf8_lossy(&output.stdout).trim()
            );
        }
        Err(err) => {
            println!("`{} --version` failed: {}", pgm, err);
            exit(1);
        }
    }
}

fn run(file_or_dir: &str) -> bool {
    let mut fail = false;

    match fs::read_dir(file_or_dir) {
        Ok(dir_contents) => {
            let mut dir_files: Vec<PathBuf> = vec![];

            for file in dir_contents {
                let file = file.unwrap();
                let file_path = file.path();
                if let Some(ext) = file_path.extension() {
                    if ext == "wasm" {
                        dir_files.push(file_path);
                    }
                }
            }

            dir_files.sort();

            for file in &dir_files {
                fail |= run_file(file);
            }
        }
        Err(_) => {
            fail |= run_file(PathBuf::from(file_or_dir).as_path());
        }
    }

    fail
}

fn print_cmd(pgm: &str, args: &[&str]) {
    println!("Running {} {}", pgm, args.join(" "));
}

fn report(
    wasm_out: RawFd,
    wasm_err: RawFd,
    wasm_exit: i32,
    expected_out: &str,
    expected_err: &str,
    expected_exit: i32,
) -> bool {
    let mut fail = false;

    let out = {
        let mut out = String::new();
        let mut out_file = unsafe { fs::File::from_raw_fd(wasm_out) };
        out_file.read_to_string(&mut out).unwrap();
        out
    };

    let err = {
        let mut err = String::new();
        let mut err_file = unsafe { fs::File::from_raw_fd(wasm_err) };
        err_file.read_to_string(&mut err).unwrap();
        err
    };

    if out != expected_out {
        println!("\tExpected and actual stdsout outputs don't match");
        println!("\tExpected: {:?}", expected_out);
        println!("\tFound:    {:?}", out);
        fail = true;
    }

    if err != expected_err {
        println!("\tExpected and actual stderr outputs don't match");
        println!("\tExpected: {:?}", expected_err);
        println!("\tFound:    {:?}", err);
        fail = true;
    }

    if wasm_exit != expected_exit {
        println!("\tExpected and actual exit codes don't match");
        println!("\tExpected: {}", expected_exit);
        println!("\tFound:    {}", wasm_exit);
        fail = true;
    }

    fail
}

fn handle_commands(cmds: Vec<cmd::Cmd>) -> (WasiCtx, i32, RawFd, RawFd) {
    let mut exit = 0;
    let mut wasi_builder = WasiCtxBuilder::new();

    let (out_read, out_write) = nix::unistd::pipe().unwrap();
    let (err_read, err_write) = nix::unistd::pipe().unwrap();

    wasi_builder.stdout(out_write);
    wasi_builder.stderr(err_write);

    for cmd in cmds {
        match cmd {
            cmd::Cmd::Preopen {
                wasm_path,
                file_path,
            } => {
                wasi_builder.preopen(wasm_path, FileOrDir::File(File::Pending(file_path.into())));
            }
            cmd::Cmd::ExitCode(exit_) => {
                exit = exit_;
            }
        }
    }

    (wasi_builder.build(), exit, out_read, err_read)
}

fn run_file(file: &Path) -> bool {
    println!("{}", file.to_string_lossy());

    let file_stem = file.file_stem().unwrap().to_str().unwrap();
    let file_src_path = format!("tests/wasi/src/{}.rs", file_stem);
    let file_src = fs::read_to_string(file_src_path).unwrap();

    let cmds = cmd::parse_cmds(&file_src);
    let (wasi, expected_exit, stdout, stderr) = handle_commands(cmds);
    let mut rt = Runtime::new_with_wasi(wasi);

    let out_path = format!("tests/wasi/src/{}.out", file_stem);
    let out = fs::read_to_string(out_path).unwrap_or_else(|_| "".to_owned());

    let err_path = format!("tests/wasi/src/{}.err", file_stem);
    let err = fs::read_to_string(err_path).unwrap_or_else(|_| "".to_owned());

    let module = match wasm::deserialize_file(&file) {
        Ok(module) => module,
        Err(err) => {
            println!("\tUnable to parse Wasm: {}", err);
            return true;
        }
    };

    let module_addr = match exec::allocate_module(&mut rt, module) {
        Ok(module_addr) => module_addr,
        Err(err) => {
            println!("\tUnable to load Wasm module: {}", err);
            return true;
        }
    };

    let exit = match exec::invoke_by_name(&mut rt, module_addr, "_start")
        .and_then(|()| exec::finish(&mut rt))
    {
        Ok(()) => 0,
        Err(ExecError::Exit(exit)) => exit,
        Err(err) => {
            println!("\tError while invoking _start: {}", err);

            drop(rt); // FIXME: closes stdout and stderr

            let err = {
                let mut err = String::new();
                let mut err_file = unsafe { fs::File::from_raw_fd(stderr) };
                err_file.read_to_string(&mut err).unwrap();
                err
            };

            println!("\tStderr: {}", err);

            return true;
        }
    };

    drop(rt); // FIXME: closes stdout and stderr

    report(stdout, stderr, exit, &out, &err, expected_exit)
}
