mod cli;
mod cmd;

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{exit, Command};
use std::sync::{Arc, RwLock};

use libwasmrun_syntax::elements as wasm;

use libwasmrun::virtfs::pipe::{ReadPipe, WritePipe};
use libwasmrun::virtfs::{VecFileContents, VirtualDirEntry};
use libwasmrun::{exec, ExecError, Runtime, WasiCtx, WasiCtxBuilder};

static WASMRUN_PATH: &str = "target/debug/wasmrun";

fn main() {
    pretty_env_logger::init();

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
    wasm_out: Arc<RwLock<Vec<u8>>>,
    wasm_err: Arc<RwLock<Vec<u8>>>,
    wasm_exit: i32,
    expected_out: &str,
    expected_err: &str,
    expected_exit: i32,
) -> bool {
    let mut fail = false;

    let out_ref = wasm_out.read().unwrap();
    let out = String::from_utf8_lossy(&*out_ref);

    let err_ref = wasm_err.read().unwrap();
    let err = String::from_utf8_lossy(&*err_ref);

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

fn handle_commands(
    cmds: Vec<cmd::Cmd>,
) -> (WasiCtx, i32, Arc<RwLock<Vec<u8>>>, Arc<RwLock<Vec<u8>>>) {
    let mut exit = 0;
    let mut wasi_builder = WasiCtxBuilder::new();

    wasi_builder.stdin(ReadPipe::from(""));

    let stdout_buf: Arc<RwLock<Vec<u8>>> = Arc::new(RwLock::new(vec![]));
    wasi_builder.stdout(WritePipe::from_shared(stdout_buf.clone()));

    let stderr_buf: Arc<RwLock<Vec<u8>>> = Arc::new(RwLock::new(vec![]));
    wasi_builder.stderr(WritePipe::from_shared(stderr_buf.clone()));

    let mut fs_map = HashMap::new();

    for cmd in cmds {
        match cmd {
            cmd::Cmd::Preopen {
                wasm_path,
                file_path,
            } => {
                // TODO: This assumes all files are relative to the wasm file's directory
                // TODO: Reading the file eagerly for now
                // TODO: Hard-coded file path
                let path = format!("tests/wasi/{}", file_path);
                let file_contents = fs::read(path).unwrap();
                let entry =
                    VirtualDirEntry::File(Box::new(VecFileContents::with_content(file_contents)));
                fs_map.insert(wasm_path, entry);
            }
            cmd::Cmd::ExitCode(exit_) => {
                exit = exit_;
            }
        }
    }

    let dir = VirtualDirEntry::Directory(fs_map);
    wasi_builder.preopened_virt(dir, ".");

    (wasi_builder.build().unwrap(), exit, stdout_buf, stderr_buf)
}

fn run_file(file: &Path) -> bool {
    println!("{}", file.to_string_lossy());

    let file_stem = file.file_stem().unwrap().to_str().unwrap();
    let file_src_path = format!("tests/wasi/src/{}.rs", file_stem);
    let file_src = fs::read_to_string(file_src_path).unwrap();

    let cmds = cmd::parse_cmds(&file_src);
    let (wasi, expected_exit, stdout, stderr) = handle_commands(cmds);
    let mut rt = Runtime::new_with_wasi_ctx(wasi);

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

            /*
            let err = {
                let mut err = String::new();
                let mut err_file = unsafe { fs::File::from_raw_fd(stderr) };
                err_file.read_to_string(&mut err).unwrap();
                err
            };
            */

            println!("\tStderr: {}", err);

            return true;
        }
    };

    report(stdout, stderr, exit, &out, &err, expected_exit)
}
