mod cli;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{exit, Command, Output};

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

fn run_cmd(pgm: &str, args: &[&str]) -> Output {
    print_cmd(pgm, args);

    let Output {
        status,
        stdout,
        stderr,
    } = Command::new(pgm).args(args.iter()).output().unwrap();

    let stdout_str = String::from_utf8_lossy(&stdout);
    let stdout_str = stdout_str.trim();
    if !stdout_str.is_empty() {
        println!("stdout:");
        println!("{}", stdout_str);
    }

    let stderr_str = String::from_utf8_lossy(&stderr);
    let stderr_str = stderr_str.trim();
    if !stderr_str.is_empty() {
        println!("stderr:");
        println!("{}", stderr_str);
    }

    Output {
        status,
        stdout,
        stderr,
    }
}

fn print_output(output: &Output) {
    println!("stdout: {:?}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: {:?}", String::from_utf8_lossy(&output.stderr));
    println!("status: {:?}", output.status);
}

fn report(wasmtime: &Output, wasmrun: &Output) -> bool {
    let mut fail = false;
    if wasmtime.stdout != wasmrun.stdout {
        println!("!!! stdouts don't match");
        fail = true;
    }
    if wasmtime.stderr != wasmrun.stderr {
        println!("!!! stderrs don't match");
        fail = true;
    }
    if wasmtime.status != wasmrun.status {
        println!("!!! statuses don't match");
        fail = true;
    }
    fail
}

fn run_file(file: &Path) -> bool {
    let mut fail = false;
    let file_str = file.to_str().unwrap();
    let wasm_interpreter_args: [&str; 1] = [file_str];

    let wasmtime_out = run_cmd("wasmtime", &wasm_interpreter_args);
    print_output(&wasmtime_out);

    let wasmrun_out = run_cmd(WASMRUN_PATH, &wasm_interpreter_args);
    print_output(&wasmrun_out);

    fail |= report(&wasmtime_out, &wasmrun_out);

    fail
}
