// NOTE Index vs. address
// ~~~~~~~~~~~~~~~~~~~~~~
//
// Indices are module-local, e.g. "function 5" doesn't make sense in a program, "function 5 in
// module 10" makes sense.
//
// Addresses are indices in heap, rather than module, and global. (i.e. no two function live at the
// same address, but they may have same indices in their own modules)
//
// TODO: We should introduce newtypes for these.

mod cli;
mod exec;

use cli::Args;
use exec::Runtime;

use std::fs;
use std::path::PathBuf;
use std::process::{exit, Command};

use parity_wasm::elements as wasm;

fn main() {
    let Args { file, spec_test } = cli::parse();

    let ret = if spec_test {
        run_spec_test(file)
    } else {
        run_wasm(file)
    };

    match ret {
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

fn run_wasm(file: String) -> Result<(), String> {
    let module = wasm::deserialize_file(file).unwrap();
    // println!("{:#?}", module);

    let mut runtime = Runtime::new();
    let module_idx = exec::allocate_module(&mut runtime, module);

    // Run the 'start' function if it exists
    if let Some(start_idx) = runtime.get_module_start(module_idx) {
        println!("Calling start function {}", start_idx);
        exec::invoke(&mut runtime, module_idx, start_idx);
        exec::finish(&mut runtime);
    }

    // Find exported _start function and call it
    let mut start_fn = None;
    for export in &runtime.get_module(module_idx).exports {
        if export.field() == "_start" {
            match export.internal() {
                wasm::Internal::Function(func_idx) => {
                    start_fn = Some(*func_idx);
                    break;
                }
                wasm::Internal::Table(_)
                | wasm::Internal::Memory(_)
                | wasm::Internal::Global(_) => {}
            }
        }
    }

    if let Some(start_fn) = start_fn {
        println!("Calling _start ({})", start_fn);
        exec::invoke(&mut runtime, module_idx, start_fn);
        exec::finish(&mut runtime);
    }

    Ok(())
}
