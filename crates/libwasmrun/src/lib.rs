pub mod exec;
mod export;
mod frame;
mod fun;
mod mem;
mod module;
mod stack;
pub mod store;
pub mod value;
pub mod wasi;

use std::ffi::CString;
use std::fmt::Display;
use std::path::Path;

use parity_wasm::elements as wasm;

pub use exec::Runtime;
pub use wasi::{Dir, File, FileOrDir, WasiCtx, WasiCtxBuilder};

#[macro_use]
extern crate log;

#[derive(Debug)]
pub enum ExecError {
    /// Wasm code trapped
    Trap(exec::Trap),
    /// Invalid module, unsupported operation, IO error, or a bug
    Panic(String),
    /// WASI proc_exit called
    Exit(i32),
}

impl Display for ExecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecError::Trap(trap) => write!(f, "Wasm module trapped: {}", trap),
            ExecError::Panic(msg) => write!(f, "Interpreter panicked: {}", msg),
            ExecError::Exit(exit) => write!(f, "proc_exit({})", exit),
        }
    }
}

pub type Result<A> = ::std::result::Result<A, ExecError>;

pub fn run_wasm(file: String, args: Vec<String>) -> Result<()> {
    let module = match wasm::deserialize_file(&file) {
        Ok(module) => module,
        Err(err) => {
            return Err(ExecError::Panic(format!("Unable to parse module: {}", err)));
        }
    };

    let mut args = args
        .into_iter()
        .map(|arg| CString::new(arg).unwrap())
        .collect::<Vec<_>>();
    let file_name = Path::new(&file).file_name().unwrap().to_str().unwrap();
    args.insert(0, CString::new(file_name).unwrap());

    let mut wasi_builder = WasiCtxBuilder::new();
    wasi_builder.set_args(args);
    let wasi_ctx = wasi_builder.build();
    let mut rt = Runtime::new_with_wasi(wasi_ctx);

    // allocate_module also runs 'start'
    let module_addr = exec::allocate_module(&mut rt, module)?;

    // Find exported _start function and call it
    match exec::invoke_by_name(&mut rt, module_addr, "_start").and_then(|()| exec::finish(&mut rt))
    {
        Ok(()) => Ok(()),
        Err(err) => Err(ExecError::Panic(format!(
            "Error while invoking _start: {:?}",
            err
        ))),
    }
}
