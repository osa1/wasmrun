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

use exec::Runtime;

use parity_wasm::elements as wasm;

pub use wasi::allocate_wasi;

#[derive(Debug)]
pub enum ExecError {
    /// Wasm code trapped
    Trap(exec::Trap),
    /// Invalid module, unsupported operation, IO error, or a bug
    Panic(String),
}

impl Display for ExecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecError::Trap(_trap) => write!(f, "Wasm module trapped"),
            ExecError::Panic(msg) => write!(f, "Interpreter panicked: {}", msg),
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

    //args.insert(0, file_name.to_string_lossy());

    let mut rt = Runtime::new_with_args(args);
    let wasi_module_addr = allocate_wasi(&mut rt.store);
    rt.register_module("wasi_snapshot_preview1".to_owned(), wasi_module_addr);

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
