mod borrow;
pub mod exec;
mod export;
mod frame;
mod fun;
mod mem;
mod module;
mod stack;
pub mod store;
mod value;
pub mod wasi;

use std::fmt::Display;

use parity_wasm::elements as wasm;

pub use exec::Runtime;
pub use store::MemAddr;
pub use value::Value;
pub use wasi_common::{virtfs, Handle, WasiCtx, WasiCtxBuilder};
pub use wasm::ValueType;

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

pub struct HostFunDecl {
    pub arg_tys: Vec<ValueType>,
    pub ret_tys: Vec<ValueType>,
    pub fun: fn(&mut Runtime) -> Result<Vec<Value>>,
}

pub type Result<A> = ::std::result::Result<A, ExecError>;

pub fn run_wasm(file: String, args: Vec<String>) -> Result<()> {
    let module = match wasm::deserialize_file(&file) {
        Ok(module) => module,
        Err(err) => {
            return Err(ExecError::Panic(format!("Unable to parse module: {}", err)));
        }
    };

    let mut wasi_builder = WasiCtxBuilder::new();
    wasi_builder.args(args);

    let wasi_ctx = wasi_builder.build().unwrap();
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
