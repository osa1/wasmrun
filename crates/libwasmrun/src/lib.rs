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
use std::path::Path;
use std::rc::Rc;

use libwasmrun_syntax as wasm;

pub use exec::Runtime;
pub use module::MemIdx;
pub use store::{ExternAddr, MemAddr};
pub use value::{Ref, Value};
pub use wasi_common::{virtfs, Handle, WasiCtx, WasiCtxBuilder};
pub use wasm::Module;
pub use wasm::ValueType;

#[macro_use]
extern crate log;

#[derive(Debug, PartialEq, Eq)]
pub enum ExecError {
    /// Wasm code trapped
    Trap(exec::Trap),
    /// Invalid module, unsupported operation, IO error, or a bug
    Panic(String),
    /// WASI error
    WASI(String),
    /// WASI proc_exit called
    Exit(i32),
}

impl Display for ExecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecError::Trap(trap) => write!(f, "Wasm module trapped: {}", trap),
            ExecError::Panic(msg) => write!(f, "Interpreter panicked: {}", msg),
            ExecError::WASI(wasi_err) => write!(f, "WASI error: {}", wasi_err),
            ExecError::Exit(exit) => write!(f, "proc_exit({})", exit),
        }
    }
}

pub struct HostFunDecl {
    pub arg_tys: Vec<ValueType>,
    pub ret_tys: Vec<ValueType>,
    pub fun: Rc<dyn Fn(&mut Runtime, Option<MemAddr>) -> Result<Vec<Value>>>,
}

pub type Result<A> = ::std::result::Result<A, ExecError>;

pub fn load_wasm<P: AsRef<Path>>(file: P) -> Result<Module> {
    wasm::deserialize_file(file)
        .map_err(|err| ExecError::Panic(format!("Unable to parse module: {}", err)))
}
