#![allow(
    clippy::len_without_is_empty,
    clippy::new_without_default,
    clippy::type_complexity
)]

#[macro_use]
mod macros;

mod collections;
pub mod exec;
mod export;
mod frame;
mod fun;
mod mem;
mod module;
mod stack;
pub mod store;
mod type_canonicalizer;
mod value;
pub mod wasi;

use std::fmt::Display;
use std::rc::Rc;

pub use libwasmrun_syntax as syntax;

pub use exec::Runtime;
pub use mem::Mem;
pub use module::{FunIdx, MemIdx};
pub use store::{ExternAddr, MemAddr};
pub use syntax::Module;
pub use syntax::ValueType;
pub use value::{Ref, Value};
pub use wasi_common::{virtfs, Handle, WasiCtx, WasiCtxBuilder};

#[macro_use]
extern crate log;

#[derive(Debug, PartialEq, Eq)]
pub enum ExecError {
    /// Wasm code trapped.
    Trap(exec::Trap),

    /// Interpreter panic. For valid Wasm modules this means a bug in wasmrun.
    Panic(String),

    /// WASI error.
    WASI(String),

    /// WASI proc_exit called.
    Exit(i32),
}

impl Display for ExecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecError::Trap(trap) => write!(f, "Wasm module trapped: {}", trap),
            ExecError::Panic(msg) => write!(f, "Wasm interpreter panicked: {}", msg),
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
