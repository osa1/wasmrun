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

pub mod exec;
mod export;
mod frame;
mod mem;
mod module;
mod spectest;
mod stack;
pub mod store;
pub mod value;

use std::fmt::Display;

use exec::Runtime;

use parity_wasm::elements as wasm;

#[derive(Debug)]
pub enum ExecError {
    /// Wasm code trapped
    Trap,
    /// Invalid module, unsupported operation, IO error, or a bug
    Panic(String),
}

impl Display for ExecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecError::Trap => write!(f, "Wasm module trapped"),
            ExecError::Panic(msg) => write!(f, "Interpreter panicked: {}", msg),
        }
    }
}

pub type Result<A> = ::std::result::Result<A, ExecError>;

pub fn run_wasm(file: String) -> Result<()> {
    let module = wasm::deserialize_file(file).unwrap();
    // println!("{:#?}", module);

    let mut runtime = Runtime::new();
    // allocate_module also runs 'start'
    let _module_idx = exec::allocate_module(&mut runtime, module)?;

    /*
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
    */

    Ok(())
}
