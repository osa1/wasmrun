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
pub mod frame;
pub mod mem;
pub mod stack;
pub mod store;
pub mod value;

use std::fmt::Display;

pub use exec::Runtime;

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
    let module_idx = exec::allocate_module(&mut runtime, module)?;

    // Run the 'start' function if it exists
    if let Some(start_idx) = runtime.get_module_start(module_idx) {
        println!("Calling start function {}", start_idx);
        exec::invoke(&mut runtime, module_idx, start_idx)?;
        exec::finish(&mut runtime)?;
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
        exec::invoke(&mut runtime, module_idx, start_fn)?;
        exec::finish(&mut runtime)?;
    }

    Ok(())
}
