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

#![feature(backtrace, or_patterns)]

mod exec;
// mod parser;

use exec::Runtime;

use parity_wasm::elements as wasm;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let file = &args[1];

    let module = wasm::deserialize_file(file).unwrap();
    // println!("{:#?}", module);

    let mut runtime = Runtime::default();
    let module_idx = exec::allocate_module(&mut runtime, module);

    // Run the 'start' function if it exists
    if let Some(start_idx) = runtime.get_module_start(module_idx) {
        println!("Calling start function {}", start_idx);
        exec::call(&mut runtime, module_idx, start_idx);
    }

    /*
        // Find exported _start function and call it
        let mut start_fn = None;
        for export in &runtime.get_module(module_idx).exports {
            if export.nm == "_start" {
                match export.desc {
                    parser::ExportDesc::Func(func_idx) => {
                        start_fn = Some(func_idx);
                        break;
                    }
                    _ => {
                        break;
                    }
                }
            }
        }

        if let Some(start_fn) = start_fn {
            println!("Calling _start ({})", start_fn);
            exec::call(&mut runtime, module_idx, start_fn);
        }
    */
}
