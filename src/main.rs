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
mod parser;

use exec::Runtime;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let file = &args[1];

    let bytes = std::fs::read(file).unwrap();
    let module = match parser::parse(&bytes) {
        Ok(module) => module,
        Err(err) => {
            eprintln!("{:#?}", err);
            ::std::process::exit(1);
        }
    };
    println!("{:#?}", module);

    let mut runtime = Runtime::default();
    let module_idx = runtime.allocate_module(module);

    // Run the 'start' function if it exists
    if let Some(start_idx) = runtime.get_module_start(module_idx) {
        runtime.call(module_idx, start_idx);
    }
}
