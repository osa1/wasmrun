#![feature(backtrace, or_patterns)]

//mod exec;
mod parser;

// use exec::Runtime;

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

    // let mut runtime = Runtime::default();
    // let module_idx = runtime.allocate_module(&mut module);

    // runtime.run_module(module_idx);
}
