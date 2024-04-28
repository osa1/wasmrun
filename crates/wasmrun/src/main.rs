mod cli;

use libwasmrun::{exec, syntax, Runtime};

use std::process::exit;

fn main() {
    pretty_env_logger::init();

    let cli::Args { file, program_args } = cli::parse();

    let module = match syntax::deserialize_file(file) {
        Ok(module) => module,
        Err(err) => panic!("Error deserializing input file: {}", err),
    };

    let mut rt = Runtime::new_with_wasi(program_args);

    let module_addr = exec::instantiate(&mut rt, module).unwrap();

    if let Err(err) = exec::invoke_by_name(&mut rt, module_addr, "_start") {
        println!("Error while calling _start: {}", err);
        exit(1);
    }

    if let Err(err) = exec::finish(&mut rt) {
        println!("Runtime error: {}", err);
        print_backtrace(&rt);
        exit(1);
    }
}

fn print_backtrace(rt: &Runtime) {
    let bt = rt.backtrace();
    println!("Backtrace:");
    for (i, frame) in bt.frames.iter().enumerate() {
        let fun = rt.store.get_fun(frame.fun_addr);
        match fun.name() {
            None => println!("  {}: ???", i),
            Some(fun_name) => println!("  {}: {}", i, fun_name),
        }
    }
}
