mod exec;
mod parser;

use exec::Runtime;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let file = &args[1];

    let bytes = std::fs::read(file).unwrap();
    let mut module = parser::parse(&bytes).unwrap();
    println!("{:#?}", module);

    let mut runtime = Runtime::default();
    let module_idx = runtime.allocate_module(&mut module);

    runtime.run_module(module_idx);
}
