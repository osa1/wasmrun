mod exec;
mod parser;

use exec::Runtime;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let file = &args[1];

    let bytes = std::fs::read(file).unwrap();
    let module = parser::parse(&bytes).unwrap();
    println!("{:#?}", module);

    let mut runtime = Runtime::default();
    runtime.allocate_module(module);
}
