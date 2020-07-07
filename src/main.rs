mod exec;
mod parser;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let file = &args[1];

    let bytes = std::fs::read(file).unwrap();
    let module = parser::parse(&bytes).unwrap();
    println!("{:#?}", module);
}
