use libwasmrun_wast::Lexer;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let contents = std::fs::read_to_string(file).unwrap();
    let lexer = Lexer::new(&contents);

    for token in lexer {
        print!("{:?} ", token.unwrap().1);
    }

    println!();
}
