use libwasmrun_parse::lexer::Token;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file_path = &args[1];
    let contents = std::fs::read_to_string(file_path).unwrap();

    let mut chars = contents.char_indices().peekable();
    loop {
        match Token::parse(&mut chars) {
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
            Ok(None) => {
                break;
            }
            Ok(Some(token)) => {
                println!("{:?}", token);
            }
        }
    }
}
