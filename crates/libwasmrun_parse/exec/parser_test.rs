//! Parses all .wast files in given dir, or parses the given file if the argument is a file

use libwasmrun_parse::lexer::Lexer;
use libwasmrun_parse::parser::TestFileParser;

use std::borrow::Borrow;
use std::path::Path;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file_path = Path::new(&args[1]);

    if file_path.is_dir() {
        do_dir(file_path)
    } else {
        do_file(file_path)
    }
}

fn do_dir(path: &Path) {
    let dir_contents = std::fs::read_dir(path).unwrap();
    for file in dir_contents {
        let file = match file {
            Err(err) => {
                eprintln!("Skipping a file: {:?}", err);
                continue;
            }
            Ok(file) => file,
        };

        match file.path().extension() {
            None => {
                println!("Skipping {}", file.path().to_string_lossy());
            }
            Some(path) => match path.to_string_lossy().borrow() {
                "wast" => do_file(&file.path()),
                _ => println!("Skipping {}", file.path().to_string_lossy()),
            },
        }
    }
}

fn do_file(path: &Path) {
    println!("{}", path.to_string_lossy());

    let contents = std::fs::read_to_string(path).unwrap();
    let lexer = Lexer::new(&contents);
    let parser = TestFileParser::new();

    match parser.parse(lexer.into_iter()) {
        Err(parse_error) => println!("{:?}", parse_error),
        Ok(ast) => println!("{:?}", ast),
    }
}
