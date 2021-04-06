//! Lexes all .wast files in given dir, or lexes the given file if the argument is a file

use libwasmrun_parse::lexer::Token;

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

        do_file(&file.path());
    }
}

fn do_file(path: &Path) {
    println!("{}", path.to_string_lossy());

    let contents = std::fs::read_to_string(path).unwrap();
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
            Ok(Some(_)) => {
                // println!("{:?}", token);
            }
        }
    }
}
