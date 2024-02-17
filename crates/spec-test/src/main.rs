mod cli;
mod spec;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::exit;

#[rustfmt::skip]
static TEST_FILES: [&str; 7] = [
    "tests/spec",
    "tests/spec/proposals/exception-handling",
    "tests/spec/proposals/extended-const",
    "tests/spec/proposals/function-references",
    "tests/spec/proposals/multi-memory",

    // Tail call tests take a long time to run, disabled for now
    // "tests/spec/proposals/tail-call",

    "tests/spec/proposals/gc/struct.wast",
    "tests/spec/proposals/gc/i31.wast",
];

fn main() {
    let cli::Args { mut files } = cli::parse();

    if files.is_empty() {
        files = TEST_FILES.iter().map(|s| s.to_string()).collect();
    }

    let mut fails = 0;

    for file in files {
        fails += run_file_or_dir(&file);
    }

    if fails != 0 {
        exit(1);
    }
}

fn run_file_or_dir(file_or_dir: &str) -> usize {
    let file_meta = fs::metadata(file_or_dir).expect("Unable to get input file or dir metadata");

    if file_meta.is_dir() {
        run_dir(file_or_dir)
    } else if file_meta.is_file() {
        run_file(file_or_dir)
    } else {
        eprintln!("Input file {} is not a directory or file", file_or_dir);
        1
    }
}

fn run_dir(dir_path: &str) -> usize {
    let dir_contents = fs::read_dir(dir_path).expect("Unable to get dir contents");

    let mut dir_files: Vec<PathBuf> = vec![];

    for file in dir_contents {
        let file = file.unwrap();
        let file_path = file.path();
        if let Some(ext) = file_path.extension() {
            if ext == "wast" {
                dir_files.push(file_path);
            }
        }
    }

    dir_files.sort();

    let fails = run_spec_dir(&dir_files);

    println!();

    let mut total_fails = 0;
    for (file, lines) in fails.iter() {
        println!("{}: {:?}", file.to_string_lossy(), lines);
        total_fails += lines.len();
    }
    if total_fails != 0 {
        println!("Total fails: {}", total_fails);
    }

    total_fails
}

fn run_file(file_path: &str) -> usize {
    let file_path: PathBuf = file_path.into();

    println!("{}", file_path.file_name().unwrap().to_str().unwrap());

    match file_path.extension() {
        Some(ext) => {
            if ext != "wast" {
                println!("Spec test extension should be .wast, found: {:?}", ext);
                return 1;
            }
        }
        None => {
            println!("Spec file should have .wast extension");
            return 1;
        }
    }

    match run_spec_test(&file_path) {
        Ok(fails) => {
            if fails.is_empty() {
                0
            } else {
                println!("{:?}", fails);
                1
            }
        }
        Err(err) => {
            println!("{}", err);
            1
        }
    }
}

/// Run all .wast files in the given directory. Does not recurse into subdirectories.
fn run_spec_dir(dir: &[PathBuf]) -> Vec<(PathBuf, Vec<usize>)> {
    let mut fails: Vec<(PathBuf, Vec<usize>)> = Default::default();

    for file_path in dir {
        if let Some(ext) = file_path.extension() {
            if ext == "wast" {
                println!("{}", file_path.file_name().unwrap().to_str().unwrap());

                // TODO: Why does this not call `run_file`?

                match run_spec_test(file_path) {
                    Ok(failing_lines) => {
                        if !failing_lines.is_empty() {
                            fails.push((file_path.to_owned(), failing_lines));
                        }
                    }
                    Err(err) => {
                        println!("{}", err);
                        fails.push((file_path.to_owned(), vec![]));
                    }
                }
            }
        }
    }

    fails
}

/// Run a single .wast file
fn run_spec_test(path: &Path) -> Result<Vec<usize>, String> {
    let wast = std::fs::read_to_string(path)
        .map_err(|err| format!("Unable to read wast file: {:?}", err))?;

    let adjust_wast = |mut err: wast::Error| {
        err.set_path(path.as_ref());
        err.set_text(&wast);
        err
    };

    let mut lexer = wast::lexer::Lexer::new(&wast);
    lexer.allow_confusing_unicode(path.ends_with("names.wast"));

    // wast parser expects at least one `(module ...)` and cannot handle empty files. Skip empty
    // files.
    let not_all_ws = lexer.iter(0).any(|token| {
        !matches!(
            token,
            Ok(wast::lexer::Token {
                kind: wast::lexer::TokenKind::LineComment
                    | wast::lexer::TokenKind::BlockComment
                    | wast::lexer::TokenKind::Whitespace,
                ..
            })
        )
    });
    if !not_all_ws {
        println!("  Empty file");
        return Ok(vec![]);
    }

    let buf = wast::parser::ParseBuffer::new_with_lexer(lexer)
        .map_err(adjust_wast)
        .map_err(|err| format!("Unable to tokenize wast file: {:?}", err))?;

    let ast = wast::parser::parse::<wast::Wast>(&buf)
        .map_err(adjust_wast)
        .map_err(|err| format!("Unable to parse wast file: {:?}", err))?;

    let mut test_runner = spec::TestFileRunner::new(&wast);

    for directive in ast.directives {
        test_runner.run_directive(directive);
    }

    Ok(test_runner.failing_lines)
}
