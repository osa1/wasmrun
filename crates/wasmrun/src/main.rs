mod cli;

use cli::Args;

fn main() {
    let Args { file } = cli::parse();
    // TODO: program args?
    match libwasmrun::run_wasm(file, vec![]) {
        Ok(()) => {}
        Err(err) => {
            println!("{}", err);
            ::std::process::exit(1);
        }
    }
}
