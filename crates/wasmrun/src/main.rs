mod cli;

use cli::Args;

fn main() {
    let Args { file } = cli::parse();
    match libwasmrun::run_wasm(file) {
        Ok(()) => {}
        Err(err) => {
            println!("{}", err);
            ::std::process::exit(1);
        }
    }
}
