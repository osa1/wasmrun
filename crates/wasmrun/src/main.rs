mod cli;

use cli::Args;

fn main() {
    let Args {
        file,
        program_args: _,
    } = cli::parse();

    match libwasmrun::run_wasm(file, vec![]) {
        Ok(()) => {}
        Err(err) => {
            println!("{}", err);
            ::std::process::exit(1);
        }
    }
}
