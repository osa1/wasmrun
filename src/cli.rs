use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};

/// Command line arguments.
#[derive(Debug)]
pub struct Args {
    /// File to run
    pub file: String,

    /// Whether the file is a WebAssembly spec test
    pub spec_test: bool,
}

/// Parses command line arguments.
pub(crate) fn parse() -> Args {
    let mut version = crate_version!().to_owned();
    let commit_hash = env!("GIT_HASH");
    if !commit_hash.is_empty() {
        version = format!("{} ({})", version, commit_hash);
    }

    let m = App::new(crate_name!())
        .version(version.as_str())
        .about(crate_description!())
        .author(crate_authors!())
        .arg(
            Arg::with_name("file")
                .required(true)
                .multiple(false)
                .help("File to run"),
        )
        .arg(
            Arg::with_name("spec_test")
                .takes_value(false)
                .short("s")
                .long("spec-test")
                .required(false)
                .help("Run a WebAssembly spec test. Requires wast2json from wabt."),
        )
        .get_matches();

    Args {
        file: m.value_of("file").unwrap().to_string(),
        spec_test: m.is_present("spec_test"),
    }
}
