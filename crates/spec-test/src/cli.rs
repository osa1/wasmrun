use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};

/// Command line arguments.
#[derive(Debug)]
pub struct Args {
    /// Spec test file or directory. When this is a directory we assume run all .wast files in the
    /// directory and compare the output with the expected output (if a reference file exists).
    pub file: String,
    /// Update the reference test output file. Only used when running multiple .wast files.
    pub accept: bool,
}

/// Parses command line arguments.
pub fn parse() -> Args {
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
        .arg(Arg::with_name("accept").required(false).long("accept"))
        .get_matches();

    Args {
        file: m.value_of("file").unwrap().to_string(),
        accept: m.is_present("accept"),
    }
}
