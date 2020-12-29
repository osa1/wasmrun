use clap::{crate_authors, crate_description, crate_name, crate_version, App, AppSettings, Arg};

/// Command line arguments.
#[derive(Debug)]
pub struct Args {
    /// File to run
    pub file: String,
    /// Arguments to pass to the WASI app
    pub program_args: Vec<String>,
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
        .setting(AppSettings::TrailingVarArg)
        .arg(
            Arg::with_name("file")
                .required(true)
                .multiple(false)
                .help("File to run"),
        )
        .arg(Arg::with_name("program_args").multiple(true))
        .get_matches();

    Args {
        file: m.value_of("file").unwrap().to_string(),
        program_args: match m.values_of("program_args") {
            None => vec![],
            Some(vals) => vals.map(str::to_owned).collect(),
        },
    }
}
