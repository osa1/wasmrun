use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};

/// Command line arguments.
#[derive(Debug)]
pub struct Args {
    /// Spec test files or directories. For directories, we run all .wast files in the directory.
    pub files: Vec<String>,
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
                .required(false)
                .multiple(true)
                .help("File or directory to run"),
        )
        .get_matches();

    Args {
        files: m
            .values_of("file")
            .unwrap_or_default()
            .map(|s| s.to_string())
            .collect(),
    }
}
