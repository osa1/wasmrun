use clap::{crate_authors, crate_description, crate_name, crate_version, App, Arg};

#[derive(Debug)]
pub struct Args {
    pub file: Option<String>,
}

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
                .multiple(false)
                .help("File to run"),
        )
        .get_matches();

    Args {
        file: m.value_of("file").map(str::to_string),
    }
}
