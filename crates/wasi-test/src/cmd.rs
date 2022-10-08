use std::str::FromStr;

#[derive(Debug)]
pub(crate) enum Cmd {
    Preopen {
        /// Path of the file in Wasm program
        wasm_path: String,
        /// Actual path of the file in the file system, relative to the test source code
        file_path: String,
    },
    ExitCode(i32),
}

pub(crate) fn parse_cmds(contents: &str) -> Vec<Cmd> {
    let mut ret = vec![];

    for line in contents.lines() {
        let line = line.trim();
        if let Some(line) = line.strip_prefix("//") {
            let words = line.split_whitespace().collect::<Vec<_>>();
            match words[0] {
                "wasm-preopen" => {
                    let wasm_path = words[1].to_owned();
                    let file_path = words[2].to_owned();
                    ret.push(Cmd::Preopen {
                        wasm_path,
                        file_path,
                    });
                }
                "wasm-exit" => {
                    ret.push(Cmd::ExitCode(i32::from_str(words[1]).unwrap()));
                }
                _ => {
                    continue;
                }
            }
        }
    }

    ret
}
