use std::fs;
use std::path::PathBuf;

use fxhash::FxHashMap;

#[derive(Debug)]
pub struct Dir {
    pub(crate) files: FxHashMap<String, FileOrDir>,
}

#[derive(Debug)]
pub enum File {
    Pending(PathBuf),
    Open(fs::File),
}

#[derive(Debug)]
pub enum FileOrDir {
    File(File),
    Dir(FxHashMap<String, FileOrDir>),
}
