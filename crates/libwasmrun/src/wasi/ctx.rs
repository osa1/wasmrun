use super::file::FileOrDir;

use std::ffi::CString;
use std::fs::File;
use std::os::unix::io::{FromRawFd, RawFd};

use fxhash::FxHashMap;

#[derive(Debug, Default)]
pub struct WasiCtx {
    pub(crate) args: Vec<CString>,
    pub(crate) preopened: FxHashMap<String, FileOrDir>,
    pub(crate) stdout: Option<File>,
    pub(crate) stderr: Option<File>,
}

#[derive(Debug, Default)]
pub struct WasiCtxBuilder {
    args: Vec<CString>,
    preopened: FxHashMap<String, FileOrDir>,
    stdout: Option<RawFd>,
    stderr: Option<RawFd>,
}

impl WasiCtxBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn build(self) -> WasiCtx {
        let WasiCtxBuilder {
            args,
            preopened,
            stdout,
            stderr,
        } = self;

        let stdout = stdout.map(|fd| unsafe { File::from_raw_fd(fd) });
        let stderr = stderr.map(|fd| unsafe { File::from_raw_fd(fd) });

        WasiCtx {
            args,
            preopened,
            stdout,
            stderr,
        }
    }

    pub fn set_args(&mut self, args: Vec<CString>) -> &mut Self {
        self.args = args;
        self
    }

    pub fn stdout(&mut self, fd: RawFd) -> &mut Self {
        self.stdout = Some(fd);
        self
    }

    pub fn stderr(&mut self, fd: RawFd) -> &mut Self {
        self.stderr = Some(fd);
        self
    }

    pub fn preopen(&mut self, path: String, file: FileOrDir) -> &mut Self {
        self.preopened.insert(path, file);
        self
    }
}
