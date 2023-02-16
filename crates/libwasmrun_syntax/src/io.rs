//! Simple abstractions for the IO operations.
//!
//! Basically it just a replacement for the std::io that is usable from
//! the `no_std` environment.

use std::io;

pub(crate) fn buffered_read<R: Read, const BUFFER_SIZE: usize>(
    read_size: usize,
    reader: &mut R,
) -> Result<Vec<u8>> {
    let mut vec_buf = Vec::new();
    let mut total_read = 0;
    let mut buf = [0u8; BUFFER_SIZE];
    while total_read < read_size {
        let next_to_read = if read_size - total_read > BUFFER_SIZE {
            BUFFER_SIZE
        } else {
            read_size - total_read
        };
        reader.read(&mut buf[0..next_to_read])?;
        vec_buf.extend_from_slice(&buf[0..next_to_read]);
        total_read += next_to_read;
    }
    Ok(vec_buf)
}

/// IO specific error.
#[derive(Debug)]
pub enum Error {
    /// Some unexpected data left in the buffer after reading all data.
    TrailingData,

    /// Unexpected End-Of-File
    UnexpectedEof,

    /// Invalid data is encountered.
    InvalidData,

    Io(std::io::Error),
}

/// IO specific Result.
pub type Result<T> = core::result::Result<T, Error>;

pub trait Read {
    /// Read a data from this read to a buffer.
    ///
    /// If there is not enough data in this read then `UnexpectedEof` will be returned.
    fn read(&mut self, buf: &mut [u8]) -> Result<()>;
}

impl<T: io::Read> Read for T {
    fn read(&mut self, buf: &mut [u8]) -> Result<()> {
        self.read_exact(buf).map_err(Error::Io)
    }
}
