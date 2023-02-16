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

/// Reader that saves the last position.
pub struct Cursor<T> {
    inner: T,
    pos: usize,
}

impl<T> Cursor<T> {
    pub fn new(inner: T) -> Cursor<T> {
        Cursor { inner, pos: 0 }
    }

    pub fn position(&self) -> usize {
        self.pos
    }
}

impl<T: AsRef<[u8]>> Read for Cursor<T> {
    fn read(&mut self, buf: &mut [u8]) -> Result<()> {
        let slice = self.inner.as_ref();
        let remainder = slice.len() - self.pos;
        let requested = buf.len();
        if requested > remainder {
            return Err(Error::UnexpectedEof);
        }
        buf.copy_from_slice(&slice[self.pos..(self.pos + requested)]);
        self.pos += requested;
        Ok(())
    }
}

impl<T: io::Read> Read for T {
    fn read(&mut self, buf: &mut [u8]) -> Result<()> {
        self.read_exact(buf).map_err(Error::Io)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cursor() {
        let mut cursor = Cursor::new(vec![0xFFu8, 0x7Fu8]);
        assert_eq!(cursor.position(), 0);

        let mut buf = [0u8];
        assert!(cursor.read(&mut buf[..]).is_ok());
        assert_eq!(cursor.position(), 1);
        assert_eq!(buf[0], 0xFFu8);
        assert!(cursor.read(&mut buf[..]).is_ok());
        assert_eq!(buf[0], 0x7Fu8);
        assert_eq!(cursor.position(), 2);
    }

    #[test]
    fn overflow_in_cursor() {
        let mut cursor = Cursor::new(vec![0u8]);
        let mut buf = [0, 1, 2];
        assert!(cursor.read(&mut buf[..]).is_err());
    }
}
