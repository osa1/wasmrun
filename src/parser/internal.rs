#[derive(Debug)]
pub enum ParseError {
    NotEnoughBytes { expected: usize, found: usize },
    UnexpectedConst { expected: Vec<u8>, found: Vec<u8> },
    UnexpectedValType { found: u8 },
    SectionNotEmpty { remains: Vec<u8> },
    Utf8Error(::std::str::Utf8Error),
    UnexpectedSectionType { expected: u8, found: u8 },
    UnexpectedOpCode(u8),
}

pub type Result<A> = ::std::result::Result<A, ParseError>;

#[derive(Debug)]
pub struct Parser<'a> {
    bytes: &'a [u8],
}

impl<'a> Parser<'a> {
    pub fn new(bytes: &'a [u8]) -> Parser<'a> {
        Parser { bytes }
    }

    pub fn get_bytes(&self) -> &[u8] {
        self.bytes
    }

    pub fn consume(&mut self, n: usize) -> Result<&'a [u8]> {
        let len = self.bytes.len();
        if len >= n {
            let (consumed, rest) = self.bytes.split_at(n);
            self.bytes = rest;
            Ok(consumed)
        } else {
            Err(ParseError::NotEnoughBytes {
                expected: n,
                found: len,
            })
        }
    }

    pub fn skip(&mut self, n: usize) -> Result<()> {
        let _ = self.consume(n)?;
        Ok(())
    }

    pub fn consume_const<'b>(&mut self, expect: &'b [u8]) -> Result<()> {
        let slice = self.consume(expect.len())?;
        if slice == expect {
            Ok(())
        } else {
            Err(ParseError::UnexpectedConst {
                expected: expect.to_owned(),
                found: slice.to_owned(),
            })
        }
    }

    /// Decode an unsigned LEB128 value
    pub fn consume_uleb128(&mut self) -> Result<u64> {
        let mut result = 0;
        let mut shift = 0;

        loop {
            let byte = self.consume_byte()?;
            result |= (u64::from(byte & 0b0111_1111)) << shift;
            if byte & 0b1000_0000 == 0 {
                break;
            }
            shift += 7;
        }

        Ok(result)
    }

    /// Decode a signed LEB128 value
    pub fn consume_sleb128(&mut self) -> Result<i64> {
        let mut result = 0;
        let mut shift = 0;

        let size = 33;

        let mut byte = self.consume_byte()?;
        loop {
            result |= (i64::from(byte & 0b0111_1111)) << shift;
            if byte & 0b1000_0000 == 0 {
                break;
            }
            shift += 7;
            byte = self.consume_byte()?;
        }

        if shift < size && byte & 0b0100_0000 != 0 {
            // or 0x40
            result |= !0 << shift;
        }

        Ok(result)
    }

    /// Read one byte without consuming.
    pub fn byte(&self) -> Result<u8> {
        match self.bytes.get(0) {
            None => Err(ParseError::NotEnoughBytes {
                expected: 1,
                found: 0,
            }),
            Some(byte) => Ok(*byte),
        }
    }

    pub fn consume_byte(&mut self) -> Result<u8> {
        match self.bytes.get(0) {
            None => Err(ParseError::NotEnoughBytes {
                expected: 1,
                found: 0,
            }),
            Some(byte) => {
                let byte = *byte;
                self.bytes = &self.bytes[1..];
                Ok(byte)
            }
        }
    }

    pub fn all_consumed(&self) -> bool {
        self.bytes.is_empty()
    }
}
