#![allow(dead_code)]

use std::convert::TryFrom;

#[derive(Debug)]
pub enum Token {
    Id(String),
}

pub struct Lexer<'a> {
    buf: &'a [u8],
    cursor: usize,
}

#[derive(Debug)]
pub enum LexerError {
    /// Identifier not terminated (i.e. EOF after '$')
    NonTerminatedId,
    /// Identifier is empty (i.e. a single '$' character)
    EmptyId,
    NonTerminatedString,
    InvalidEscapeSequence,
    InvalidUnicodeValue,
    InvalidStringChar,
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a [u8]) -> Lexer<'a> {
        Lexer { buf, cursor: 0 }
    }

    fn id(&mut self) -> Result<String, LexerError> {
        debug_assert_eq!(self.buf[self.cursor], b'$');
        self.cursor += 1;

        let mut id = String::with_capacity(10);

        if self.cursor >= self.buf.len() {
            return Err(LexerError::NonTerminatedId);
        }

        while self.cursor < self.buf.len() && is_id_char(self.buf[self.cursor]) {
            id.push(char::from(self.buf[self.cursor]));
            self.cursor += 1;
        }

        if id.is_empty() {
            return Err(LexerError::EmptyId);
        }

        Ok(id)
    }

    fn string(&mut self) -> Result<String, LexerError> {
        debug_assert_eq!(self.buf[self.cursor], b'"');
        self.cursor += 1;

        let mut str = String::with_capacity(10);

        if self.cursor >= self.buf.len() {
            return Err(LexerError::NonTerminatedString);
        }

        while self.buf[self.cursor] != b'"' {
            let b = self.buf[self.cursor];
            self.cursor += 1;
            if b >= 0x20 && b != 0x7F && b != b'"' && b != b'\\' {
                str.push(char::from(b));
            } else if b == b'\\' {
                if self.cursor >= self.buf.len() {
                    return Err(LexerError::NonTerminatedString);
                }
                let b = self.buf[self.cursor];
                self.cursor += 1;
                match b {
                    b't' => {
                        str.push('\t');
                    }
                    b'n' => {
                        str.push('\n');
                    }
                    b'r' => {
                        str.push('\r');
                    }
                    b'"' => {
                        str.push('"');
                    }
                    b'\'' => {
                        str.push('\'');
                    }
                    b'\\' => {
                        str.push('\\');
                    }
                    b'u' => {
                        if self.cursor >= self.buf.len() {
                            return Err(LexerError::NonTerminatedString);
                        }
                        if self.buf[self.cursor] != b'{' {
                            return Err(LexerError::NonTerminatedString);
                        }
                        self.cursor += 1;
                        let num = self.hexnum()?;
                        let char = u32::try_from(num)
                            .map_err(|_| LexerError::InvalidUnicodeValue)
                            .and_then(|u32| {
                                char::try_from(u32).map_err(|_| LexerError::InvalidUnicodeValue)
                            })?;
                        str.push(char);
                        if self.cursor >= self.buf.len() || self.buf[self.cursor] != b'}' {
                            return Err(LexerError::NonTerminatedString);
                        }
                        self.cursor += 1;
                    }
                    b1 if b1.is_ascii_hexdigit() => {
                        if self.cursor >= self.buf.len() {
                            return Err(LexerError::NonTerminatedString);
                        }
                        let b2 = self.buf[self.cursor];
                        self.cursor += 1;
                        match char::try_from(
                            u32::from(hex_value(b1)) * 16 + u32::from(hex_value(b2)),
                        ) {
                            Ok(char) => {
                                str.push(char);
                            }
                            Err(_) => {
                                return Err(LexerError::InvalidUnicodeValue);
                            }
                        }
                    }
                    _ => {
                        return Err(LexerError::InvalidEscapeSequence);
                    }
                }
            } else {
                return Err(LexerError::InvalidStringChar);
            }
        }

        debug_assert_eq!(self.buf[self.cursor], b'"');
        self.cursor += 1;
        Ok(str)
    }

    fn hexnum(&mut self) -> Result<u64, LexerError> {
        let mut ret = 0;

        while self.cursor < self.buf.len() {
            let b = self.buf[self.cursor];
            if b.is_ascii_hexdigit() {
                ret *= 16;
                ret += u64::from(hex_value(b));
                self.cursor += 1;
            } else if b == b'_' {
                self.cursor += 1;
            } else {
                break;
            }
        }

        Ok(ret)
    }
}

fn is_id_char(c: u8) -> bool {
    c >= 33 // '!', excludes space as well
        && c != b'"'
        && c != b'('
        && c != b')'
        && c != b';'
        && c != b'['
        && c != b']'
        && c != b'{'
        && c != b'}'
        && c != 127 // DEL
}

fn hex_value(c: u8) -> u8 {
    if c >= b'0' && c <= b'9' {
        c - b'0'
    } else if c >= b'A' && c <= b'F' {
        c - b'A' + 10
    } else {
        debug_assert!(c >= b'a' && c <= b'f');
        c - b'a' + 10
    }
}

#[test]
fn parse_id() {
    let mut lexer = Lexer::new("$i32_blah_blah".as_bytes());
    assert_eq!(lexer.id().unwrap(), "i32_blah_blah".to_string());
}

#[test]
fn parse_string() {
    let mut lexer = Lexer::new("\"test\"".as_bytes());
    assert_eq!(lexer.string().unwrap(), "test".to_string());
}

#[test]
fn parse_hexnum() {
    let mut lexer = Lexer::new("12_AB".as_bytes());
    assert_eq!(lexer.hexnum().unwrap(), 4779);
}
