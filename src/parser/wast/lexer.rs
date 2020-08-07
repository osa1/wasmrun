#![allow(dead_code)]

use std::convert::TryFrom;

#[derive(Debug)]
pub enum Token {
    Id(String),
    String(String),
    LParen,
    RParen,
    Keyword(String),
    Reserved(String),
    Integer(Sign, u64),
    // Uninterpreted floats
    Float {
        hex: bool,
        // Float part before the `.`
        integral: u64,
        // Float part after the `.`
        decimal: f64,
        // The part after 'E' or 'e'. Exponent of `integral.decimal`. Either 2^exponent or
        // 10^exponent.
        exponent: i64,
    },
}

#[derive(Debug)]
pub enum Sign {
    Pos,
    Neg,
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
    NonTerminatedComment,
    NonTerminatedNumber,
    InvalidEscapeSequence,
    InvalidUnicodeValue,
    InvalidStringChar,
    InvalidHexNumber,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a [u8]) -> Lexer<'a> {
        Lexer { buf, cursor: 0 }
    }

    pub fn next(&mut self) -> Option<Result<Token, LexerError>> {
        if self.cursor >= self.buf.len() {
            return None;
        }

        let tok = loop {
            if self.cursor >= self.buf.len() {
                return None;
            }

            match self.buf[self.cursor] {
                b' ' | b'\t' | b'\n' => {
                    self.cursor += 1;
                }
                b';' => {
                    self.cursor += 1;
                    if self.cursor >= self.buf.len() || self.buf[self.cursor] != b';' {
                        return Some(Err(LexerError::NonTerminatedComment));
                    }
                    self.cursor += 1;
                    if let Err(err) = self.skip_line_comment() {
                        return Some(Err(err));
                    }
                }
                b'(' => {
                    self.cursor += 1;
                    if self.cursor >= self.buf.len() {
                        break Ok(Token::LParen);
                    }
                    if self.buf[self.cursor] == b';' {
                        self.cursor += 1;
                        if let Err(err) = self.skip_block_comment() {
                            return Some(Err(err));
                        }
                    } else {
                        break Ok(Token::LParen);
                    }
                }
                b')' => {
                    self.cursor += 1;
                    break Ok(Token::RParen);
                }
                b'"' => {
                    break self.string().map(Token::String);
                }
                b'$' => {
                    break self.id().map(Token::Id);
                }
                b'+' => {
                    self.cursor += 1;
                    break self.int_or_float(Sign::Pos);
                }
                b'-' => {
                    self.cursor += 1;
                    break self.int_or_float(Sign::Neg);
                }
                b if b.is_ascii_digit() => {
                    break self.int_or_float(Sign::Pos);
                }
                b if b >= b'a' && b <= b'z' => {
                    break self.keyword_or_reserved();
                }
                other => todo!("{}", char::from(other)),
            }
        };

        Some(tok)
    }

    fn keyword_or_reserved(&mut self) -> Result<Token, LexerError> {
        let mut str = String::with_capacity(10);

        str.push(char::from(self.buf[self.cursor]));
        self.cursor += 1;

        while self.cursor < self.buf.len() && is_id_char(self.buf[self.cursor]) {
            str.push(char::from(self.buf[self.cursor]));
            self.cursor += 1;
        }

        Ok(Token::Reserved(str)) // TODO
    }

    fn skip_block_comment(&mut self) -> Result<(), LexerError> {
        loop {
            if self.cursor >= self.buf.len() {
                return Err(LexerError::NonTerminatedComment);
            }

            match self.buf[self.cursor] {
                b'(' => {
                    self.cursor += 1; // '('
                    if self.cursor < self.buf.len() && self.buf[self.cursor] == b';' {
                        self.cursor += 1; // ';'
                        self.skip_block_comment()?;
                    }
                }
                b';' => {
                    self.cursor += 1; // ';'
                    if self.cursor < self.buf.len() && self.buf[self.cursor] == b')' {
                        self.cursor += 1; // ')'
                        return Ok(());
                    }
                }
                _ => {
                    self.cursor += 1;
                }
            }
        }
    }

    fn skip_line_comment(&mut self) -> Result<(), LexerError> {
        while self.cursor < self.buf.len() {
            let b = self.buf[self.cursor];
            self.cursor += 1;
            if b == b'\n' {
                break;
            }
        }
        Ok(())
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

    // Parse a sign + float or integer. Sign is consumed. Hex or not is not known.
    fn int_or_float(&mut self, sign: Sign) -> Result<Token, LexerError> {
        if self.cursor >= self.buf.len() {
            return Err(LexerError::NonTerminatedNumber);
        }

        let mut hex = false;

        if self.buf[self.cursor] == b'0'
            && (self.cursor + 1 < self.buf.len())
            && self.buf[self.cursor + 1] == b'x'
        {
            self.cursor += 2; // '0x'
            hex = true;
        }

        let num = self.num(hex)?;

        let mut float = false;
        if self.cursor < self.buf.len() && self.buf[self.cursor] == b'.' {
            self.cursor += 1;
            float = true;
        }

        // 1. 'num' '.'? ^
        // 2. 'num' '.'  ^ 'frac'
        // 3. 'num' '.'? ^ ('E'|'e') 'sign' 'num'
        // 4. 'num' '.'  ^ 'frac' ('E'|'e') 'sign' 'num'
        //               ^
        //             cursor
        //
        // Also handle the hex variant where 'E' is 'P'
        //
        // NB. Not sure how to distinguish (1) without a '.' from an integer

        if self.cursor < self.buf.len() {
            // We may see 'e' or 'E' even without a dot before in (3)
            match self.exp_opt(hex)? {
                Some(exponent) => {
                    // (3)
                    return Ok(Token::Float {
                        hex,
                        integral: num,
                        decimal: 0f64,
                        exponent,
                    });
                }
                None => {
                    // Not (3), but should be (2) or (4) or we've seen a '.'
                    if float {
                        // (2) or (4)
                        let decimal = self.frac(hex)?;
                        match self.exp_opt(hex)? {
                            Some(exponent) => {
                                return Ok(Token::Float {
                                    hex,
                                    integral: num,
                                    decimal,
                                    exponent,
                                });
                            }
                            None => {
                                // (2)
                                return Ok(Token::Float {
                                    hex,
                                    integral: num,
                                    decimal,
                                    exponent: 0,
                                });
                            }
                        }
                    }
                }
            }
        }

        Ok(Token::Integer(sign, num))
    }

    fn exp_opt(&mut self, hex: bool) -> Result<Option<i64>, LexerError> {
        if self.cursor >= self.buf.len() {
            return Ok(None);
        }

        let c = self.buf[self.cursor];
        if c == b'E' || c == b'e' || c == b'P' || c == b'p' {
            if (hex && (c != b'P' && c != b'p')) || (!hex && (c != b'E' || c != b'e')) {
                return Err(LexerError::InvalidHexNumber);
            }

            self.cursor += 1;
            let exp_sign = self.sign();
            let exp_sign = match exp_sign {
                Sign::Pos => 1,
                Sign::Neg => -1,
            };
            let exp_num = self.num(hex)?;

            Ok(Some(exp_sign * exp_num as i64))
        } else {
            Ok(None)
        }
    }

    fn sign(&mut self) -> Sign {
        let c = self.buf[self.cursor];
        if c == b'+' {
            self.cursor += 1;
            Sign::Pos
        } else if c == b'-' {
            self.cursor += 1;
            Sign::Neg
        } else {
            Sign::Pos
        }
    }

    fn frac(&mut self, hex: bool) -> Result<f64, LexerError> {
        let range_begin = self.cursor;
        let mut range_end = self.cursor;

        while range_end < self.buf.len() {
            let b = self.buf[range_end];
            if (hex && b.is_ascii_hexdigit()) || b.is_ascii_digit() || b == b'_' {
                range_end += 1;
            } else {
                break;
            }
        }

        self.cursor = range_end;

        let mut ret = 0f64;
        let m = if hex { 16f64 } else { 10f64 };
        for i in (range_begin..range_end - 1).rev() {
            let b = self.buf[i];
            if (hex && b.is_ascii_hexdigit()) || b.is_ascii_digit() {
                let f = f64::from(b - b'0');
                ret = (f + (ret / m)) / m;
            } else if b == b'_' {
                continue;
            } else {
                panic!("Unexpected character in reverse scan: {}", char::from(b));
            }
        }

        Ok(ret)
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

    fn num(&mut self, hex: bool) -> Result<u64, LexerError> {
        let mut ret = 0;
        let m = if hex { 16 } else { 10 };

        while self.cursor < self.buf.len() {
            let b = self.buf[self.cursor];
            if (hex && b.is_ascii_hexdigit()) || b.is_ascii_digit() {
                ret *= m;
                ret += u64::from(if hex { hex_value(b) } else { b - b'0' });
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

#[test]
fn parse_float_1() {
    let mut lexer = Lexer::new("0x1.fffffep+127".as_bytes());

    match lexer.next() {
        Some(Ok(Token::Float { .. })) => {}
        other => panic!("{:?}", other),
    }

    match lexer.next() {
        None => {}
        other => panic!("{:?}", other),
    }
}
