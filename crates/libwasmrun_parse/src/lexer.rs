use libwasmrun_parse_macros::make_enum;

use std::convert::TryFrom;
use std::iter::Peekable;
use std::str::CharIndices;

// Byte index
type Loc = usize;

type Spanned<A> = Result<(Loc, A, Loc), LexerError>;

#[derive(Debug, PartialEq)]
pub enum Token {
    /// Left parenthesis (`(`)
    LParen,

    /// Right parenthesis (`)`)
    RParen,

    /// `=`, used in named parameters, e.g. `(i32.load offset=25 align=4 ...)`
    Eq,

    /// String literal
    String(String),

    /// Integer literal
    Int(i64),

    /// Float literal
    Float(f64),

    /// Variable
    Var(String),

    /// Instructions
    Instr(Instr),

    /// Keywords
    Keyword(Keyword),
}

#[derive(Debug)]
pub enum LexerError {
    UnterminatedString(usize),
    EmptyVar(usize),
    UnknownToken(usize),
    InvalidIntDigit(usize),
    CantParseInt(String),
    CantParseFloat(String),
    UnexpectedEOF(usize),
}

pub struct Lexer<'input> {
    iter: Peekable<CharIndices<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Lexer<'input> {
        Lexer {
            iter: input.char_indices().peekable(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.peek().copied() {
                None => return None,
                Some((idx, char)) => match char {
                    _ if char.is_whitespace() => {
                        let _ = self.iter.next();
                        continue;
                    }
                    _ if char.is_ascii_digit() => {
                        return Some(parse_int_or_float(&mut self.iter, false));
                    }
                    '-' => {
                        let _ = self.iter.next();
                        return Some(parse_int_or_float(&mut self.iter, true));
                    }
                    ';' => {
                        let _ = self.iter.next();
                        loop {
                            match self.iter.next() {
                                None => return None,
                                Some((_, '\n')) => break,
                                Some((_, _)) => continue,
                            }
                        }
                    }
                    '(' => {
                        let _ = self.iter.next();
                        return Some(Ok((idx, Token::LParen, '('.len_utf8())));
                    }
                    ')' => {
                        let _ = self.iter.next();
                        return Some(Ok((idx, Token::RParen, ')'.len_utf8())));
                    }
                    '=' => {
                        let _ = self.iter.next();
                        return Some(Ok((idx, Token::Eq, '='.len_utf8())));
                    }
                    '"' => {
                        let _ = self.iter.next();
                        return Some(
                            parse_string(&mut self.iter, idx)
                                .map(|(str, end)| (idx, Token::String(str), end)),
                        );
                    }
                    '$' => {
                        let _ = self.iter.next();
                        return Some(
                            parse_var(&mut self.iter, idx)
                                .map(|(str, end)| (idx, Token::Var(str), end)),
                        );
                    }
                    'n' => {
                        // `nan:...` notation. Handling this here only works because we don't have
                        // any instructions or keywords starting with 'na'.
                        let _ = self.iter.next();

                        macro_rules! skip_char {
                            ($c:expr) => {
                                match self.iter.next() {
                                    None => {
                                        return Some(Err(LexerError::UnexpectedEOF(
                                            idx + 'a'.len_utf8(),
                                        )));
                                    }
                                    Some((idx, c)) if c == $c => idx,
                                    Some(_) => {
                                        return Some(Err(LexerError::UnknownToken(idx)));
                                    }
                                }
                            };
                        }

                        match self.iter.next() {
                            None => {
                                return Some(Err(LexerError::UnexpectedEOF(idx + 'a'.len_utf8())));
                            }
                            Some((_, 'a')) => {
                                // nan:...
                                let _ = skip_char!('n');
                                let _ = skip_char!(':');
                                let _ = skip_char!('0');
                                let _ = skip_char!('x');

                                let (payload, end) = parse_hex(&mut self.iter);
                                // TODO: Check payload range
                                // TODO: IIRC we need transmute here and `as f64` doesn't work, but I don't
                                // remember why. Document it here.
                                let f: f64 =
                                    unsafe { std::mem::transmute((f64::NAN as u64) | payload) };
                                return Some(Ok((idx, Token::Float(f), end)));
                            }
                            Some((_, 'o')) => {
                                // 'nop' instruction
                                let end = skip_char!('p');
                                return Some(Ok((
                                    idx,
                                    Token::Instr(Instr::Nop),
                                    end + 'p'.len_utf8(),
                                )));
                            }
                            Some(_) => {
                                return Some(Err(LexerError::UnknownToken(idx)));
                            }
                        }
                    }
                    _ => {
                        // Keyword or instruction. Try instruction first
                        let mut instr_chars = self.iter.clone();
                        match Instr::parse(&mut instr_chars) {
                            Some((end, instr)) => {
                                self.iter = instr_chars;
                                return Some(Ok((idx, Token::Instr(instr), end)));
                            }
                            None => {
                                // Try keyword
                                let mut keyword_chars = self.iter.clone();
                                match Keyword::parse(&mut keyword_chars) {
                                    Some((end, kw)) => {
                                        self.iter = keyword_chars;
                                        return Some(Ok((idx, Token::Keyword(kw), end)));
                                    }
                                    None => return Some(Err(LexerError::UnknownToken(idx))),
                                }
                            }
                        }
                    }
                },
            }
        }
    }
}

/// Assumes that the initial '"' is consumed. Consumes the closing '"' and returns index after it.
fn parse_string(
    chars: &mut Peekable<CharIndices>,
    dquote_idx: usize,
) -> Result<(String, Loc), LexerError> {
    let mut ret = String::new();

    loop {
        match chars.next() {
            None => {
                return Err(LexerError::UnterminatedString(dquote_idx));
            }
            Some((idx, '"')) => {
                return Ok((ret, idx + '"'.len_utf8()));
            }
            Some((_, char)) => {
                ret.push(char);
            }
        }
    }
}

/// Assumes that the initial '$' is consumed.
fn parse_var(
    chars: &mut Peekable<CharIndices>,
    dollar_idx: usize,
) -> Result<(String, Loc), LexerError> {
    let mut ret = String::new();
    let mut last = 0;

    loop {
        match chars.next() {
            None => {
                if ret.is_empty() {
                    return Err(LexerError::EmptyVar(dollar_idx));
                } else {
                    assert!(last != 0);
                    return Ok((ret, last));
                }
            }
            Some((idx, char)) => {
                if char.is_whitespace() {
                    return Ok((ret, idx));
                } else {
                    last = idx + char.len_utf8();
                    ret.push(char);
                }
            }
        }
    }
}

fn parse_int_or_float(chars: &mut Peekable<CharIndices>, negate: bool) -> Spanned<Token> {
    let mut begin = 0;

    if let Some((begin_, '0')) = chars.peek().copied() {
        let _ = chars.next();
        if let Some((_, 'x')) = chars.peek().copied() {
            let _ = chars.next();
            let (u, end_idx) = parse_hex(chars);
            let i = u64::try_from(u).unwrap() as i64;
            return Ok((begin_, Token::Int(if negate { -i } else { i }), end_idx));
        } else {
            begin = begin_;
        }
    }

    let (dec, end_idx) = parse_dec(chars);

    if let Some((_, '.')) = chars.peek() {
        let _ = chars.next();
        let (float, end_idx) = parse_dec(chars);
        let str = format!("{}.{}", dec, float);
        match str.parse::<f64>() {
            Ok(f) => {
                let f = if negate { -f } else { f };
                Ok((begin, Token::Float(f), end_idx))
            }
            Err(_) => Err(LexerError::CantParseFloat(str)),
        }
    } else {
        let dec = i64::try_from(dec).unwrap();
        Ok((begin, Token::Int(if negate { -dec } else { dec }), end_idx))
    }
}

// Consumes at least one character
fn parse_dec(chars: &mut Peekable<CharIndices>) -> (u64, usize) {
    let mut i = 0;
    let mut end_idx = 0;

    loop {
        match chars.peek().copied() {
            None => {
                break;
            }
            Some((idx, char)) => {
                if char.is_ascii_digit() {
                    end_idx = idx + char.len_utf8();
                    let _ = chars.next();
                    i *= 10;
                    i += u64::from(char) - u64::from(b'0');
                } else {
                    break;
                }
            }
        }
    }

    (i, end_idx)
}

fn parse_hex(chars: &mut Peekable<CharIndices>) -> (u64, usize) {
    let mut i = 0;
    let mut end_idx = 0;

    loop {
        match chars.peek().copied() {
            None => {
                break;
            }
            Some((idx, char)) => {
                end_idx = idx + char.len_utf8();
                if char.is_ascii_hexdigit() {
                    let _ = chars.next();

                    i *= 16;

                    let c = u64::from(char);

                    let d = if c >= u64::from(b'a') && c <= u64::from(b'f') {
                        10 + c - u64::from(b'a')
                    } else if c >= u64::from(b'A') && c <= u64::from(b'F') {
                        10 + c - u64::from(b'A')
                    } else {
                        c - u64::from(b'0')
                    };

                    i += d
                } else {
                    break;
                }
            }
        }
    }

    assert!(end_idx != 0); // TODO: turn this in to a lexer error

    (i, end_idx)
}

make_enum! {
    Instr,
    "unreachable",
    "nop", // parsd with 'nan' in `Token::parse`
    "block",
    "loop",
    "if",
    "else",
    "end",
    "br",
    "br_if",
    "br_table",
    "return",
    "call",
    "call_indirect",
    "drop",
    "select",
    "local.get",
    "local.set",
    "local.tee",
    "global.get",
    "global.set",
    "table.get",
    "table.set",
    "i32.load",
    "i64.load",
    "f32.load",
    "f64.load",
    "i32.load8_s",
    "i32.load8_u",
    "i32.load16_s",
    "i32.load16_u",
    "i64.load8_s",
    "i64.load8_u",
    "i64.load16_s",
    "i64.load16_u",
    "i64.load32_s",
    "i64.load32_u",
    "i32.store",
    "i64.store",
    "f32.store",
    "f64.store",
    "i32.store8",
    "i32.store16",
    "i64.store8",
    "i64.store16",
    "i64.store32",
    "memory.size",
    "memory.grow",
    "i32.const",
    "i64.const",
    "f32.const",
    "f64.const",
    "i32.eqz",
    "i32.eq",
    "i32.ne",
    "i32.lt_s",
    "i32.lt_u",
    "i32.gt_s",
    "i32.gt_u",
    "i32.le_s",
    "i32.le_u",
    "i32.ge_s",
    "i32.ge_u",
    "i64.eqz",
    "i64.eq",
    "i64.ne",
    "i64.lt_s",
    "i64.lt_u",
    "i64.gt_s",
    "i64.gt_u",
    "i64.le_s",
    "i64.le_u",
    "i64.ge_s",
    "i64.ge_u",
    "f32.eq",
    "f32.ne",
    "f32.lt",
    "f32.gt",
    "f32.le",
    "f32.ge",
    "f64.eq",
    "f64.ne",
    "f64.lt",
    "f64.gt",
    "f64.le",
    "f64.ge",
    "i32.clz",
    "i32.ctz",
    "i32.popcnt",
    "i32.add",
    "i32.sub",
    "i32.mul",
    "i32.div_s",
    "i32.div_u",
    "i32.and",
    "i32.or",
    "i32.xor",
    "i32.shl",
    "i32.shr_s",
    "i32.shr_u",
    "i32.rotl",
    "i32.rotr",
    "i64.clz",
    "i64.ctz",
    "i64.popcnt",
    "i64.add",
    "i64.sub",
    "i64.mul",
    "i64.div_s",
    "i64.div_u",
    "i64.rem_s",
    "i64.rem_u",
    "i64.and",
    "i64.or",
    "i64.xor",
    "i64.shl",
    "i64.shr_s",
    "i64.shr_u",
    "i64.rotl",
    "i64.rotr",
    "f32.abs",
    "f32.neg",
    "f32.ceil",
    "f32.floor",
    "f32.trunc",
    "f32.nearest",
    "f32.sqrt",
    "f32.add",
    "f32.sub",
    "f32.mul",
    "f32.div",
    "f32.min",
    "f32.max",
    "f32.copysign",
    "f64.abs",
    "f64.neg",
    "f64.ceil",
    "f64.floor",
    "f64.trunc",
    "f64.nearest",
    "f64.sqrt",
    "f64.add",
    "f64.mul",
    "f64.div",
    "f64.min",
    "f64.max",
    "f64.copysign",
    "i32.wrap_i64",
    "i32.trunc_f32_s",
    "i32.trunc_f32_u",
    "i32.trunc_f64_s",
    "i32.trunc_f64_u",
    "i64.extend_i32_s",
    "i64.extend_i32_u",
    "i64.trunc_f32_s",
    "i64.trunc_f32_u",
    "i64.trunc_f64_s",
    "i64.trunc_f64_u",
    "f32.convert_i32_s",
    "f32.convert_i32_u",
    "f32.convert_i64_s",
    "f32.convert_i64_u",
    "f32.demote_f64",
    "f64.convert_i32_s",
    "f64.convert_i32_u",
    "f64.convert_i64_s",
    "f64.convert_i64_u",
    "f64.promote_f32",
    "i32.reinterpret_f32",
    "i64.reinterpret_f64",
    "f32.reinterpret_i32",
    "f64.reinterpret_i64",
    "i32.extend8_s",
    "i32.extend16_s",
    "i64.extend8_s",
    "i64.extend16_s",
    "i64.extend32_s",
    "ref.null",
    "ref.is_null",
    "ref.func",
    "i32.trunc_sat_f32_s",
    "i32.trunc_sat_f32_u",
    "i32.trunc_sat_f64_s",
    "i32.trunc_sat_f64_u",
    "i64.trunc_sat_f32_s",
    "i64.trunc_sat_f32_u",
    "i64.trunc_sat_f64_s",
    "i64.trunc_sat_f64_u",
    "memory.init",
    "data.drop",
    "memory.copy",
    "memory.fill",
    "table.init",
    "elem.drop",
    "table.copy",
    "table.grow",
    "table.size",
    "table.fill",
}

make_enum! {
    Keyword,
    "align",
    "assert_invalid",
    "assert_malformed",
    "assert_return",
    "assert_trap",
    "binary",
    "data",
    "elem",
    "export",
    "f32",
    "f64",
    "func",
    "funcref",
    "global",
    "i32",
    "i64",
    "import",
    "invoke",
    "local",
    "memory",
    "module",
    "mut",
    "offset",
    "param",
    "quote",
    "result",
    "start",
    "table",
    "then",
    "type",
}

#[test]
fn test_keyword_parser() {
    let mut chars = "assert_invalid".char_indices().peekable();
    assert_eq!(
        Keyword::parse(&mut chars).unwrap().1,
        Keyword::AssertInvalid
    );
    assert_eq!(chars.next(), None);
}

#[test]
fn test_parse_int() {
    let mut chars = "0.0".char_indices().peekable();
    assert_eq!(
        parse_int_or_float(&mut chars, false).unwrap().1,
        Token::Float(0.0f64)
    );
    assert_eq!(chars.next(), None);
}

#[test]
fn test_parse_dec() {
    let mut chars = "123".char_indices().peekable();
    assert_eq!(parse_dec(&mut chars).0, 123);
    assert_eq!(chars.next(), None);
}

#[test]
fn test_parse_hex() {
    let mut chars = "123abc".char_indices().peekable();
    assert_eq!(parse_hex(&mut chars).0, 0x123abc);
    assert_eq!(chars.next(), None);
}

#[test]
fn test_number_parser() {
    // 1234e-5
    // 4242.4242
    // 123456789e-5
    // 0x1.fffffffffffffp+1023
    // 0xCAFE
}
