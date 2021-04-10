use libwasmrun_parse_macros::make_enum;

use std::iter::Peekable;
use std::str::CharIndices;

// Byte index
type Loc = usize;

type Spanned<A> = Result<(Loc, A, Loc), LexerError>;

#[derive(Debug, PartialEq, Eq)]
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

    /// Float literal, uninterpreted
    Float(FloatLit),

    /// Variable
    Var(String),

    /// Instructions
    Instr(Instr),

    /// Keywords
    Keyword(Keyword),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FloatLit {
    Nan(Sign, u64),
    Inf(Sign),
    Float {
        int: i64,
        frac: u64,
        hex: bool,
        power: i64,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Sign {
    Pos,
    Neg,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexerError {
    UnterminatedString(usize),
    UnterminatedNumber,
    EmptyVar(usize),
    UnknownToken(usize),
    InvalidIntDigit(usize),
    CantParseNumber,
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

    pub fn next_token(&mut self) -> Option<Spanned<Token>> {
        <Self as Iterator>::next(self)
    }

    fn next(&mut self) -> Option<(usize, char)> {
        self.iter.next()
    }

    fn bump(&mut self) {
        let _ = self.next();
    }

    /// Bump `n` times
    fn bump_n(&mut self, n: usize) {
        for _ in 0..n {
            self.bump();
        }
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.iter.peek().copied()
    }

    // NB. this peeks
    fn nth(&mut self, n: usize) -> Option<(usize, char)> {
        self.iter.clone().nth(n)
    }

    // NB. this peeks
    fn nth_matches(&mut self, n: usize, expected: char) -> bool {
        match self.nth(n) {
            None => false,
            Some((_, c)) => c == expected,
        }
    }

    /// Reads next character, bumps cursor if it's as expected and returns `true`. Returns `false`
    /// otherwise, without bumping the cursor.
    fn expect(&mut self, expected: char) -> bool {
        match self.peek() {
            None => false,
            Some((_, c)) => {
                if c == expected {
                    self.bump();
                    true
                } else {
                    false
                }
            }
        }
    }

    /// Same as `expect`, but takes string argument.
    fn expect_str(&mut self, expected: &str) -> bool {
        let mut iter = self.iter.clone();
        let mut n = 0;

        for char in expected.chars() {
            n += 1;
            match iter.next() {
                None => return false,
                Some((_, c)) => {
                    if c != char {
                        return false;
                    }
                }
            }
        }

        self.bump_n(n);
        true
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.peek() {
                None => return None,
                Some((idx, char)) => {
                    match char {
                        _ if char.is_whitespace() => {
                            let _ = self.next();
                            continue;
                        }
                        _ if char.is_ascii_digit() => {
                            return Some(self.parse_number(idx, Sign::Pos));
                        }
                        '-' | '+' => {
                            let sign = if char == '-' { Sign::Neg } else { Sign::Pos };
                            self.bump(); // consume sign
                            return Some(self.parse_number(idx, sign));
                        }
                        ';' => {
                            let _ = self.next();
                            loop {
                                match self.next() {
                                    None => return None,
                                    Some((_, '\n')) => break,
                                    Some((_, _)) => continue,
                                }
                            }
                        }
                        '(' => {
                            let _ = self.next();
                            return Some(Ok((idx, Token::LParen, '('.len_utf8())));
                        }
                        ')' => {
                            let _ = self.next();
                            return Some(Ok((idx, Token::RParen, ')'.len_utf8())));
                        }
                        '=' => {
                            let _ = self.next();
                            return Some(Ok((idx, Token::Eq, '='.len_utf8())));
                        }
                        '"' => {
                            let _ = self.next();
                            return Some(
                                self.parse_string(idx)
                                    .map(|(str, end)| (idx, Token::String(str), end)),
                            );
                        }
                        '$' => {
                            let _ = self.next();
                            return Some(
                                self.parse_var(idx)
                                    .map(|(str, end)| (idx, Token::Var(str), end)),
                            );
                        }
                        'n' => {
                            if self.nth_matches(1, 'a') && self.nth_matches(2, 'n') {
                                return Some(self.parse_number(idx, Sign::Pos));
                            } else {
                                return Some(Err(LexerError::UnknownToken(idx)));
                            }
                        }
                        'i' if self.nth_matches(1, 'n') && self.nth_matches(2, 'f') => {
                            return Some(self.parse_number(idx, Sign::Pos));
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
                    }
                }
            }
        }
    }
}

impl<'input> Lexer<'input> {
    /// Assumes that the initial '"' is consumed. Consumes the closing '"' and returns index after it.
    fn parse_string(&mut self, dquote_idx: Loc) -> Result<(String, Loc), LexerError> {
        let mut ret = String::new();

        loop {
            match self.next() {
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
    fn parse_var(&mut self, dollar_idx: Loc) -> Result<(String, Loc), LexerError> {
        let mut ret = String::new();
        let mut last = 0;

        loop {
            match self.next() {
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

    fn parse_number(&mut self, begin_idx: Loc, sign: Sign) -> Spanned<Token> {
        match self.peek() {
            None => return Err(LexerError::UnterminatedNumber),
            Some((_, 'i')) => {
                self.bump(); // consume 'i'
                if !self.expect('n') {
                    return Err(LexerError::CantParseNumber);
                }
                if !self.expect('f') {
                    return Err(LexerError::CantParseNumber);
                }
                Ok((
                    begin_idx,
                    Token::Float(FloatLit::Inf(sign)),
                    begin_idx + "inf".len(),
                ))
            }
            Some((_, 'n')) => {
                self.bump(); // consume 'n'

                if !self.expect_str("an") {
                    return Err(LexerError::CantParseNumber);
                }

                if self.expect_str(":0x") {
                    let (payload, end_idx) = self.hexnum();
                    Ok((
                        begin_idx,
                        Token::Float(FloatLit::Nan(sign, payload)),
                        end_idx,
                    ))
                } else {
                    Ok((
                        begin_idx,
                        Token::Float(FloatLit::Nan(sign, 0)),
                        begin_idx + "nan".len(),
                    ))
                }
            }
            Some((idx, '0')) => {
                if self.nth_matches(1, 'x') {
                    self.bump(); // consume '0'
                    self.bump(); // consume 'x'
                    self.parse_hex_number(idx, sign)
                } else {
                    self.parse_dec_number(idx, sign)
                }
            }
            Some((idx, c)) if c.is_ascii_digit() => self.parse_dec_number(idx, sign),
            _ => Err(LexerError::CantParseNumber),
        }
    }

    fn parse_sign(&mut self) -> Option<Sign> {
        match self.peek() {
            Some((_, '+')) => {
                self.bump();
                Some(Sign::Pos)
            }
            Some((_, '-')) => {
                self.bump();
                Some(Sign::Neg)
            }
            _ => None,
        }
    }

    fn parse_dec_number(&mut self, begin_idx: Loc, sign1: Sign) -> Spanned<Token> {
        let (num1, end1) = self.num();
        if self.expect('.') {
            let (num2, end2) = self.num();
            if self.expect('e') || self.expect('E') {
                let sign2 = self.parse_sign();
                let (num3, end3) = self.num();
                let num3 = if sign2 == Some(Sign::Neg) {
                    -(num3 as i64)
                } else {
                    num3 as i64
                };
                Ok((
                    begin_idx,
                    Token::Float(FloatLit::Float {
                        int: if sign1 == Sign::Neg {
                            -(num1 as i64)
                        } else {
                            num1 as i64
                        },
                        frac: num2,
                        hex: false,
                        power: num3,
                    }),
                    end3,
                ))
            } else {
                Ok((
                    begin_idx,
                    Token::Float(FloatLit::Float {
                        int: if sign1 == Sign::Neg {
                            -(num1 as i64)
                        } else {
                            num1 as i64
                        },
                        frac: num2,
                        hex: false,
                        power: 0,
                    }),
                    end2,
                ))
            }
        } else {
            let num = num1 as i64;
            let num = if sign1 == Sign::Neg && num >> 63 == 0 {
                -num
            } else {
                num
            };
            Ok((begin_idx, Token::Int(num), end1))
        }
    }

    fn parse_hex_number(&mut self, begin_idx: Loc, sign1: Sign) -> Spanned<Token> {
        let (num1, end1) = self.hexnum();
        if self.expect('.') {
            let (num2, end2) = self.hexnum();
            if self.expect('p') || self.expect('P') {
                let sign2 = self.parse_sign();
                let (num3, end3) = self.hexnum();
                let num3 = if sign2 == Some(Sign::Neg) {
                    -(num3 as i64)
                } else {
                    num3 as i64
                };
                Ok((
                    begin_idx,
                    Token::Float(FloatLit::Float {
                        int: if sign1 == Sign::Neg {
                            -(num1 as i64)
                        } else {
                            num1 as i64
                        },
                        frac: num2,
                        hex: true,
                        power: num3,
                    }),
                    end3,
                ))
            } else {
                Ok((
                    begin_idx,
                    Token::Float(FloatLit::Float {
                        int: if sign1 == Sign::Neg {
                            -(num1 as i64)
                        } else {
                            num1 as i64
                        },
                        frac: num2,
                        hex: true,
                        power: 0,
                    }),
                    end2,
                ))
            }
        } else {
            let num = num1 as i64;
            let num = if sign1 == Sign::Neg && num >> 63 == 0 {
                -num
            } else {
                num
            };
            Ok((begin_idx, Token::Int(num), end1))
        }
    }

    // 'num' production: https://webassembly.github.io/spec/core/text/values.html#text-num
    fn num(&mut self) -> (u64, usize) {
        let mut i = 0;
        let mut end_idx = 0;

        loop {
            match self.peek() {
                None => {
                    break;
                }
                Some((idx, char)) => {
                    if char.is_ascii_digit() {
                        end_idx = idx + char.len_utf8();
                        self.bump();
                        i *= 10;
                        i += u64::from(char) - u64::from(b'0');
                    } else if char == '_' {
                        self.bump();
                    } else {
                        break;
                    }
                }
            }
        }

        (i, end_idx)
    }

    // 'hexnum' production: https://webassembly.github.io/spec/core/text/values.html#text-hexnum
    fn hexnum(&mut self) -> (u64, usize) {
        let mut i = 0;
        let mut end_idx = 0;

        loop {
            match self.peek() {
                None => {
                    break;
                }
                Some((idx, char)) => {
                    if char.is_ascii_hexdigit() {
                        end_idx = idx + char.len_utf8();
                        self.bump();

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
                    } else if char == '_' {
                        end_idx = idx + char.len_utf8();
                        self.bump();
                    } else {
                        break;
                    }
                }
            }
        }

        assert!(end_idx != 0); // TODO: turn this in to a lexer error

        println!("hexnum returning {:#x}", i);

        (i, end_idx)
    }
}

make_enum! {
    Instr,
    "unreachable",
    "nop", // parsed with 'nan' in `Token::parse`
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

#[cfg(test)]
mod tests {
    use super::*;

    fn next_token(lexer: &mut Lexer<'_>) -> Token {
        lexer.next_token().unwrap().unwrap().1
    }

    #[test]
    fn keyword() {
        let mut lexer = Lexer::new("assert_invalid");
        assert_eq!(
            next_token(&mut lexer),
            Token::Keyword(Keyword::AssertInvalid)
        );
        assert_eq!(lexer.next_token(), None);
    }

    #[test]
    fn instruction() {
        let mut lexer = Lexer::new("i32.add");
        assert_eq!(next_token(&mut lexer), Token::Instr(Instr::I32Add));
        assert_eq!(lexer.next_token(), None);
    }

    #[test]
    fn int() {
        let mut lexer = Lexer::new(
            // "0 1 -1 +1 0xF +0xF -0xF \
            //  0x0bAdD00D 0xffffffff 0x7fffffff -0x7fffffff \
            //  0x0CABBA6E0ba66a6e \
            //  0xffffffffffffffff 0x7fffffffffffffff -0x7fffffffffffffff -0x8000000000000000 \
            "0x8000000000000000",
        );

        // assert_eq!(next_token(&mut lexer), Token::Int(0));
        // assert_eq!(next_token(&mut lexer), Token::Int(1));
        // assert_eq!(next_token(&mut lexer), Token::Int(-1));
        // assert_eq!(next_token(&mut lexer), Token::Int(1));
        // assert_eq!(next_token(&mut lexer), Token::Int(15));
        // assert_eq!(next_token(&mut lexer), Token::Int(15));
        // assert_eq!(next_token(&mut lexer), Token::Int(-15));

        // assert_eq!(next_token(&mut lexer), Token::Int(195940365));
        // assert_eq!(next_token(&mut lexer), Token::Int(4294967295));
        // assert_eq!(next_token(&mut lexer), Token::Int(2147483647));
        // assert_eq!(next_token(&mut lexer), Token::Int(-2147483647));

        // assert_eq!(next_token(&mut lexer), Token::Int(913028331277281902));

        // assert_eq!(next_token(&mut lexer), Token::Int(-1));
        // assert_eq!(next_token(&mut lexer), Token::Int(9223372036854775807));
        // assert_eq!(next_token(&mut lexer), Token::Int(-9223372036854775807));
        // assert_eq!(next_token(&mut lexer), Token::Int(-9223372036854775808));

        // TODO: Fix and enable the rest

        assert_eq!(next_token(&mut lexer), Token::Int(-1));

        // assert_eq!(lexer.next_token(), None);
    }

    #[test]
    fn float() {
        let mut lexer = Lexer::new(
            "nan +nan -nan
             nan:0x400000 nan:0x200000 -nan:0x7fffff
             inf -inf
             0x0.0p0
             0x1.921fb6p+2
             3.4028234e-38",
        );

        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Nan(Sign::Pos, 0))
        );
        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Nan(Sign::Pos, 0))
        );
        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Nan(Sign::Neg, 0))
        );

        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Nan(Sign::Pos, 0x400000))
        );
        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Nan(Sign::Pos, 0x200000))
        );
        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Nan(Sign::Neg, 0x7fffff))
        );

        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Inf(Sign::Pos))
        );
        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Inf(Sign::Neg))
        );

        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Float {
                int: 0,
                frac: 0,
                hex: true,
                power: 0,
            })
        );

        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Float {
                int: 1,
                frac: 0x921fb6,
                hex: true,
                power: 2,
            })
        );

        assert_eq!(
            next_token(&mut lexer),
            Token::Float(FloatLit::Float {
                int: 3,
                frac: 4028234,
                hex: false,
                power: -38,
            })
        );

        assert_eq!(lexer.next_token(), None);
    }
}
