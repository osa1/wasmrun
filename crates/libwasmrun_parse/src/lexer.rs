use libwasmrun_parse_macros::make_enum;

use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug)]
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
}

pub type Result<A> = std::result::Result<Option<A>, LexerError>;

impl Token {
    pub fn parse(chars: &mut Peekable<CharIndices>) -> Result<Token> {
        loop {
            match chars.peek().copied() {
                None => return Ok(None),
                Some((idx, char)) => match char {
                    _ if char.is_whitespace() => {
                        let _ = chars.next();
                        continue;
                    }
                    '(' => {
                        let _ = chars.next();
                        return Ok(Some(Token::LParen));
                    }
                    ')' => {
                        let _ = chars.next();
                        return Ok(Some(Token::RParen));
                    }
                    '=' => {
                        let _ = chars.next();
                        return Ok(Some(Token::Eq));
                    }
                    '"' => {
                        let _ = chars.next();
                        let str = parse_string(chars, idx)?;
                        return Ok(Some(Token::String(str)));
                    }
                    '$' => {
                        let _ = chars.next();
                        let str = parse_var(chars, idx)?;
                        return Ok(Some(Token::Var(str)));
                    }
                    // TODO: literals
                    _ => {
                        // Keyword or instruction. Try instruction first
                        let mut instr_chars = chars.clone();
                        let mut instr_chars_adjusted = (&mut instr_chars).map(|(_, c)| c);
                        match Instr::parse(&mut instr_chars_adjusted) {
                            Some(instr) => {
                                *chars = instr_chars;
                                return Ok(Some(Token::Instr(instr)));
                            }
                            None => {
                                // Try keyword
                                let mut keyword_chars = chars.clone();
                                let mut keyword_chars_adjusted =
                                    (&mut keyword_chars).map(|(_, c)| c);
                                match Keyword::parse(&mut keyword_chars_adjusted) {
                                    Some(kw) => {
                                        *chars = keyword_chars;
                                        return Ok(Some(Token::Keyword(kw)));
                                    }
                                    None => return Err(LexerError::UnknownToken(idx)),
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
) -> std::result::Result<String, LexerError> {
    let mut ret = String::new();

    loop {
        match chars.next() {
            None => {
                return Err(LexerError::UnterminatedString(dquote_idx));
            }
            Some((_, '"')) => {
                return Ok(ret);
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
) -> std::result::Result<String, LexerError> {
    let mut ret = String::new();

    loop {
        match chars.next() {
            None => {
                if ret.is_empty() {
                    return Err(LexerError::EmptyVar(dollar_idx));
                } else {
                    return Ok(ret);
                }
            }
            Some((_, char)) => {
                if char.is_whitespace() {
                    return Ok(ret);
                } else {
                    ret.push(char);
                }
            }
        }
    }
}

make_enum! {
    Instr,
    "unreachable",
    "nop",
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
    "data",
    "export",
    "func",
    "invoke",
    "memory",
    "module",
    "offset",
    "param",
    "result",
    "start",
}

#[test]
fn test_keyword_parser() {
    let mut chars = "assert_invalid".chars();
    assert_eq!(Keyword::parse(&mut chars).unwrap(), Keyword::AssertInvalid);
}
