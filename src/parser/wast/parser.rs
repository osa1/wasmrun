#![allow(dead_code)]

use crate::parser::types::*;
use crate::parser::wast::lexer::{Lexer, LexerError, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

pub enum ParseError {
    LexerError(LexerError),
    UnexpectedToken {
        expected: &'static str,
        found: String,
    },
    UnexpectedEOF,
}

pub type Result<A> = ::std::result::Result<A, ParseError>;

impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> Self {
        ParseError::LexerError(err)
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser { lexer }
    }

    pub fn parse_module(&mut self) -> Result<Module> {
        self.parens(&mut |parser| {

            parser.kw("module")?;

            todo!()
        })
    }

    fn next_token(&mut self) -> Result<Token> {
        match self.lexer.next() {
            None => Err(ParseError::UnexpectedEOF),
            Some(Err(err)) => Err(err.into()),
            Some(Ok(token)) => Ok(token),
        }
    }

    fn parens<A>(&mut self, parse: &mut dyn FnMut(&mut Parser<'a>) -> Result<A>) -> Result<A> {
        match self.next_token()? {
            Token::LParen => {
                let ret = parse(self)?;
                match self.next_token()? {
                    Token::RParen => Ok(ret),
                    other => Err(ParseError::UnexpectedToken {
                        expected: "right paren",
                        found: format!("{:?}", other),
                    }),
                }
            }
            other => Err(ParseError::UnexpectedToken {
                expected: "right paren",
                found: format!("{:?}", other),
            }),
        }
    }

    fn kw(&mut self, kw: &'static str) -> Result<()> {
        match self.next_token()? {
            Token::Reserved(reserved) if reserved == kw => Ok(()),
            other => Err(ParseError::UnexpectedToken {
                expected: kw,
                found: format!("{:?}", other),
            }),
        }
    }
}
