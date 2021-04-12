#[macro_use]
extern crate lalrpop_util;

pub mod ast;
pub mod instr;
pub mod lexer;

lalrpop_mod!(pub parser);
