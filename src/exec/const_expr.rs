use super::parser::types::{Expr, Instruction};
use super::value::Value;

/// A constant expression
///
/// There are two notes in the spec about constant expressions:
///
/// - https://webassembly.github.io/spec/core/exec/modules.html#instantiation
///   
///   At the end of the section: "Evaluation of constant expressions does not affect the store."
///
/// - https://webassembly.github.io/spec/core/valid/instructions.html#constant-expressions
///
///   > Currently, constant expressions occurring as initializers of globals are further
///   > constrained in that contained global.get instructions are only allowed to refer to imported
///   > globals. This is enforced in the validation rule for modules by constraining the context C
///   > accordingly. The definition of constant expression may be extended in future versions of
///   > WebAssembly.
///
#[derive(Debug)]
pub enum ConstExpr {
    Const(Value),
    GlobalGet(u32),
}

impl ConstExpr {
    pub fn from_expr(expr: &Expr) -> Option<ConstExpr> {
        match &&*expr.instrs {
            [instr] => ConstExpr::from_instr(instr),
            _ => None,
        }
    }

    pub fn from_instr(instr: &Instruction) -> Option<ConstExpr> {
        use Instruction::*;
        match instr {
            I32Const(i) => Some(ConstExpr::Const(Value::I32(*i))),
            I64Const(i) => Some(ConstExpr::Const(Value::I64(*i))),
            F32Const(f) => Some(ConstExpr::Const(Value::F32(*f))),
            F64Const(f) => Some(ConstExpr::Const(Value::F64(*f))),
            _ => None,
        }
    }
}
