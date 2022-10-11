use crate::module::{FunIdx, GlobalIdx};
use crate::stack::Stack;
use crate::store::FunAddr;
use crate::value::{Ref, Value};
use crate::{ExecError, Result};

use libwasmrun_syntax::{Instruction, SimdInstruction};

pub(crate) fn eval_const_expr<GetGlobal, GetFun>(
    get_global: GetGlobal,
    get_fun: GetFun,
    instrs: &[Instruction],
) -> Result<Value>
where
    GetGlobal: Fn(GlobalIdx) -> Value,
    GetFun: Fn(FunIdx) -> FunAddr,
{
    let mut stack = Stack::default();

    for instr in instrs {
        match instr {
            Instruction::I32Const(value) => stack.push_i32(*value)?,

            Instruction::I64Const(value) => stack.push_i64(*value)?,

            Instruction::F32Const(value) => stack.push_f32(f32::from_bits(*value))?,

            Instruction::F64Const(value) => stack.push_f64(f64::from_bits(*value))?,

            Instruction::Simd(SimdInstruction::V128Const(value)) => {
                stack.push_i128(i128::from_le_bytes(*value))?
            }

            Instruction::RefNull(ref_ty) => stack.push_ref(Ref::Null(*ref_ty))?,

            Instruction::RefFunc(fun_idx) => stack.push_ref(Ref::Ref(get_fun(FunIdx(*fun_idx))))?,

            Instruction::GetGlobal(idx) => stack.push_value(get_global(GlobalIdx(*idx)))?,

            Instruction::I32Add => {
                let i2 = stack.pop_i32()?;
                let i1 = stack.pop_i32()?;
                stack.push_i32(i1.wrapping_add(i2))?;
            }

            Instruction::I32Sub => {
                let i2 = stack.pop_i32()?;
                let i1 = stack.pop_i32()?;
                stack.push_i32(i1.wrapping_sub(i2))?;
            }

            Instruction::I32Mul => {
                let i2 = stack.pop_i32()?;
                let i1 = stack.pop_i32()?;
                stack.push_i32(i1.wrapping_mul(i2))?;
            }

            Instruction::I64Add => {
                let i2 = stack.pop_i64()?;
                let i1 = stack.pop_i64()?;
                stack.push_i64(i1.wrapping_add(i2))?;
            }

            Instruction::I64Sub => {
                let i2 = stack.pop_i64()?;
                let i1 = stack.pop_i64()?;
                stack.push_i64(i1.wrapping_sub(i2))?;
            }

            Instruction::I64Mul => {
                let i2 = stack.pop_i64()?;
                let i1 = stack.pop_i64()?;
                stack.push_i64(i1.wrapping_mul(i2))?;
            }

            Instruction::End => break,

            other => exec_panic!("Unsupported const instruction: {:?}", other),
        }
    }

    stack.pop_value()
}
