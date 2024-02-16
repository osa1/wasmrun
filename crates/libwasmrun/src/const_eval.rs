use crate::exec::Runtime;
use crate::module::{FunIdx, GlobalIdx};
use crate::stack::Stack;
use crate::store::ModuleAddr;
use crate::value::{Ref, Value};
use crate::Result;

use libwasmrun_syntax::{Instruction, SimdInstruction};

pub(crate) fn eval_const_expr(
    rt: &mut Runtime,
    module_addr: ModuleAddr,
    instrs: &[Instruction],
) -> Result<Value> {
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

            Instruction::RefNull(heap_ty) => stack.push_ref(Ref::Null(*heap_ty))?,

            Instruction::RefFunc(fun_idx) => {
                let module = rt.get_module(module_addr);
                let fun_addr = module.get_fun(FunIdx(*fun_idx));
                stack.push_ref(Ref::Func(fun_addr))?
            }

            Instruction::GetGlobal(idx) => {
                let module = rt.get_module(module_addr);
                let global_addr = module.get_global(GlobalIdx(*idx));
                let global_value = rt.store.get_global(global_addr).value;
                stack.push_value(global_value)?;
            }

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
