use crate::exec::Runtime;
use crate::module::{FunIdx, GlobalIdx, TypeIdx};
use crate::stack::Stack;
use crate::store::ModuleAddr;
use crate::value::{Ref, Value};
use crate::Result;

use libwasmrun_syntax as wasm;
use libwasmrun_syntax::{Instruction, SimdInstruction};

pub(crate) fn eval_const_expr(
    rt: &mut Runtime,
    module_addr: ModuleAddr,
    instrs: &[Instruction],
) -> Result<Value> {
    // TODO FIXME: Now that we have access to `Runtime` maybe we should use `rt.stack`.
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

            Instruction::RefI31 => {
                let value = stack.pop_i32()?;
                let mut i31 = (value as u32) & 0x7f_ff_ff_ff;

                if value < 0 {
                    i31 |= 0x80_00_00_00;
                }

                stack.push_value(Value::Ref(Ref::I31(i31 as i32)))?;
            }

            Instruction::StructNew(ty_idx) => {
                let struct_type = rt
                    .store
                    .get_module(module_addr)
                    .get_type(TypeIdx(*ty_idx))
                    .as_struct_type()
                    .unwrap();

                let mut fields: Vec<Value> = Vec::with_capacity(struct_type.fields.len());

                for wasm::FieldType {
                    storage_ty,
                    mutability: _,
                } in struct_type.fields.iter().rev()
                {
                    let value = stack.pop_value().unwrap();
                    match (storage_ty, value) {
                        (wasm::StorageType::Val(wasm::ValueType::I32), Value::I32(_))
                        | (wasm::StorageType::Val(wasm::ValueType::I64), Value::I64(_))
                        | (wasm::StorageType::Val(wasm::ValueType::F32), Value::F32(_))
                        | (wasm::StorageType::Val(wasm::ValueType::F64), Value::F64(_))
                        | (wasm::StorageType::Val(wasm::ValueType::V128), Value::I128(_)) => {
                            fields.push(value);
                        }

                        (wasm::StorageType::Val(wasm::ValueType::Reference(_)), Value::Ref(_)) => {
                            // TODO: Check reference type.
                            fields.push(value);
                        }

                        (wasm::StorageType::Packed(wasm::PackedType::I8), Value::I32(value)) => {
                            fields.push(Value::I32((value as i8) as i32));
                        }

                        (wasm::StorageType::Packed(wasm::PackedType::I16), Value::I32(value)) => {
                            fields.push(Value::I32((value as i16) as i32));
                        }

                        _ => todo!(),
                    }
                }

                fields.reverse();

                let struct_addr = rt.store.allocate_struct(struct_type.clone(), fields);

                stack.push_ref(Ref::Struct(struct_addr))?;
            }

            Instruction::StructNewDefault(ty_idx) => {
                let struct_type = rt
                    .store
                    .get_module(module_addr)
                    .get_type(TypeIdx(*ty_idx))
                    .as_struct_type()
                    .unwrap();

                let fields: Vec<Value> = struct_type
                    .fields
                    .iter()
                    .rev()
                    .map(
                        |wasm::FieldType {
                             storage_ty,
                             mutability: _,
                         }| Value::default_from_storage_type(storage_ty),
                    )
                    .collect();

                let struct_addr = rt.store.allocate_struct(struct_type.clone(), fields);

                stack.push_ref(Ref::Struct(struct_addr))?;
            }

            Instruction::End => break,

            other => exec_panic!("Unsupported const instruction: {:?}", other),
        }
    }

    stack.pop_value()
}
