use crate::exec::{trapping_add, ExecError, MemIdx, Result};
use crate::store::ModuleAddr;
use crate::Runtime;

use libwasmrun_syntax::{MemArg, SimdInstruction};

// Informal specification of SIMD insturctions:
// https://github.com/WebAssembly/simd/blob/main/proposals/simd/SIMD.md
pub fn exec_simd_instr(
    rt: &mut Runtime,
    module_addr: ModuleAddr,
    instr: SimdInstruction,
) -> Result<()> {
    match instr {
        SimdInstruction::V128Load(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 16)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            let b5 = mem[addr + 4];
            let b6 = mem[addr + 5];
            let b7 = mem[addr + 6];
            let b8 = mem[addr + 7];
            let b9 = mem[addr + 8];
            let b10 = mem[addr + 9];
            let b11 = mem[addr + 10];
            let b12 = mem[addr + 11];
            let b13 = mem[addr + 12];
            let b14 = mem[addr + 13];
            let b15 = mem[addr + 14];
            let b16 = mem[addr + 15];

            rt.stack.push_i128(i128::from_le_bytes([
                b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16,
            ]))?;

            rt.ip += 1;
        }

        SimdInstruction::V128Load8x8s(MemArg { align: _, offset }) => {
            // Load 8 8-bit integers, sign extend each to 16-bit
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 8)?;

            let b1 = (((mem[addr] as i8) as i16) as u16).to_le_bytes();
            let b2 = (((mem[addr + 1] as i8) as i16) as u16).to_le_bytes();
            let b3 = (((mem[addr + 2] as i8) as i16) as u16).to_le_bytes();
            let b4 = (((mem[addr + 3] as i8) as i16) as u16).to_le_bytes();
            let b5 = (((mem[addr + 4] as i8) as i16) as u16).to_le_bytes();
            let b6 = (((mem[addr + 5] as i8) as i16) as u16).to_le_bytes();
            let b7 = (((mem[addr + 6] as i8) as i16) as u16).to_le_bytes();
            let b8 = (((mem[addr + 7] as i8) as i16) as u16).to_le_bytes();

            rt.stack.push_i128(i128::from_le_bytes([
                b1[0], b1[1], b2[0], b2[1], b3[0], b3[1], b4[0], b4[1], b5[0], b5[1], b6[0], b6[1],
                b7[0], b7[1], b8[0], b8[1],
            ]))?;

            rt.ip += 1;
        }

        _ => {
            return Err(ExecError::Panic(format!(
                "SIMD instruction not implemented: {:?}",
                instr
            )))
        }
    }

    Ok(())
}
