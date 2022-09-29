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
        }

        SimdInstruction::V128Load8Lane(MemArg { align: _, offset }, lane) => {
            let mut vec = rt.stack.pop_i128()?.to_le_bytes();

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 1)?;

            let b = mem[addr];

            let lane = usize::from(lane);
            vec[lane] = b;

            rt.stack.push_i128(i128::from_le_bytes(vec))?;
        }

        SimdInstruction::V128Load16Lane(MemArg { align: _, offset }, lane) => {
            let mut vec = rt.stack.pop_i128()?.to_le_bytes();

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 2)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];

            let lane = usize::from(lane);
            vec[lane * 2] = b1;
            vec[lane * 2 + 1] = b2;

            rt.stack.push_i128(i128::from_le_bytes(vec))?;
        }

        SimdInstruction::V128Load32Lane(MemArg { align: _, offset }, lane) => {
            let mut vec = rt.stack.pop_i128()?.to_le_bytes();

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 4)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];

            let lane = usize::from(lane);
            vec[lane * 4] = b1;
            vec[lane * 4 + 1] = b2;
            vec[lane * 4 + 2] = b3;
            vec[lane * 4 + 3] = b4;

            rt.stack.push_i128(i128::from_le_bytes(vec))?;
        }

        SimdInstruction::V128Load64Lane(MemArg { align: _, offset }, lane) => {
            let mut vec = rt.stack.pop_i128()?.to_le_bytes();

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 8)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            let b5 = mem[addr + 4];
            let b6 = mem[addr + 5];
            let b7 = mem[addr + 6];
            let b8 = mem[addr + 7];

            let lane = usize::from(lane);
            vec[lane * 8] = b1;
            vec[lane * 8 + 1] = b2;
            vec[lane * 8 + 2] = b3;
            vec[lane * 8 + 3] = b4;
            vec[lane * 8 + 4] = b5;
            vec[lane * 8 + 5] = b6;
            vec[lane * 8 + 6] = b7;
            vec[lane * 8 + 7] = b8;

            rt.stack.push_i128(i128::from_le_bytes(vec))?;
        }

        SimdInstruction::I8x16ExtractLaneS(lane_idx) => {
            let vec = rt.stack.pop_i128()?.to_le_bytes();
            rt.stack.push_i32(vec[usize::from(lane_idx)] as i32)?;
        }

        SimdInstruction::I8x16Eq => {
            let v2 = rt.stack.pop_i128()?.to_le_bytes();
            let v1 = rt.stack.pop_i128()?.to_le_bytes();
            let mut res: [u8; 16] = [0; 16];
            for i in 0..16 {
                res[i] = if v1[i] == v2[i] { 0xFF } else { 0x00 };
            }
            rt.stack.push_i128(i128::from_le_bytes(res))?;
        }

        SimdInstruction::V128Not => {
            let mut v = rt.stack.pop_i128()?.to_le_bytes();
            for i in 0..16 {
                v[i] = !v[i];
            }
            rt.stack.push_i128(i128::from_le_bytes(v))?;
        }

        SimdInstruction::I8x16AllTrue => {
            let v = rt.stack.pop_i128()?.to_le_bytes();
            let res = v.iter().all(|b| *b != 0);
            rt.stack.push_i32(if res { 1 } else { 0 })?;
        }

        SimdInstruction::V128Bitselect => {
            let c = rt.stack.pop_i128()? as u128;
            let v2 = rt.stack.pop_i128()? as u128;
            let v1 = rt.stack.pop_i128()? as u128;
            let res = ((v1 & c) | (v2 & !c)) as i128;
            rt.stack.push_i128(res)?;
        }

        SimdInstruction::I8x16Shl => {
            let shift = rt.stack.pop_i32()?;
            let mut v = rt.stack.pop_i128()?.to_le_bytes();
            for i in 0..16 {
                v[i] = v[i].wrapping_shl(shift as u32);
            }
            rt.stack.push_i128(i128::from_le_bytes(v))?;
        }

        SimdInstruction::I8x16Add => {
            let v2 = rt.stack.pop_i128()?.to_le_bytes();
            let v1 = rt.stack.pop_i128()?.to_le_bytes();
            let mut res: [u8; 16] = [0; 16];
            for i in 0..16 {
                res[i] = v1[i].wrapping_add(v2[i]);
            }
            rt.stack.push_i128(i128::from_le_bytes(res))?;
        }

        SimdInstruction::I8x16Sub => {
            let v2 = rt.stack.pop_i128()?.to_le_bytes();
            let v1 = rt.stack.pop_i128()?.to_le_bytes();
            let mut res: [u8; 16] = [0; 16];
            for i in 0..16 {
                res[i] = v1[i].wrapping_sub(v2[i]);
            }
            rt.stack.push_i128(i128::from_le_bytes(res))?;
        }

        SimdInstruction::F32x4Mul => {
            let v2 = rt.stack.pop_i128()?;
            let v1 = rt.stack.pop_i128()?;
            let fs2 = vec_to_f32x4(v2);
            let mut fs1 = vec_to_f32x4(v1);
            for i in 0..4 {
                fs1[i] *= fs2[i];
            }
            rt.stack.push_i128(f32x4_to_vec(fs1))?;
        }

        SimdInstruction::F32x4Abs => {
            let v = rt.stack.pop_i128()?;
            let mut fs = vec_to_f32x4(v);
            for i in 0..4 {
                fs[i] = fs[i].abs();
            }
            rt.stack.push_i128(f32x4_to_vec(fs))?;
        }

        SimdInstruction::F32x4Min => {
            let v2 = rt.stack.pop_i128()?;
            let v1 = rt.stack.pop_i128()?;
            let fs2 = vec_to_f32x4(v2);
            let mut fs1 = vec_to_f32x4(v1);
            for i in 0..4 {
                fs1[i] = fs1[i].min(fs2[i]);
            }
            rt.stack.push_i128(f32x4_to_vec(fs1))?;
        }

        SimdInstruction::I32x4TruncSatF32x4S => {
            let v = rt.stack.pop_i128()?;
            let fs = vec_to_f32x4(v);
            let is: [i32; 4] = fs.map(super::i32_trunc_sat_s_f32);
            rt.stack.push_i128(i32x4_to_vec(is))?;
        }

        SimdInstruction::F32x4ConvertI32x4U => {
            let v = rt.stack.pop_i128()?;
            let is = vec_to_i32x4(v);
            let fs: [f32; 4] = is.map(super::f32_convert_u_i32);
            rt.stack.push_i128(f32x4_to_vec(fs))?;
        }

        SimdInstruction::I8x16Swizzle => {
            let v2 = rt.stack.pop_i128()?.to_le_bytes();
            let v1 = rt.stack.pop_i128()?.to_le_bytes();
            let mut res: [u8; 16] = [0; 16];
            for i in 0..16 {
                let idx = v2[i];
                if idx < 16 {
                    res[i] = v1[usize::from(idx)];
                }
            }
            rt.stack.push_i128(i128::from_le_bytes(res))?;
        }

        _ => {
            return Err(ExecError::Panic(format!(
                "SIMD instruction not implemented: {:?}",
                instr
            )))
        }
    }

    rt.ip += 1;
    Ok(())
}

fn vec_to_f32x4(v: i128) -> [f32; 4] {
    let bytes = v.to_le_bytes();
    [
        f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        f32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]),
        f32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]),
        f32::from_le_bytes([bytes[12], bytes[13], bytes[14], bytes[15]]),
    ]
}

fn vec_to_i32x4(v: i128) -> [i32; 4] {
    let bytes = v.to_le_bytes();
    [
        i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        i32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]),
        i32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]),
        i32::from_le_bytes([bytes[12], bytes[13], bytes[14], bytes[15]]),
    ]
}

fn f32x4_to_vec(fs: [f32; 4]) -> i128 {
    let [f1, f2, f3, f4] = fs;
    let f1_bytes = f1.to_le_bytes();
    let f2_bytes = f2.to_le_bytes();
    let f3_bytes = f3.to_le_bytes();
    let f4_bytes = f4.to_le_bytes();
    i128::from_le_bytes([
        f1_bytes[0],
        f1_bytes[1],
        f1_bytes[2],
        f1_bytes[3],
        f2_bytes[0],
        f2_bytes[1],
        f2_bytes[2],
        f2_bytes[3],
        f3_bytes[0],
        f3_bytes[1],
        f3_bytes[2],
        f3_bytes[3],
        f4_bytes[0],
        f4_bytes[1],
        f4_bytes[2],
        f4_bytes[3],
    ])
}

fn i32x4_to_vec(is: [i32; 4]) -> i128 {
    let [i1, i2, i3, i4] = is;
    let i1_bytes = i1.to_le_bytes();
    let i2_bytes = i2.to_le_bytes();
    let i3_bytes = i3.to_le_bytes();
    let i4_bytes = i4.to_le_bytes();
    i128::from_le_bytes([
        i1_bytes[0],
        i1_bytes[1],
        i1_bytes[2],
        i1_bytes[3],
        i2_bytes[0],
        i2_bytes[1],
        i2_bytes[2],
        i2_bytes[3],
        i3_bytes[0],
        i3_bytes[1],
        i3_bytes[2],
        i3_bytes[3],
        i4_bytes[0],
        i4_bytes[1],
        i4_bytes[2],
        i4_bytes[3],
    ])
}
