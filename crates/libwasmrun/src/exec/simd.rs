#![allow(clippy::identity_op, clippy::needless_range_loop)]

use crate::exec::{trapping_add, MemIdx, Result};
use crate::store::ModuleAddr;
use crate::value::{canonicalize_f32_nan, canonicalize_f64_nan};
use crate::Runtime;

use libwasmrun_syntax::{MemArg, SimdInstruction};

use std::convert::TryInto;
use std::ops::Neg;

// Informal specification of SIMD insturctions:
// https://github.com/WebAssembly/simd/blob/main/proposals/simd/SIMD.md
pub fn exec_simd_instr(
    rt: &mut Runtime,
    module_addr: ModuleAddr,
    instr: SimdInstruction,
) -> Result<()> {
    match instr {
        SimdInstruction::V128Const(bytes) => {
            rt.stack.push_i128(i128::from_le_bytes(bytes))?;
        }

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

        SimdInstruction::V128Store(MemArg { align: _, offset }) => {
            let vec = rt.stack.pop_i128()?.to_le_bytes();

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);
            mem.check_range(addr, 16)?;

            for i in 0..16 {
                mem[addr + i] = vec[i as usize];
            }
        }

        SimdInstruction::V128Store8Lane(MemArg { align: _, offset }, lane) => {
            let vec = rt.stack.pop_i128()?.to_le_bytes();

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);
            mem.check_range(addr, 1)?;

            let byte = vec[usize::from(lane)];
            mem[addr] = byte;
        }

        SimdInstruction::V128Store16Lane(MemArg { align: _, offset }, lane) => {
            let vec = rt.stack.pop_i128()?.to_le_bytes();

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);
            mem.check_range(addr, 2)?;

            let b1 = vec[usize::from(lane) * 2];
            let b2 = vec[usize::from(lane) * 2 + 1];
            mem[addr] = b1;
            mem[addr + 1] = b2;
        }

        SimdInstruction::V128Store32Lane(MemArg { align: _, offset }, lane) => {
            let vec = rt.stack.pop_i128()?.to_le_bytes();

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);
            mem.check_range(addr, 4)?;

            let lane = usize::from(lane);
            mem.set_range(addr, &vec[lane * 4..lane * 4 + 4])?;
        }

        SimdInstruction::V128Store64Lane(MemArg { align: _, offset }, lane) => {
            let vec = rt.stack.pop_i128()?.to_le_bytes();

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);

            let lane = usize::from(lane);
            mem.set_range(addr, &vec[lane * 8..lane * 8 + 8])?;
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

        SimdInstruction::V128Load8x8u(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 8)?;

            let b1 = (mem[addr] as u16).to_le_bytes();
            let b2 = (mem[addr + 1] as u16).to_le_bytes();
            let b3 = (mem[addr + 2] as u16).to_le_bytes();
            let b4 = (mem[addr + 3] as u16).to_le_bytes();
            let b5 = (mem[addr + 4] as u16).to_le_bytes();
            let b6 = (mem[addr + 5] as u16).to_le_bytes();
            let b7 = (mem[addr + 6] as u16).to_le_bytes();
            let b8 = (mem[addr + 7] as u16).to_le_bytes();

            rt.stack.push_i128(i128::from_le_bytes([
                b1[0], b1[1], b2[0], b2[1], b3[0], b3[1], b4[0], b4[1], b5[0], b5[1], b6[0], b6[1],
                b7[0], b7[1], b8[0], b8[1],
            ]))?;
        }

        SimdInstruction::V128Load16x4s(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 8)?;

            let i1 = mem.load_16(addr)? as i16 as i32;
            let i2 = mem.load_16(addr + 2)? as i16 as i32;
            let i3 = mem.load_16(addr + 4)? as i16 as i32;
            let i4 = mem.load_16(addr + 6)? as i16 as i32;

            rt.stack.push_i128(i32x4_to_vec([i1, i2, i3, i4]))?;
        }

        SimdInstruction::V128Load16x4u(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 8)?;

            let i1 = mem.load_16(addr)? as i32;
            let i2 = mem.load_16(addr + 2)? as i32;
            let i3 = mem.load_16(addr + 4)? as i32;
            let i4 = mem.load_16(addr + 6)? as i32;

            rt.stack.push_i128(i32x4_to_vec([i1, i2, i3, i4]))?;
        }

        SimdInstruction::V128Load32x2s(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 8)?;

            let i1 = mem.load_32(addr)? as i32 as i64;
            let i2 = mem.load_32(addr + 4)? as i32 as i64;

            rt.stack.push_i128(i64x2_to_vec([i1, i2]))?;
        }

        SimdInstruction::V128Load32x2u(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 8)?;

            let i1 = mem.load_32(addr)? as i64;
            let i2 = mem.load_32(addr + 4)? as i64;

            rt.stack.push_i128(i64x2_to_vec([i1, i2]))?;
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

        SimdInstruction::V128Load8Splat(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);

            let b = mem.load_8(addr)?;

            rt.stack.push_i128(i128::from_le_bytes([b; 16]))?;
        }

        SimdInstruction::V128Load16Splat(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);

            let bs = mem.load_16(addr)?.to_le_bytes();

            rt.stack.push_i128(i128::from_le_bytes([
                bs[0], bs[1], bs[0], bs[1], bs[0], bs[1], bs[0], bs[1], bs[0], bs[1], bs[0], bs[1],
                bs[0], bs[1], bs[0], bs[1],
            ]))?;
        }

        SimdInstruction::V128Load32Splat(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);

            let bs = mem.load_32(addr)?.to_le_bytes();

            rt.stack.push_i128(i128::from_le_bytes([
                bs[0], bs[1], bs[2], bs[3], bs[0], bs[1], bs[2], bs[3], bs[0], bs[1], bs[2], bs[3],
                bs[0], bs[1], bs[2], bs[3],
            ]))?;
        }

        SimdInstruction::V128Load64Splat(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);

            let bs = mem.load_64(addr)?.to_le_bytes();

            rt.stack.push_i128(i128::from_le_bytes([
                bs[0], bs[1], bs[2], bs[3], bs[4], bs[5], bs[6], bs[7], bs[0], bs[1], bs[2], bs[3],
                bs[4], bs[5], bs[6], bs[7],
            ]))?;
        }

        SimdInstruction::I8x16ExtractLaneS(lane_idx) => {
            let vec = rt.stack.pop_i128()?.to_le_bytes();
            rt.stack.push_i32(vec[usize::from(lane_idx)] as i8 as i32)?;
        }

        SimdInstruction::I8x16ExtractLaneU(lane_idx) => {
            let vec = rt.stack.pop_i128()?.to_le_bytes();
            rt.stack
                .push_i32((vec[usize::from(lane_idx)] as u32) as i32)?;
        }

        SimdInstruction::I16x8ExtractLaneS(lane_idx) => {
            let vec = vec_to_i16x8(rt.stack.pop_i128()?);
            rt.stack.push_i32(vec[usize::from(lane_idx)] as i32)?;
        }

        SimdInstruction::I16x8ExtractLaneU(lane_idx) => {
            let vec = vec_to_i16x8(rt.stack.pop_i128()?);
            rt.stack
                .push_i32((vec[usize::from(lane_idx)] as u16 as u32) as i32)?;
        }

        SimdInstruction::V128Not => {
            let v = rt.stack.pop_i128()?;
            rt.stack.push_i128(!v)?;
        }

        SimdInstruction::V128And => {
            let v2 = rt.stack.pop_i128()?;
            let v1 = rt.stack.pop_i128()?;
            rt.stack.push_i128(v1 & v2)?;
        }

        SimdInstruction::V128Or => {
            let v2 = rt.stack.pop_i128()?;
            let v1 = rt.stack.pop_i128()?;
            rt.stack.push_i128(v1 | v2)?;
        }

        SimdInstruction::V128Xor => {
            let v2 = rt.stack.pop_i128()?;
            let v1 = rt.stack.pop_i128()?;
            rt.stack.push_i128(v1 ^ v2)?;
        }

        SimdInstruction::V128AndNot => {
            let v2 = rt.stack.pop_i128()?;
            let v1 = rt.stack.pop_i128()?;
            rt.stack.push_i128(v1 & !v2)?;
        }

        SimdInstruction::I8x16AllTrue => {
            let v = rt.stack.pop_i128()?.to_le_bytes();
            let res = v.iter().all(|b| *b != 0);
            rt.stack.push_i32(if res { 1 } else { 0 })?;
        }

        SimdInstruction::I16x8AllTrue => {
            let v = vec_to_i16x8(rt.stack.pop_i128()?);
            let res = v.iter().all(|b| *b != 0);
            rt.stack.push_i32(if res { 1 } else { 0 })?;
        }

        SimdInstruction::I32x4AllTrue => {
            let v = vec_to_i32x4(rt.stack.pop_i128()?);
            let res = v.iter().all(|b| *b != 0);
            rt.stack.push_i32(if res { 1 } else { 0 })?;
        }

        SimdInstruction::I64x2AllTrue => {
            let v = vec_to_i64x2(rt.stack.pop_i128()?);
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
            for i in &mut v {
                *i = i.wrapping_shl(shift as u32);
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

        SimdInstruction::I8x16Neg => i8x16_lanewise_map(rt, |i| ((i as i8).wrapping_neg()) as u8)?,

        SimdInstruction::I16x8Neg => i16x8_lanewise_map(rt, |i| i.wrapping_neg())?,

        SimdInstruction::I32x4Neg => i32x4_lanewise_map(rt, |i| i.wrapping_neg())?,

        SimdInstruction::F32x4Mul => {
            f32x4_lanewise_zip_map(rt, |f1, f2| canonicalize_f32_nan(f1 * f2))?
        }

        SimdInstruction::F32x4Div => {
            f32x4_lanewise_zip_map(rt, |f1, f2| canonicalize_f32_nan(f1 / f2))?
        }

        SimdInstruction::F32x4Neg => f32x4_lanewise_map(rt, |f| -f)?,

        SimdInstruction::F32x4Sqrt => f32x4_lanewise_map(rt, |f| canonicalize_f32_nan(f.sqrt()))?,

        SimdInstruction::F32x4Add => {
            f32x4_lanewise_zip_map(rt, |f1, f2| canonicalize_f32_nan(f1 + f2))?
        }

        SimdInstruction::F32x4Sub => {
            f32x4_lanewise_zip_map(rt, |f1, f2| canonicalize_f32_nan(f1 - f2))?
        }

        SimdInstruction::F32x4Abs => f32x4_lanewise_map(rt, f32::abs)?,

        SimdInstruction::F64x2Mul => {
            f64x2_lanewise_zip_map(rt, |f1, f2| canonicalize_f64_nan(f1 * f2))?
        }

        SimdInstruction::F64x2Div => {
            f64x2_lanewise_zip_map(rt, |f1, f2| canonicalize_f64_nan(f1 / f2))?
        }

        SimdInstruction::F64x2Neg => f64x2_lanewise_map(rt, f64::neg)?,

        SimdInstruction::F64x2Sqrt => f64x2_lanewise_map(rt, |f| canonicalize_f64_nan(f.sqrt()))?,

        SimdInstruction::F64x2Add => {
            f64x2_lanewise_zip_map(rt, |f1, f2| canonicalize_f64_nan(f1 + f2))?
        }

        SimdInstruction::F64x2Sub => {
            f64x2_lanewise_zip_map(rt, |f1, f2| canonicalize_f64_nan(f1 - f2))?
        }

        SimdInstruction::F64x2Abs => f64x2_lanewise_map(rt, f64::abs)?,

        SimdInstruction::I32x4TruncSatF32x4S => {
            let v = rt.stack.pop_i128()?;
            let fs = vec_to_f32x4(v);
            let is: [i32; 4] = fs.map(super::i32_trunc_sat_s_f32);
            rt.stack.push_i128(i32x4_to_vec(is))?;
        }

        SimdInstruction::I32x4TruncSatF32x4U => {
            let v = rt.stack.pop_i128()?;
            let fs = vec_to_f32x4(v);
            let is: [i32; 4] = fs.map(super::i32_trunc_sat_u_f32);
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

        SimdInstruction::I8x16Abs => i8x16_lanewise_map(rt, |i| (i as i8).wrapping_abs() as u8)?,

        SimdInstruction::I16x8Add => i16x8_lanewise_zip_map(rt, |i1, i2| i1.wrapping_add(i2))?,

        SimdInstruction::I16x8Sub => i16x8_lanewise_zip_map(rt, |i1, i2| i1.wrapping_sub(i2))?,

        SimdInstruction::I16x8Mul => i16x8_lanewise_zip_map(rt, |i1, i2| i1.wrapping_mul(i2))?,

        SimdInstruction::I16x8Abs => i16x8_lanewise_map(rt, |i| i.wrapping_abs())?,

        SimdInstruction::I32x4Add => {
            let v2 = vec_to_i32x4(rt.stack.pop_i128()?);
            let mut v1 = vec_to_i32x4(rt.stack.pop_i128()?);
            for i in 0..4 {
                v1[i] = v1[i].wrapping_add(v2[i]);
            }
            rt.stack.push_i128(i32x4_to_vec(v1))?;
        }

        SimdInstruction::I64x2Mul => i64x2_lanewise_zip_map(rt, |i1, i2| i1.wrapping_mul(i2))?,

        SimdInstruction::I64x2Abs => i64x2_lanewise_map(rt, i64::wrapping_abs)?,

        SimdInstruction::I64x2Neg => i64x2_lanewise_map(rt, i64::wrapping_neg)?,

        SimdInstruction::I64x2Sub => i64x2_lanewise_zip_map(rt, |i1, i2| i1.wrapping_sub(i2))?,

        SimdInstruction::I64x2Add => i64x2_lanewise_zip_map(rt, |i1, i2| i1.wrapping_add(i2))?,

        SimdInstruction::I16x8ExtendHighI8x16S => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in v {
                let i16 = (*i) as i8 as i16;
                res.extend_from_slice(&i16.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I16x8ExtendHighI8x16U => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in v {
                let u16 = (*i) as u16;
                res.extend_from_slice(&u16.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I16x8ExtendLowI8x16S => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in v {
                let i16 = (*i) as i8 as i16;
                res.extend_from_slice(&i16.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I16x8ExtendLowI8x16U => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in v {
                let u16 = (*i) as u16;
                res.extend_from_slice(&u16.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I32x4Mul => i32x4_lanewise_zip_map(rt, |i1, i2| i1.wrapping_mul(i2))?,

        SimdInstruction::I32x4Sub => i32x4_lanewise_zip_map(rt, |i1, i2| i1.wrapping_sub(i2))?,

        SimdInstruction::I32x4Abs => i32x4_lanewise_map(rt, i32::wrapping_abs)?,

        SimdInstruction::I32x4ExtendHighI16x8S => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in 0..4 {
                let i32 = i16::from_le_bytes([v[i * 2], v[i * 2 + 1]]) as i32;
                res.extend_from_slice(&i32.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I32x4ExtendHighI16x8U => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in 0..4 {
                let u32 = u16::from_le_bytes([v[i * 2], v[i * 2 + 1]]) as u32;
                res.extend_from_slice(&u32.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I32x4ExtendLowI16x8S => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in 0..4 {
                let i32 = i16::from_le_bytes([v[i * 2], v[i * 2 + 1]]) as i32;
                res.extend_from_slice(&i32.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I32x4ExtendLowI16x8U => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in 0..4 {
                let u32 = u16::from_le_bytes([v[i * 2], v[i * 2 + 1]]) as u32;
                res.extend_from_slice(&u32.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I64x2ExtendHighI32x4S => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in 0..2 {
                let i64 =
                    i32::from_le_bytes([v[i * 4], v[i * 4 + 1], v[i * 4 + 2], v[i * 4 + 3]]) as i64;
                res.extend_from_slice(&i64.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I64x2ExtendHighI32x4U => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in 0..2 {
                let u64 =
                    u32::from_le_bytes([v[i * 4], v[i * 4 + 1], v[i * 4 + 2], v[i * 4 + 3]]) as u64;
                res.extend_from_slice(&u64.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I64x2ExtendLowI32x4S => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in 0..2 {
                let i64 =
                    i32::from_le_bytes([v[i * 4], v[i * 4 + 1], v[i * 4 + 2], v[i * 4 + 3]]) as i64;
                res.extend_from_slice(&i64.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I64x2ExtendLowI32x4U => {
            let v = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let mut res: Vec<u8> = Vec::with_capacity(16);
            for i in 0..2 {
                let u64 =
                    u32::from_le_bytes([v[i * 4], v[i * 4 + 1], v[i * 4 + 2], v[i * 4 + 3]]) as u64;
                res.extend_from_slice(&u64.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::V128Load32Zero(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 4)?;

            let u32 = mem.load_32(addr)?;

            let mut res: Vec<u8> = Vec::with_capacity(16);
            res.extend_from_slice(&u32.to_le_bytes());
            res.extend_from_slice(&[0u8; 12]);

            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::V128Load64Zero(MemArg { align: _, offset }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 4)?;

            let u64 = mem.load_64(addr)?;

            let mut res: Vec<u8> = Vec::with_capacity(16);
            res.extend_from_slice(&u64.to_le_bytes());
            res.extend_from_slice(&[0u8; 8]);

            rt.stack
                .push_i128(i128::from_le_bytes(res.try_into().unwrap()))?;
        }

        SimdInstruction::I32x4ExtractLane(lane) => {
            let v = rt.stack.pop_i128()?.to_le_bytes();
            let lane = usize::from(lane);
            let i = i32::from_le_bytes(v[lane * 4..lane * 4 + 4].try_into().unwrap());
            rt.stack.push_i32(i)?;
        }

        SimdInstruction::F32x4ExtractLane(lane) => {
            let v = rt.stack.pop_i128()?.to_le_bytes();
            let lane = usize::from(lane);
            let i = f32::from_le_bytes(v[lane * 4..lane * 4 + 4].try_into().unwrap());
            rt.stack.push_f32(i)?;
        }

        SimdInstruction::I64x2ExtractLane(lane) => {
            let v = rt.stack.pop_i128()?.to_le_bytes();
            let lane = usize::from(lane);
            let i = i64::from_le_bytes(v[lane * 8..lane * 8 + 8].try_into().unwrap());
            rt.stack.push_i64(i)?;
        }

        SimdInstruction::F64x2ExtractLane(lane) => {
            let v = rt.stack.pop_i128()?.to_le_bytes();
            let lane = usize::from(lane);
            let i = f64::from_le_bytes(v[lane * 8..lane * 8 + 8].try_into().unwrap());
            rt.stack.push_f64(i)?;
        }

        SimdInstruction::I8x16ReplaceLane(lane) => {
            let i = rt.stack.pop_i32()? as u8;
            let mut v = rt.stack.pop_i128()?.to_le_bytes();
            v[usize::from(lane)] = i;
            rt.stack.push_i128(i128::from_le_bytes(v))?;
        }

        SimdInstruction::I16x8ReplaceLane(lane) => {
            let i = (rt.stack.pop_i32()? as u16).to_le_bytes();
            let mut v = rt.stack.pop_i128()?.to_le_bytes();
            let lane = usize::from(lane);
            v[lane * 2 + 0] = i[0];
            v[lane * 2 + 1] = i[1];
            rt.stack.push_i128(i128::from_le_bytes(v))?;
        }

        SimdInstruction::I32x4ReplaceLane(lane) => {
            let i = rt.stack.pop_i32()?.to_le_bytes();
            let mut v = rt.stack.pop_i128()?.to_le_bytes();
            let lane = usize::from(lane);
            v[lane * 4 + 0] = i[0];
            v[lane * 4 + 1] = i[1];
            v[lane * 4 + 2] = i[2];
            v[lane * 4 + 3] = i[3];
            rt.stack.push_i128(i128::from_le_bytes(v))?;
        }

        SimdInstruction::F32x4ReplaceLane(lane) => {
            let i = rt.stack.pop_f32()?.to_le_bytes();
            let mut v = rt.stack.pop_i128()?.to_le_bytes();
            let lane = usize::from(lane);
            v[lane * 4 + 0] = i[0];
            v[lane * 4 + 1] = i[1];
            v[lane * 4 + 2] = i[2];
            v[lane * 4 + 3] = i[3];
            rt.stack.push_i128(i128::from_le_bytes(v))?;
        }

        SimdInstruction::I64x2ReplaceLane(lane) => {
            let i = rt.stack.pop_i64()?.to_le_bytes();
            let mut v = rt.stack.pop_i128()?.to_le_bytes();
            let lane = usize::from(lane);
            v[lane * 8 + 0] = i[0];
            v[lane * 8 + 1] = i[1];
            v[lane * 8 + 2] = i[2];
            v[lane * 8 + 3] = i[3];
            v[lane * 8 + 4] = i[4];
            v[lane * 8 + 5] = i[5];
            v[lane * 8 + 6] = i[6];
            v[lane * 8 + 7] = i[7];
            rt.stack.push_i128(i128::from_le_bytes(v))?;
        }

        SimdInstruction::F64x2ReplaceLane(lane) => {
            let i = rt.stack.pop_f64()?.to_le_bytes();
            let mut v = rt.stack.pop_i128()?.to_le_bytes();
            let lane = usize::from(lane);
            v[lane * 8 + 0] = i[0];
            v[lane * 8 + 1] = i[1];
            v[lane * 8 + 2] = i[2];
            v[lane * 8 + 3] = i[3];
            v[lane * 8 + 4] = i[4];
            v[lane * 8 + 5] = i[5];
            v[lane * 8 + 6] = i[6];
            v[lane * 8 + 7] = i[7];
            rt.stack.push_i128(i128::from_le_bytes(v))?;
        }

        SimdInstruction::V128AnyTrue => {
            let v = rt.stack.pop_i128()?;
            rt.stack.push_i32(if v == 0 { 0 } else { 1 })?;
        }

        SimdInstruction::I8x16Eq => i8x16_rel(rt, |i1, i2| i1 == i2)?,

        SimdInstruction::I8x16Ne => i8x16_rel(rt, |i1, i2| i1 != i2)?,

        SimdInstruction::I8x16LtS => i8x16_rel(rt, |i1, i2| (i1 as i8) < (i2 as i8))?,

        SimdInstruction::I8x16LtU => i8x16_rel(rt, |i1, i2| i1 < i2)?,

        SimdInstruction::I8x16LeS => i8x16_rel(rt, |i1, i2| (i1 as i8) <= (i2 as i8))?,

        SimdInstruction::I8x16LeU => i8x16_rel(rt, |i1, i2| i1 <= i2)?,

        SimdInstruction::I8x16GtS => i8x16_rel(rt, |i1, i2| (i1 as i8) > (i2 as i8))?,

        SimdInstruction::I8x16GtU => i8x16_rel(rt, |i1, i2| i1 > i2)?,

        SimdInstruction::I8x16GeS => i8x16_rel(rt, |i1, i2| (i1 as i8) >= (i2 as i8))?,

        SimdInstruction::I8x16GeU => i8x16_rel(rt, |i1, i2| i1 >= i2)?,

        SimdInstruction::I16x8Eq => i16x8_rel(rt, |i1, i2| i1 == i2)?,

        SimdInstruction::I16x8Ne => i16x8_rel(rt, |i1, i2| i1 != i2)?,

        SimdInstruction::I16x8LtS => i16x8_rel(rt, |i1, i2| i1 < i2)?,

        SimdInstruction::I16x8LtU => i16x8_rel(rt, |i1, i2| (i1 as u16) < (i2 as u16))?,

        SimdInstruction::I16x8LeS => i16x8_rel(rt, |i1, i2| i1 <= i2)?,

        SimdInstruction::I16x8LeU => i16x8_rel(rt, |i1, i2| (i1 as u16) <= (i2 as u16))?,

        SimdInstruction::I16x8GtS => i16x8_rel(rt, |i1, i2| i1 > i2)?,

        SimdInstruction::I16x8GtU => i16x8_rel(rt, |i1, i2| (i1 as u16) > (i2 as u16))?,

        SimdInstruction::I16x8GeS => i16x8_rel(rt, |i1, i2| i1 >= i2)?,

        SimdInstruction::I16x8GeU => i16x8_rel(rt, |i1, i2| (i1 as u16) >= (i2 as u16))?,

        SimdInstruction::I32x4Eq => i32x4_rel(rt, |i1, i2| i1 == i2)?,

        SimdInstruction::I32x4Ne => i32x4_rel(rt, |i1, i2| i1 != i2)?,

        SimdInstruction::I32x4LtS => i32x4_rel(rt, |i1, i2| i1 < i2)?,

        SimdInstruction::I32x4LtU => i32x4_rel(rt, |i1, i2| (i1 as u32) < (i2 as u32))?,

        SimdInstruction::I32x4LeS => i32x4_rel(rt, |i1, i2| i1 <= i2)?,

        SimdInstruction::I32x4LeU => i32x4_rel(rt, |i1, i2| (i1 as u32) <= (i2 as u32))?,

        SimdInstruction::I32x4GtS => i32x4_rel(rt, |i1, i2| i1 > i2)?,

        SimdInstruction::I32x4GtU => i32x4_rel(rt, |i1, i2| (i1 as u32) > (i2 as u32))?,

        SimdInstruction::I32x4GeS => i32x4_rel(rt, |i1, i2| i1 >= i2)?,

        SimdInstruction::I32x4GeU => i32x4_rel(rt, |i1, i2| (i1 as u32) >= (i2 as u32))?,

        SimdInstruction::F32x4Eq => f32x4_rel(rt, |i1, i2| i1 == i2)?,

        SimdInstruction::F32x4Ne => f32x4_rel(rt, |i1, i2| i1 != i2)?,

        SimdInstruction::F32x4Lt => f32x4_rel(rt, |i1, i2| i1 < i2)?,

        SimdInstruction::F32x4Le => f32x4_rel(rt, |i1, i2| i1 <= i2)?,

        SimdInstruction::F32x4Gt => f32x4_rel(rt, |i1, i2| i1 > i2)?,

        SimdInstruction::F32x4Ge => f32x4_rel(rt, |i1, i2| i1 >= i2)?,

        SimdInstruction::I64x2Eq => i64x2_rel(rt, |i1, i2| i1 == i2)?,

        SimdInstruction::I64x2Ne => i64x2_rel(rt, |i1, i2| i1 != i2)?,

        SimdInstruction::I64x2LtS => i64x2_rel(rt, |i1, i2| i1 < i2)?,

        SimdInstruction::I64x2LeS => i64x2_rel(rt, |i1, i2| i1 <= i2)?,

        SimdInstruction::I64x2GtS => i64x2_rel(rt, |i1, i2| i1 > i2)?,

        SimdInstruction::I64x2GeS => i64x2_rel(rt, |i1, i2| i1 >= i2)?,

        SimdInstruction::F64x2Eq => f64x2_rel(rt, |i1, i2| i1 == i2)?,

        SimdInstruction::F64x2Ne => f64x2_rel(rt, |i1, i2| i1 != i2)?,

        SimdInstruction::F64x2Lt => f64x2_rel(rt, |i1, i2| i1 < i2)?,

        SimdInstruction::F64x2Le => f64x2_rel(rt, |i1, i2| i1 <= i2)?,

        SimdInstruction::F64x2Gt => f64x2_rel(rt, |i1, i2| i1 > i2)?,

        SimdInstruction::F64x2Ge => f64x2_rel(rt, |i1, i2| i1 >= i2)?,

        SimdInstruction::F32x4PMax => {
            f32x4_lanewise_zip_map(rt, |f1, f2| if f1 < f2 { f2 } else { f1 })?
        }

        SimdInstruction::F32x4Max => f32x4_lanewise_zip_map(rt, super::f32_max)?,

        SimdInstruction::F32x4PMin => {
            f32x4_lanewise_zip_map(rt, |f1, f2| if f2 < f1 { f2 } else { f1 })?
        }

        SimdInstruction::F32x4Min => f32x4_lanewise_zip_map(rt, super::f32_min)?,

        SimdInstruction::F64x2PMax => {
            f64x2_lanewise_zip_map(rt, |f1, f2| if f1 < f2 { f2 } else { f1 })?
        }

        SimdInstruction::F64x2Max => f64x2_lanewise_zip_map(rt, super::f64_max)?,

        SimdInstruction::F64x2PMin => {
            f64x2_lanewise_zip_map(rt, |f1, f2| if f2 < f1 { f2 } else { f1 })?
        }

        SimdInstruction::F64x2Min => f64x2_lanewise_zip_map(rt, super::f64_min)?,

        SimdInstruction::I8x16Splat => {
            let i = rt.stack.pop_i32()? as i8 as u8;
            rt.stack.push_i128(i128::from_le_bytes([i; 16]))?
        }

        SimdInstruction::I16x8Splat => {
            let i = rt.stack.pop_i32()? as i16;
            let mut bytes = Vec::with_capacity(16);
            for _ in 0..8 {
                bytes.extend_from_slice(&i.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(bytes.try_into().unwrap()))?
        }

        SimdInstruction::I32x4Splat => {
            let i = rt.stack.pop_i32()?;
            let mut bytes = Vec::with_capacity(16);
            for _ in 0..4 {
                bytes.extend_from_slice(&i.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(bytes.try_into().unwrap()))?
        }

        SimdInstruction::F32x4Splat => {
            let i = rt.stack.pop_f32()?;
            let mut bytes = Vec::with_capacity(16);
            for _ in 0..4 {
                bytes.extend_from_slice(&i.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(bytes.try_into().unwrap()))?
        }

        SimdInstruction::I64x2Splat => {
            let i = rt.stack.pop_i64()?;
            let mut bytes = Vec::with_capacity(16);
            for _ in 0..2 {
                bytes.extend_from_slice(&i.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(bytes.try_into().unwrap()))?
        }

        SimdInstruction::F64x2Splat => {
            let i = rt.stack.pop_f64()?;
            let mut bytes = Vec::with_capacity(16);
            for _ in 0..2 {
                bytes.extend_from_slice(&i.to_le_bytes());
            }
            rt.stack
                .push_i128(i128::from_le_bytes(bytes.try_into().unwrap()))?
        }

        SimdInstruction::F32x4Ceil => f32x4_lanewise_map(rt, |f| canonicalize_f32_nan(f.ceil()))?,

        SimdInstruction::F32x4Floor => f32x4_lanewise_map(rt, |f| canonicalize_f32_nan(f.floor()))?,

        SimdInstruction::F32x4Trunc => f32x4_lanewise_map(rt, |f| canonicalize_f32_nan(f.trunc()))?,

        SimdInstruction::F32x4Nearest => f32x4_lanewise_map(rt, super::f32_nearest)?,

        SimdInstruction::F64x2Ceil => f64x2_lanewise_map(rt, |f| canonicalize_f64_nan(f.ceil()))?,

        SimdInstruction::F64x2Floor => f64x2_lanewise_map(rt, |f| canonicalize_f64_nan(f.floor()))?,

        SimdInstruction::F64x2Trunc => f64x2_lanewise_map(rt, |f| canonicalize_f64_nan(f.trunc()))?,

        SimdInstruction::F64x2Nearest => f64x2_lanewise_map(rt, super::f64_nearest)?,

        SimdInstruction::I16x8Shl => {
            let shift = (rt.stack.pop_i32()? % 16) as u32;
            let v = vec_to_i16x8(rt.stack.pop_i128()?).map(|i| i.wrapping_shl(shift));
            rt.stack.push_i128(i16x8_to_vec(v))?
        }

        SimdInstruction::I32x4Shl => {
            let shift = (rt.stack.pop_i32()? % 32) as u32;
            let v = vec_to_i32x4(rt.stack.pop_i128()?).map(|i| i.wrapping_shl(shift));
            rt.stack.push_i128(i32x4_to_vec(v))?
        }

        SimdInstruction::I64x2Shl => {
            let shift = (rt.stack.pop_i32()? % 64) as u32;
            let v = vec_to_i64x2(rt.stack.pop_i128()?).map(|i| i.wrapping_shl(shift));
            rt.stack.push_i128(i64x2_to_vec(v))?
        }

        SimdInstruction::I8x16ShrS => {
            let shift = (rt.stack.pop_i32()? % 8) as u32;
            let v = rt
                .stack
                .pop_i128()?
                .to_le_bytes()
                .map(|i| (i as i8).wrapping_shr(shift) as u8);
            rt.stack.push_i128(i128::from_le_bytes(v))?
        }

        SimdInstruction::I8x16ShrU => {
            let shift = (rt.stack.pop_i32()? % 8) as u32;
            let v = rt
                .stack
                .pop_i128()?
                .to_le_bytes()
                .map(|i| i.wrapping_shr(shift));
            rt.stack.push_i128(i128::from_le_bytes(v))?
        }

        SimdInstruction::I16x8ShrS => {
            let shift = (rt.stack.pop_i32()? % 16) as u32;
            let v = vec_to_i16x8(rt.stack.pop_i128()?).map(|i| i.wrapping_shr(shift));
            rt.stack.push_i128(i16x8_to_vec(v))?
        }

        SimdInstruction::I16x8ShrU => {
            let shift = (rt.stack.pop_i32()? % 16) as u32;
            let v =
                vec_to_i16x8(rt.stack.pop_i128()?).map(|i| (i as u16).wrapping_shr(shift) as i16);
            rt.stack.push_i128(i16x8_to_vec(v))?
        }

        SimdInstruction::I32x4ShrS => {
            let shift = (rt.stack.pop_i32()? % 32) as u32;
            let v = vec_to_i32x4(rt.stack.pop_i128()?).map(|i| i.wrapping_shr(shift));
            rt.stack.push_i128(i32x4_to_vec(v))?
        }

        SimdInstruction::I32x4ShrU => {
            let shift = (rt.stack.pop_i32()? % 32) as u32;
            let v =
                vec_to_i32x4(rt.stack.pop_i128()?).map(|i| (i as u32).wrapping_shr(shift) as i32);
            rt.stack.push_i128(i32x4_to_vec(v))?
        }

        SimdInstruction::I64x2ShrS => {
            let shift = (rt.stack.pop_i32()? % 64) as u32;
            let v = vec_to_i64x2(rt.stack.pop_i128()?).map(|i| i.wrapping_shr(shift));
            rt.stack.push_i128(i64x2_to_vec(v))?
        }

        SimdInstruction::I64x2ShrU => {
            let shift = (rt.stack.pop_i32()? % 64) as u32;
            let v =
                vec_to_i64x2(rt.stack.pop_i128()?).map(|i| (i as u64).wrapping_shr(shift) as i64);
            rt.stack.push_i128(i64x2_to_vec(v))?
        }

        SimdInstruction::I8x16MinS => {
            i8x16_lanewise_zip_map(rt, |i1, i2| (i1 as i8).min(i2 as i8) as u8)?
        }

        SimdInstruction::I8x16MinU => i8x16_lanewise_zip_map(rt, |i1, i2| i1.min(i2))?,

        SimdInstruction::I8x16MaxS => {
            i8x16_lanewise_zip_map(rt, |i1, i2| (i1 as i8).max(i2 as i8) as u8)?
        }

        SimdInstruction::I8x16MaxU => i8x16_lanewise_zip_map(rt, |i1, i2| i1.max(i2))?,

        SimdInstruction::I32x4MinS => i32x4_lanewise_zip_map(rt, |i1, i2| i1.min(i2))?,

        SimdInstruction::I32x4MinU => {
            i32x4_lanewise_zip_map(rt, |i1, i2| (i1 as u32).min(i2 as u32) as i32)?
        }

        SimdInstruction::I32x4MaxS => i32x4_lanewise_zip_map(rt, |i1, i2| i1.max(i2))?,

        SimdInstruction::I32x4MaxU => {
            i32x4_lanewise_zip_map(rt, |i1, i2| (i1 as u32).max(i2 as u32) as i32)?
        }

        SimdInstruction::I16x8MinS => i16x8_lanewise_zip_map(rt, |i1, i2| i1.min(i2))?,

        SimdInstruction::I16x8MinU => {
            i16x8_lanewise_zip_map(rt, |i1, i2| (i1 as u16).min(i2 as u16) as i16)?
        }

        SimdInstruction::I16x8MaxS => i16x8_lanewise_zip_map(rt, |i1, i2| i1.max(i2))?,

        SimdInstruction::I16x8MaxU => {
            i16x8_lanewise_zip_map(rt, |i1, i2| (i1 as u16).max(i2 as u16) as i16)?
        }

        SimdInstruction::I8x16Popcnt => i8x16_lanewise_map(rt, |i| i.count_ones() as u8)?,

        SimdInstruction::I8x16AddSatS => {
            i8x16_lanewise_zip_map(rt, |i1, i2| (i1 as i8).saturating_add(i2 as i8) as u8)?
        }

        SimdInstruction::I8x16AddSatU => {
            i8x16_lanewise_zip_map(rt, |i1, i2| i1.saturating_add(i2))?
        }

        SimdInstruction::I8x16SubSatS => {
            i8x16_lanewise_zip_map(rt, |i1, i2| (i1 as i8).saturating_sub(i2 as i8) as u8)?
        }

        SimdInstruction::I8x16SubSatU => {
            i8x16_lanewise_zip_map(rt, |i1, i2| i1.saturating_sub(i2))?
        }

        SimdInstruction::I16x8AddSatS => {
            i16x8_lanewise_zip_map(rt, |i1, i2| i1.saturating_add(i2))?
        }

        SimdInstruction::I16x8AddSatU => {
            i16x8_lanewise_zip_map(rt, |i1, i2| (i1 as u16).saturating_add(i2 as u16) as i16)?
        }

        SimdInstruction::I16x8SubSatS => {
            i16x8_lanewise_zip_map(rt, |i1, i2| i1.saturating_sub(i2))?
        }

        SimdInstruction::I16x8SubSatU => {
            i16x8_lanewise_zip_map(rt, |i1, i2| (i1 as u16).saturating_sub(i2 as u16) as i16)?
        }

        SimdInstruction::I8x16Shuffle(lane_indices) => {
            let v2 = rt.stack.pop_i128()?.to_le_bytes();
            let v1 = rt.stack.pop_i128()?.to_le_bytes();
            let mut ret = [0u8; 16];
            for i in 0..16 {
                let lane_idx = usize::from(lane_indices[i]);
                if lane_idx < 16 {
                    ret[i] = v1[lane_idx];
                } else {
                    ret[i] = v2[lane_idx - 16];
                }
            }
            rt.stack.push_i128(i128::from_le_bytes(ret))?
        }

        SimdInstruction::I8x16Bitmask => {
            let v = rt.stack.pop_i128()?.to_le_bytes();
            let mut result: i32 = 0;
            for i in 0..16 {
                if (v[i] as i8) < 0 {
                    result |= 1 << i;
                }
            }
            rt.stack.push_i32(result)?
        }

        SimdInstruction::I16x8Bitmask => {
            let v = vec_to_i16x8(rt.stack.pop_i128()?);
            let mut result: i32 = 0;
            for i in 0..8 {
                if v[i] < 0 {
                    result |= 1 << i;
                }
            }
            rt.stack.push_i32(result)?
        }

        SimdInstruction::I32x4Bitmask => {
            let v = vec_to_i32x4(rt.stack.pop_i128()?);
            let mut result: i32 = 0;
            for i in 0..4 {
                if v[i] < 0 {
                    result |= 1 << i;
                }
            }
            rt.stack.push_i32(result)?
        }

        SimdInstruction::I64x2Bitmask => {
            let v = vec_to_i64x2(rt.stack.pop_i128()?);
            let mut result: i32 = 0;
            for i in 0..2 {
                if v[i] < 0 {
                    result |= 1 << i;
                }
            }
            rt.stack.push_i32(result)?
        }

        SimdInstruction::I8x16NarrowI16x8S => {
            let v2 = vec_to_i16x8(rt.stack.pop_i128()?);
            let v1 = vec_to_i16x8(rt.stack.pop_i128()?);
            let mut result = [0u8; 16];

            fn sat(i: i16) -> i8 {
                i.clamp(i16::from(i8::MIN), i16::from(i8::MAX)) as i8
            }

            for i in 0..8 {
                result[i] = sat(v1[i]) as u8;
            }

            for i in 0..8 {
                result[8 + i] = sat(v2[i]) as u8;
            }

            rt.stack.push_i128(i128::from_le_bytes(result))?
        }

        SimdInstruction::I8x16NarrowI16x8U => {
            let v2 = vec_to_i16x8(rt.stack.pop_i128()?);
            let v1 = vec_to_i16x8(rt.stack.pop_i128()?);
            let mut result = [0u8; 16];

            fn sat(i: i16) -> u8 {
                i.clamp(0, i16::from(u8::MAX)) as u8
            }

            for i in 0..8 {
                result[i] = sat(v1[i]);
            }

            for i in 0..8 {
                result[8 + i] = sat(v2[i]);
            }

            rt.stack.push_i128(i128::from_le_bytes(result))?
        }

        SimdInstruction::I16x8NarrowI32x4S => {
            let v2 = vec_to_i32x4(rt.stack.pop_i128()?);
            let v1 = vec_to_i32x4(rt.stack.pop_i128()?);
            let mut result = [0i16; 8];

            fn sat(i: i32) -> i16 {
                i.clamp(i32::from(i16::MIN), i32::from(i16::MAX)) as i16
            }

            for i in 0..4 {
                result[i] = sat(v1[i]);
            }

            for i in 0..4 {
                result[4 + i] = sat(v2[i]);
            }

            rt.stack.push_i128(i16x8_to_vec(result))?
        }

        SimdInstruction::I16x8NarrowI32x4U => {
            let v2 = vec_to_i32x4(rt.stack.pop_i128()?);
            let v1 = vec_to_i32x4(rt.stack.pop_i128()?);
            let mut result = [0i16; 8];

            fn sat(i: i32) -> u16 {
                i.clamp(0, i32::from(u16::MAX)) as u16
            }

            for i in 0..4 {
                result[i] = sat(v1[i]) as i16;
            }

            for i in 0..4 {
                result[4 + i] = sat(v2[i]) as i16;
            }

            rt.stack.push_i128(i16x8_to_vec(result))?
        }

        SimdInstruction::I8x16AvgrU => {
            i8x16_lanewise_zip_map(rt, |i1, i2| ((i16::from(i1) + i16::from(i2) + 1) / 2) as u8)?
        }

        SimdInstruction::I16x8AvgrU => i16x8_lanewise_zip_map(rt, |i1, i2| {
            ((u32::from(i1 as u16) + u32::from(i2 as u16) + 1) / 2) as u16 as i16
        })?,

        SimdInstruction::I16x8ExtaddPairwiseI8x16S => {
            let v = rt.stack.pop_i128()?.to_le_bytes();
            let mut ret = [0i16; 8];

            for i in 0..8 {
                ret[i] = (v[i * 2] as i8 as i16) + (v[i * 2 + 1] as i8 as i16);
            }

            rt.stack.push_i128(i16x8_to_vec(ret))?
        }

        SimdInstruction::I16x8ExtaddPairwiseI8x16U => {
            let v = rt.stack.pop_i128()?.to_le_bytes();
            let mut ret = [0i16; 8];

            for i in 0..8 {
                ret[i] = ((v[i * 2] as u16) + (v[i * 2 + 1] as u16)) as i16;
            }

            rt.stack.push_i128(i16x8_to_vec(ret))?
        }

        SimdInstruction::I32x4ExtaddPairwiseI16x8S => {
            let v = vec_to_i16x8(rt.stack.pop_i128()?);
            let mut ret = [0i32; 4];

            for i in 0..4 {
                ret[i] = (v[i * 2] as i32) + (v[i * 2 + 1] as i32);
            }

            rt.stack.push_i128(i32x4_to_vec(ret))?
        }

        SimdInstruction::I32x4ExtaddPairwiseI16x8U => {
            let v = vec_to_i16x8(rt.stack.pop_i128()?);
            let mut ret = [0i32; 4];

            for i in 0..4 {
                ret[i] = ((v[i * 2] as u16 as u32) + (v[i * 2 + 1] as u16 as u32)) as i32;
            }

            rt.stack.push_i128(i32x4_to_vec(ret))?
        }

        SimdInstruction::I16x8Q15MulrSatS => i16x8_lanewise_zip_map(rt, |i1, i2| {
            // let q15mulr_sat_s x y =
            //   (* mul x64 y64 can overflow int64 when both are int32 min, but this is only
            //    * used by i16x8, so we are fine for now. *)
            //   assert (Rep.bitwidth < 32);
            //   let x64 = Rep.to_int64 x in
            //   let y64 = Rep.to_int64 y in
            //   saturate_s (Rep.of_int64 Int64.((shift_right (add (mul x64 y64) 0x4000L) 15)))

            fn sat(i: i32) -> i16 {
                i.clamp(i32::from(i16::MIN), i32::from(i16::MAX)) as i16
            }

            sat((i32::from(i1) * i32::from(i2) + 0x4000) >> 15)
        })?,

        SimdInstruction::I16x8ExtmulLowI8x16S => {
            let v2 = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let v1 = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let mut ret = [0i16; 8];
            for i in 0..8 {
                ret[i] = (v1[i] as i8 as i16).wrapping_mul(v2[i] as i8 as i16);
            }
            rt.stack.push_i128(i16x8_to_vec(ret))?
        }

        SimdInstruction::I16x8ExtmulHighI8x16S => {
            let v2 = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let v1 = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let mut ret = [0i16; 8];
            for i in 0..8 {
                ret[i] = (v1[i] as i8 as i16).wrapping_mul(v2[i] as i8 as i16);
            }
            rt.stack.push_i128(i16x8_to_vec(ret))?
        }

        SimdInstruction::I16x8ExtmulLowI8x16U => {
            let v2 = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let v1 = &rt.stack.pop_i128()?.to_le_bytes()[..8];
            let mut ret = [0i16; 8];
            for i in 0..8 {
                ret[i] = ((v1[i] as u16).wrapping_mul(v2[i] as u16)) as i16;
            }
            rt.stack.push_i128(i16x8_to_vec(ret))?
        }

        SimdInstruction::I16x8ExtmulHighI8x16U => {
            let v2 = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let v1 = &rt.stack.pop_i128()?.to_le_bytes()[8..];
            let mut ret = [0i16; 8];
            for i in 0..8 {
                ret[i] = ((v1[i] as u16).wrapping_mul(v2[i] as u16)) as i16;
            }
            rt.stack.push_i128(i16x8_to_vec(ret))?
        }

        SimdInstruction::I32x4ExtmulLowI16x8S => {
            let v2 = &vec_to_i16x8(rt.stack.pop_i128()?)[..4];
            let v1 = &vec_to_i16x8(rt.stack.pop_i128()?)[..4];
            let mut ret = [0i32; 4];
            for i in 0..4 {
                ret[i] = (v1[i] as i32) * (v2[i] as i32);
            }
            rt.stack.push_i128(i32x4_to_vec(ret))?
        }

        SimdInstruction::I32x4ExtmulHighI16x8S => {
            let v2 = &vec_to_i16x8(rt.stack.pop_i128()?)[4..];
            let v1 = &vec_to_i16x8(rt.stack.pop_i128()?)[4..];
            let mut ret = [0i32; 4];
            for i in 0..4 {
                ret[i] = (v1[i] as i32) * (v2[i] as i32);
            }
            rt.stack.push_i128(i32x4_to_vec(ret))?
        }

        SimdInstruction::I32x4ExtmulLowI16x8U => {
            let v2 = &vec_to_i16x8(rt.stack.pop_i128()?)[..4];
            let v1 = &vec_to_i16x8(rt.stack.pop_i128()?)[..4];
            let mut ret = [0i32; 4];
            for i in 0..4 {
                ret[i] = ((v1[i] as u16 as u32) * (v2[i] as u16 as u32)) as i32;
            }
            rt.stack.push_i128(i32x4_to_vec(ret))?
        }

        SimdInstruction::I32x4ExtmulHighI16x8U => {
            let v2 = &vec_to_i16x8(rt.stack.pop_i128()?)[4..];
            let v1 = &vec_to_i16x8(rt.stack.pop_i128()?)[4..];
            let mut ret = [0i32; 4];
            for i in 0..4 {
                ret[i] = ((v1[i] as u16 as u32) * (v2[i] as u16 as u32)) as i32;
            }
            rt.stack.push_i128(i32x4_to_vec(ret))?
        }

        SimdInstruction::I64x2ExtmulLowI32x4S => {
            let v2 = &vec_to_i32x4(rt.stack.pop_i128()?)[..2];
            let v1 = &vec_to_i32x4(rt.stack.pop_i128()?)[..2];
            let mut ret = [0i64; 2];
            for i in 0..2 {
                ret[i] = (v1[i] as i64) * (v2[i] as i64);
            }
            rt.stack.push_i128(i64x2_to_vec(ret))?
        }

        SimdInstruction::I64x2ExtmulHighI32x4S => {
            let v2 = &vec_to_i32x4(rt.stack.pop_i128()?)[2..];
            let v1 = &vec_to_i32x4(rt.stack.pop_i128()?)[2..];
            let mut ret = [0i64; 2];
            for i in 0..2 {
                ret[i] = (v1[i] as i64) * (v2[i] as i64);
            }
            rt.stack.push_i128(i64x2_to_vec(ret))?
        }

        SimdInstruction::I64x2ExtmulLowI32x4U => {
            let v2 = &vec_to_i32x4(rt.stack.pop_i128()?)[..2];
            let v1 = &vec_to_i32x4(rt.stack.pop_i128()?)[..2];
            let mut ret = [0i64; 2];
            for i in 0..2 {
                ret[i] = ((v1[i] as u32 as u64) * (v2[i] as u32 as u64)) as i64;
            }
            rt.stack.push_i128(i64x2_to_vec(ret))?
        }

        SimdInstruction::I64x2ExtmulHighI32x4U => {
            let v2 = &vec_to_i32x4(rt.stack.pop_i128()?)[2..];
            let v1 = &vec_to_i32x4(rt.stack.pop_i128()?)[2..];
            let mut ret = [0i64; 2];
            for i in 0..2 {
                ret[i] = ((v1[i] as u32 as u64) * (v2[i] as u32 as u64)) as i64;
            }
            rt.stack.push_i128(i64x2_to_vec(ret))?
        }

        SimdInstruction::F32x4ConvertI32x4S => {
            let v = vec_to_i32x4(rt.stack.pop_i128()?).map(|i| i as f32);
            rt.stack.push_i128(f32x4_to_vec(v))?
        }

        SimdInstruction::F64x2ConvertLowI32x4S => {
            let v = vec_to_i32x4(rt.stack.pop_i128()?);
            let res = [v[0] as f64, v[1] as f64];
            rt.stack.push_i128(f64x2_to_vec(res))?
        }

        SimdInstruction::F64x2ConvertLowI32x4U => {
            let v = vec_to_i32x4(rt.stack.pop_i128()?);
            let res = [v[0] as u32 as f64, v[1] as u32 as f64];
            rt.stack.push_i128(f64x2_to_vec(res))?
        }

        SimdInstruction::I32x4DotI16x8S => {
            let v2 = vec_to_i16x8(rt.stack.pop_i128()?).map(i32::from);
            let mut v1 = vec_to_i16x8(rt.stack.pop_i128()?).map(i32::from);
            let mut res = [0i32; 4];
            for i in 0..8 {
                v1[i] *= v2[i];
            }
            for i in 0..4 {
                res[i] = v1[2 * i].wrapping_add(v1[2 * i + 1]);
            }
            rt.stack.push_i128(i32x4_to_vec(res))?
        }

        SimdInstruction::I32x4TruncSatF64x2SZero => {
            let v = vec_to_f64x2(rt.stack.pop_i128()?);
            rt.stack
                .push_i128(i32x4_to_vec([v[0] as i32, v[1] as i32, 0, 0]))?
        }

        SimdInstruction::I32x4TruncSatF64x2UZero => {
            let v = vec_to_f64x2(rt.stack.pop_i128()?);
            rt.stack.push_i128(i32x4_to_vec([
                super::i32_trunc_sat_s_f64(v[0]),
                super::i32_trunc_sat_s_f64(v[1]),
                0,
                0,
            ]))?
        }

        SimdInstruction::F32x4DemoteF64x2Zero => {
            let v = vec_to_f64x2(rt.stack.pop_i128()?);
            rt.stack.push_i128(f32x4_to_vec([
                canonicalize_f32_nan(v[0] as f32),
                canonicalize_f32_nan(v[1] as f32),
                0f32,
                0f32,
            ]))?
        }

        SimdInstruction::F64x2PromoteLowF32x4 => {
            let v = vec_to_f32x4(rt.stack.pop_i128()?);
            rt.stack.push_i128(f64x2_to_vec([
                canonicalize_f64_nan(v[0] as f64),
                canonicalize_f64_nan(v[1] as f64),
            ]))?
        }
    }

    rt.ip += 1;
    Ok(())
}

fn i8x16_rel<F>(rt: &mut Runtime, rel: F) -> Result<()>
where
    F: Fn(u8, u8) -> bool,
{
    let v2 = rt.stack.pop_i128()?.to_le_bytes();
    let v1 = rt.stack.pop_i128()?.to_le_bytes();
    let mut ret = [0u8; 16];
    for i in 0..16 {
        ret[i] = if rel(v1[i], v2[i]) { 0xFF } else { 0 };
    }
    rt.stack.push_i128(i128::from_le_bytes(ret))
}

fn i16x8_rel<F>(rt: &mut Runtime, rel: F) -> Result<()>
where
    F: Fn(i16, i16) -> bool,
{
    let v2 = vec_to_i16x8(rt.stack.pop_i128()?);
    let v1 = vec_to_i16x8(rt.stack.pop_i128()?);
    let mut ret = [0i16; 8];
    for i in 0..8 {
        ret[i] = if rel(v1[i], v2[i]) {
            0xFFFFu16 as i16
        } else {
            0i16
        };
    }
    rt.stack.push_i128(i16x8_to_vec(ret))
}

fn i32x4_rel<F>(rt: &mut Runtime, rel: F) -> Result<()>
where
    F: Fn(i32, i32) -> bool,
{
    let v2 = vec_to_i32x4(rt.stack.pop_i128()?);
    let v1 = vec_to_i32x4(rt.stack.pop_i128()?);
    let mut ret = [0i32; 4];
    for i in 0..4 {
        ret[i] = if rel(v1[i], v2[i]) {
            0xFFFFFFFFu32 as i32
        } else {
            0i32
        };
    }
    rt.stack.push_i128(i32x4_to_vec(ret))
}

fn f32x4_rel<F>(rt: &mut Runtime, rel: F) -> Result<()>
where
    F: Fn(f32, f32) -> bool,
{
    let v2 = vec_to_f32x4(rt.stack.pop_i128()?);
    let v1 = vec_to_f32x4(rt.stack.pop_i128()?);
    let mut ret = [0f32; 4];
    for i in 0..4 {
        ret[i] = if rel(v1[i], v2[i]) {
            f32::from_le_bytes([0xFF; 4])
        } else {
            0f32
        };
    }
    rt.stack.push_i128(f32x4_to_vec(ret))
}

fn i64x2_rel<F>(rt: &mut Runtime, rel: F) -> Result<()>
where
    F: Fn(i64, i64) -> bool,
{
    let v2 = vec_to_i64x2(rt.stack.pop_i128()?);
    let v1 = vec_to_i64x2(rt.stack.pop_i128()?);
    let mut ret = [0i64; 2];
    for i in 0..2 {
        ret[i] = if rel(v1[i], v2[i]) {
            0xFFFFFFFFFFFFFFFFu64 as i64
        } else {
            0i64
        };
    }
    rt.stack.push_i128(i64x2_to_vec(ret))
}

fn f64x2_rel<F>(rt: &mut Runtime, rel: F) -> Result<()>
where
    F: Fn(f64, f64) -> bool,
{
    let v2 = vec_to_f64x2(rt.stack.pop_i128()?);
    let v1 = vec_to_f64x2(rt.stack.pop_i128()?);
    let mut ret = [0f64; 2];
    for i in 0..2 {
        ret[i] = if rel(v1[i], v2[i]) {
            f64::from_le_bytes([0xFF; 8])
        } else {
            0f64
        };
    }
    rt.stack.push_i128(f64x2_to_vec(ret))
}

fn i8x16_lanewise_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(u8) -> u8,
{
    let mut v = rt.stack.pop_i128()?.to_le_bytes();
    for i in &mut v {
        *i = f(*i);
    }
    rt.stack.push_i128(i128::from_le_bytes(v))
}

fn i8x16_lanewise_zip_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(u8, u8) -> u8,
{
    let v2 = rt.stack.pop_i128()?.to_le_bytes();
    let v1 = rt.stack.pop_i128()?.to_le_bytes();
    let mut ret = [0; 16];
    for i in 0..16 {
        ret[i] = f(v1[i], v2[i]);
    }
    rt.stack.push_i128(i128::from_le_bytes(ret))
}

fn i16x8_lanewise_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(i16) -> i16,
{
    let mut v = vec_to_i16x8(rt.stack.pop_i128()?);
    for i in &mut v {
        *i = f(*i);
    }
    rt.stack.push_i128(i16x8_to_vec(v))
}

fn i16x8_lanewise_zip_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(i16, i16) -> i16,
{
    let v2 = vec_to_i16x8(rt.stack.pop_i128()?);
    let v1 = vec_to_i16x8(rt.stack.pop_i128()?);
    let mut ret = [0i16; 8];
    for i in 0..8 {
        ret[i] = f(v1[i], v2[i]);
    }
    rt.stack.push_i128(i16x8_to_vec(ret))
}

fn i32x4_lanewise_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(i32) -> i32,
{
    let mut v = vec_to_i32x4(rt.stack.pop_i128()?);
    for i in &mut v {
        *i = f(*i);
    }
    rt.stack.push_i128(i32x4_to_vec(v))
}

fn i32x4_lanewise_zip_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(i32, i32) -> i32,
{
    let v2 = vec_to_i32x4(rt.stack.pop_i128()?);
    let v1 = vec_to_i32x4(rt.stack.pop_i128()?);
    let mut ret = [0i32; 4];
    for i in 0..4 {
        ret[i] = f(v1[i], v2[i]);
    }
    rt.stack.push_i128(i32x4_to_vec(ret))
}

fn f32x4_lanewise_zip_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(f32, f32) -> f32,
{
    let v2 = vec_to_f32x4(rt.stack.pop_i128()?);
    let v1 = vec_to_f32x4(rt.stack.pop_i128()?);
    let mut ret = [0.0f32; 4];
    for i in 0..4 {
        ret[i] = f(v1[i], v2[i]);
    }
    rt.stack.push_i128(f32x4_to_vec(ret))
}

fn f32x4_lanewise_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(f32) -> f32,
{
    let mut v = vec_to_f32x4(rt.stack.pop_i128()?);
    for i in &mut v {
        *i = f(*i);
    }
    rt.stack.push_i128(f32x4_to_vec(v))
}

fn i64x2_lanewise_zip_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(i64, i64) -> i64,
{
    let v2 = vec_to_i64x2(rt.stack.pop_i128()?);
    let v1 = vec_to_i64x2(rt.stack.pop_i128()?);
    let mut ret = [0i64; 2];
    for i in 0..2 {
        ret[i] = f(v1[i], v2[i]);
    }
    rt.stack.push_i128(i64x2_to_vec(ret))
}

fn i64x2_lanewise_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(i64) -> i64,
{
    let mut v = vec_to_i64x2(rt.stack.pop_i128()?);
    for i in &mut v {
        *i = f(*i);
    }
    rt.stack.push_i128(i64x2_to_vec(v))
}

fn f64x2_lanewise_zip_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(f64, f64) -> f64,
{
    let v2 = vec_to_f64x2(rt.stack.pop_i128()?);
    let v1 = vec_to_f64x2(rt.stack.pop_i128()?);
    let mut ret = [0.0f64; 2];
    for i in 0..2 {
        ret[i] = f(v1[i], v2[i]);
    }
    rt.stack.push_i128(f64x2_to_vec(ret))
}

fn f64x2_lanewise_map<F>(rt: &mut Runtime, f: F) -> Result<()>
where
    F: Fn(f64) -> f64,
{
    let mut v = vec_to_f64x2(rt.stack.pop_i128()?);
    for i in &mut v {
        *i = f(*i);
    }
    rt.stack.push_i128(f64x2_to_vec(v))
}

fn vec_to_i16x8(v: i128) -> [i16; 8] {
    let bytes = v.to_le_bytes();
    [
        i16::from_le_bytes([bytes[0], bytes[1]]),
        i16::from_le_bytes([bytes[2], bytes[3]]),
        i16::from_le_bytes([bytes[4], bytes[5]]),
        i16::from_le_bytes([bytes[6], bytes[7]]),
        i16::from_le_bytes([bytes[8], bytes[9]]),
        i16::from_le_bytes([bytes[10], bytes[11]]),
        i16::from_le_bytes([bytes[12], bytes[13]]),
        i16::from_le_bytes([bytes[14], bytes[15]]),
    ]
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

fn vec_to_f64x2(v: i128) -> [f64; 2] {
    let bytes = v.to_le_bytes();
    [
        f64::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]),
        f64::from_le_bytes([
            bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15],
        ]),
    ]
}

fn vec_to_i64x2(v: i128) -> [i64; 2] {
    let bytes = v.to_le_bytes();
    [
        i64::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]),
        i64::from_le_bytes([
            bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15],
        ]),
    ]
}

fn i16x8_to_vec(is: [i16; 8]) -> i128 {
    let [i1, i2, i3, i4, i5, i6, i7, i8] = is;
    let i1_bytes = i1.to_le_bytes();
    let i2_bytes = i2.to_le_bytes();
    let i3_bytes = i3.to_le_bytes();
    let i4_bytes = i4.to_le_bytes();
    let i5_bytes = i5.to_le_bytes();
    let i6_bytes = i6.to_le_bytes();
    let i7_bytes = i7.to_le_bytes();
    let i8_bytes = i8.to_le_bytes();
    i128::from_le_bytes([
        i1_bytes[0],
        i1_bytes[1],
        i2_bytes[0],
        i2_bytes[1],
        i3_bytes[0],
        i3_bytes[1],
        i4_bytes[0],
        i4_bytes[1],
        i5_bytes[0],
        i5_bytes[1],
        i6_bytes[0],
        i6_bytes[1],
        i7_bytes[0],
        i7_bytes[1],
        i8_bytes[0],
        i8_bytes[1],
    ])
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

fn f64x2_to_vec(fs: [f64; 2]) -> i128 {
    let [f1, f2] = fs;
    let f1_bytes = f1.to_le_bytes();
    let f2_bytes = f2.to_le_bytes();
    i128::from_le_bytes([
        f1_bytes[0],
        f1_bytes[1],
        f1_bytes[2],
        f1_bytes[3],
        f1_bytes[4],
        f1_bytes[5],
        f1_bytes[6],
        f1_bytes[7],
        f2_bytes[0],
        f2_bytes[1],
        f2_bytes[2],
        f2_bytes[3],
        f2_bytes[4],
        f2_bytes[5],
        f2_bytes[6],
        f2_bytes[7],
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

fn i64x2_to_vec(is: [i64; 2]) -> i128 {
    let [i1, i2] = is;
    let i1_bytes = i1.to_le_bytes();
    let i2_bytes = i2.to_le_bytes();
    i128::from_le_bytes([
        i1_bytes[0],
        i1_bytes[1],
        i1_bytes[2],
        i1_bytes[3],
        i1_bytes[4],
        i1_bytes[5],
        i1_bytes[6],
        i1_bytes[7],
        i2_bytes[0],
        i2_bytes[1],
        i2_bytes[2],
        i2_bytes[3],
        i2_bytes[4],
        i2_bytes[5],
        i2_bytes[6],
        i2_bytes[7],
    ])
}
