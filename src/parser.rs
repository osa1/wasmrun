mod internal;
pub mod types;

use internal::*;
pub use internal::{ParseError, Result};
pub use types::*;

pub fn parse(bytes: &[u8]) -> Result<()> {
    let mut parser = Parser::new(bytes);

    // Magic number: "\0wasm"
    parser.consume_const(&[0x00, 0x61, 0x73, 0x6D])?;

    // Version number: 1
    parser.consume_const(&[0x01, 0x00, 0x00, 0x00])?;

    skip_customsecs(&mut parser)?;

    let fun_tys = parse_type_section(&mut parser)?;

    println!("{:#?}", fun_tys);

    skip_customsecs(&mut parser)?;

    let imports = parse_imports(&mut parser)?;

    println!("{:?}", imports);

    skip_customsecs(&mut parser)?;

    let funs = parse_fun_section(&mut parser)?;

    println!("{:?}", funs);

    skip_customsecs(&mut parser)?;

    let tables = parse_table_section(&mut parser)?;

    println!("tables: {:?}", tables);

    skip_customsecs(&mut parser)?;

    let mems = parse_mem_section(&mut parser)?;

    println!("mems: {:?}", mems);

    skip_customsecs(&mut parser)?;

    let globals = parse_globals(&mut parser)?;

    println!("globals: {:?}", mems);

    Ok(())
}

fn parse_section<'a, A>(
    parser: &mut Parser<'a>,
    section_ty: u8,
    parse: &dyn Fn(&mut Parser<'a>) -> Result<A>,
) -> Result<A> {
    match parser.byte() {
        Ok(ty) if ty == section_ty => {}
        Ok(other) => {
            return Err(ParseError::UnexpectedSectionType {
                expected: section_ty,
                found: other,
            });
        }
        Err(err) => {
            return Err(err);
        }
    }

    parser.skip(1)?;

    let section_size = parser.consume_uleb128()?;
    let section_data = parser.consume(section_size as usize)?;

    let mut section_parser = Parser::new(section_data);
    let ret = parse(&mut section_parser)?;

    if !section_parser.all_consumed() {
        return Err(ParseError::SectionNotEmpty {
            remains: section_parser.get_bytes().to_owned(),
        });
    }

    Ok(ret)
}

fn parse_type_section<'a>(parser: &mut Parser<'a>) -> Result<Vec<FuncType>> {
    parse_section(parser, 1, &|parser: &mut Parser<'a>| {
        let vec_len = parser.consume_uleb128()?;

        println!("vec len={}", vec_len);

        let mut fun_tys: Vec<FuncType> = Vec::with_capacity(vec_len as usize);
        for _ in 0..vec_len {
            parser.consume_const(&[0x60])?;
            let args = parse_resulttype(parser)?;
            let ret = parse_resulttype(parser)?;
            fun_tys.push(FuncType { args, ret });
        }

        Ok(fun_tys)
    })
}

fn parse_imports<'a>(parser: &mut Parser<'a>) -> Result<Vec<Import>> {
    parse_section(parser, 2, &|parser| {
        let vec_len = parser.consume_uleb128()?;

        println!("vec len={}", vec_len);

        let mut imports: Vec<Import> = Vec::with_capacity(vec_len as usize);
        for _ in 0..vec_len {
            let module = parse_name(parser)?;
            let name = parse_name(parser)?;
            let desc = parse_importdesc(parser)?;
            imports.push(Import { module, name, desc });
        }

        Ok(imports)
    })
}

fn parse_fun_section<'a>(parser: &mut Parser<'a>) -> Result<Vec<TypeIdx>> {
    parse_section(parser, 3, &|parser| {
        let vec_len = parser.consume_uleb128()?;
        let mut vec = Vec::with_capacity(vec_len as usize);

        for _ in 0..vec_len {
            vec.push(parser.consume_uleb128()?);
        }

        Ok(vec)
    })
}

fn parse_table_section<'a>(parser: &mut Parser<'a>) -> Result<Vec<Limits>> {
    parse_section(parser, 4, &|parser| {
        let vec_len = parser.consume_uleb128()?;
        let mut vec = Vec::with_capacity(vec_len as usize);

        for _ in 0..vec_len {
            parser.consume_const(&[0x70])?;
            vec.push(parse_limits(parser)?);
        }

        Ok(vec)
    })
}

fn parse_mem_section<'a>(parser: &mut Parser<'a>) -> Result<Vec<Limits>> {
    parse_section(parser, 5, &|parser| {
        let vec_len = parser.consume_uleb128()?;
        let mut vec = Vec::with_capacity(vec_len as usize);

        for _ in 0..vec_len {
            vec.push(parse_limits(parser)?);
        }

        Ok(vec)
    })
}

fn parse_globals<'a>(parser: &mut Parser<'a>) -> Result<Vec<Global>> {
    parse_section(parser, 6, &|parser| {
        let vec_len = parser.consume_uleb128()?;
        let mut vec = Vec::with_capacity(vec_len as usize);

        for _ in 0..vec_len {
            let ty = parse_global_type(parser)?;
            let expr = parse_expr(parser)?;
            vec.push(Global { ty, expr });
        }

        Ok(vec)
    })
}

fn parse_expr<'a>(parser: &mut Parser<'a>) -> Result<Expr> {
    let mut instrs = vec![];
    while parser.byte()? != 0x0B {
        instrs.push(parse_instr(parser)?);
    }
    parser.skip(1)?; // consume 0x0B
    Ok(Expr { instrs })
}

fn parse_instr<'a>(parser: &mut Parser<'a>) -> Result<Instruction> {
    use Instruction::*;
    match parser.consume_byte()? {
        // Control instructions
        0x00 => Ok(Unreachable),
        0x01 => Ok(Nop),
        0x02 => Ok(Block(parse_block(parser)?)),
        0x03 => Ok(Loop(parse_block(parser)?)),
        0x04 => Ok(If(parse_if(parser)?)),
        0x0C => Ok(Br(parser.consume_uleb128()?)),
        0x0D => Ok(BrIf(parser.consume_uleb128()?)),
        0x0E => Ok(BrTable(parse_br_table(parser)?)),
        0x0F => Ok(Return),
        0x10 => Ok(Call(parser.consume_uleb128()?)),
        0x11 => {
            let type_idx = parser.consume_uleb128()?;
            parser.consume_const(&[0x00])?;
            Ok(CallIndirect(type_idx))
        }

        // Parametric instructions
        0x1A => Ok(Drop),
        0x1B => Ok(Select),

        // Variable instructions
        0x20 => Ok(LocalGet(parser.consume_uleb128()?)),
        0x21 => Ok(LocalSet(parser.consume_uleb128()?)),
        0x22 => Ok(LocalTee(parser.consume_uleb128()?)),
        0x23 => Ok(GlobalGet(parser.consume_uleb128()?)),
        0x24 => Ok(GlobalSet(parser.consume_uleb128()?)),

        // Memory instructions
        0x28 => Ok(I32Load(parse_memarg(parser)?)),
        0x29 => Ok(I64Load(parse_memarg(parser)?)),
        0x2A => Ok(F32Load(parse_memarg(parser)?)),
        0x2B => Ok(F64Load(parse_memarg(parser)?)),
        0x2C => Ok(I32Load8s(parse_memarg(parser)?)),
        0x2D => Ok(I32Load8u(parse_memarg(parser)?)),
        0x2E => Ok(I32Load16s(parse_memarg(parser)?)),
        0x2F => Ok(I32Load16u(parse_memarg(parser)?)),
        0x30 => Ok(I64Load8s(parse_memarg(parser)?)),
        0x31 => Ok(I64Load8u(parse_memarg(parser)?)),
        0x32 => Ok(I64Load16s(parse_memarg(parser)?)),
        0x33 => Ok(I64Load16u(parse_memarg(parser)?)),
        0x34 => Ok(I64Load32s(parse_memarg(parser)?)),
        0x35 => Ok(I64Load32u(parse_memarg(parser)?)),
        0x36 => Ok(I32Store(parse_memarg(parser)?)),
        0x37 => Ok(I64Store(parse_memarg(parser)?)),
        0x38 => Ok(F32Store(parse_memarg(parser)?)),
        0x39 => Ok(F64Store(parse_memarg(parser)?)),
        0x3A => Ok(I32Store8(parse_memarg(parser)?)),
        0x3B => Ok(I32Store16(parse_memarg(parser)?)),
        0x3C => Ok(I64Store8(parse_memarg(parser)?)),
        0x3D => Ok(I64Store16(parse_memarg(parser)?)),
        0x3E => Ok(I64Store32(parse_memarg(parser)?)),
        0x3F => {
            parser.consume_const(&[0x00])?;
            Ok(MemorySize)
        }
        0x40 => {
            parser.consume_const(&[0x00])?;
            Ok(MemoryGrow)
        }

        // Numeric instructions
        0x41 => {
            let b1 = parser.consume_byte()?;
            let b2 = parser.consume_byte()?;
            let b3 = parser.consume_byte()?;
            let b4 = parser.consume_byte()?;
            Ok(I32Const(i32::from_le_bytes([b1, b2, b3, b4])))
        }
        0x42 => {
            let b1 = parser.consume_byte()?;
            let b2 = parser.consume_byte()?;
            let b3 = parser.consume_byte()?;
            let b4 = parser.consume_byte()?;
            let b5 = parser.consume_byte()?;
            let b6 = parser.consume_byte()?;
            let b7 = parser.consume_byte()?;
            let b8 = parser.consume_byte()?;
            Ok(I64Const(i64::from_le_bytes([
                b1, b2, b3, b4, b5, b6, b7, b8,
            ])))
        }
        0x43 => {
            let b1 = parser.consume_byte()?;
            let b2 = parser.consume_byte()?;
            let b3 = parser.consume_byte()?;
            let b4 = parser.consume_byte()?;
            Ok(F32Const(f32::from_le_bytes([b1, b2, b3, b4])))
        }
        0x44 => {
            let b1 = parser.consume_byte()?;
            let b2 = parser.consume_byte()?;
            let b3 = parser.consume_byte()?;
            let b4 = parser.consume_byte()?;
            let b5 = parser.consume_byte()?;
            let b6 = parser.consume_byte()?;
            let b7 = parser.consume_byte()?;
            let b8 = parser.consume_byte()?;
            Ok(F64Const(f64::from_le_bytes([
                b1, b2, b3, b4, b5, b6, b7, b8,
            ])))
        }
        0x45 => Ok(I32Eqz),
        0x46 => Ok(I32Eq),
        0x47 => Ok(I32Ne),
        0x48 => Ok(I32Lt_s),
        0x49 => Ok(I32Lt_u),
        0x4A => Ok(I32Gt_s),
        0x4B => Ok(I32Gt_u),
        0x4C => Ok(I32Le_s),
        0x4D => Ok(I32Le_u),
        0x4E => Ok(I32Ge_s),
        0x4F => Ok(I32Ge_u),
        0x50 => Ok(I64Eqz),
        0x51 => Ok(I64Eq),
        0x52 => Ok(I64Ne),
        0x53 => Ok(I64Lt_s),
        0x54 => Ok(I64Lt_u),
        0x55 => Ok(I64Gt_s),
        0x56 => Ok(I64Gt_u),
        0x57 => Ok(I64Le_s),
        0x58 => Ok(I64Le_u),
        0x59 => Ok(I64Ge_s),
        0x5A => Ok(I64Ge_u),
        0x5B => Ok(F32Eq),
        0x5C => Ok(F32Ne),
        0x5D => Ok(F32Lt),
        0x5E => Ok(F32Gt),
        0x5F => Ok(F32Le),
        0x60 => Ok(F32Ge),
        0x61 => Ok(F64Eq),
        0x62 => Ok(F64Ne),
        0x63 => Ok(F64Lt),
        0x64 => Ok(F64Gt),
        0x65 => Ok(F64Le),
        0x66 => Ok(F64Ge),
        0x67 => Ok(I32Clz),
        0x68 => Ok(I32Ctz),
        0x69 => Ok(I32Popcnt),
        0x6A => Ok(I32Add),
        0x6B => Ok(I32Sub),
        0x6C => Ok(I32Mul),
        0x6D => Ok(I32Div_s),
        0x6E => Ok(I32Div_u),
        0x6F => Ok(I32Rem_s),
        0x70 => Ok(I32Rem_u),
        0x71 => Ok(I32And),
        0x72 => Ok(I32Or),
        0x73 => Ok(I32Xor),
        0x74 => Ok(I32Shl),
        0x75 => Ok(I32Shr_s),
        0x76 => Ok(I32Shr_u),
        0x77 => Ok(I32Rotl),
        0x78 => Ok(I32Rotr),
        0x79 => Ok(I64Clz),
        0x7A => Ok(I64Ctz),
        0x7B => Ok(I64Popcnt),
        0x7C => Ok(I64Add),
        0x7D => Ok(I64Sub),
        0x7E => Ok(I64Mul),
        0x7F => Ok(I64Div_s),
        0x80 => Ok(I64Div_u),
        0x81 => Ok(I64Rem_s),
        0x82 => Ok(I64Rem_u),
        0x83 => Ok(I64And),
        0x84 => Ok(I64Or),
        0x85 => Ok(I64Xor),
        0x86 => Ok(I64Shl),
        0x87 => Ok(I64Shr_s),
        0x88 => Ok(I64Shr_u),
        0x89 => Ok(I64Rotl),
        0x8A => Ok(I64Rotr),
        0x8B => Ok(F32Abs),
        0x8C => Ok(F32Neg),
        0x8D => Ok(F32Ceil),
        0x8E => Ok(F32Floor),
        0x8F => Ok(F32Trunc),
        0x90 => Ok(F32Nearest),
        0x91 => Ok(F32Sqrt),
        0x92 => Ok(F32Add),
        0x93 => Ok(F32Sub),
        0x94 => Ok(F32Mul),
        0x95 => Ok(F32Div),
        0x96 => Ok(F32Min),
        0x97 => Ok(F32Max),
        0x98 => Ok(F32Copysign),
        0x99 => Ok(F64Abs),
        0x9A => Ok(F64Neg),
        0x9B => Ok(F64Ceil),
        0x9C => Ok(F64Floor),
        0x9D => Ok(F64Trunc),
        0x9E => Ok(F64Nearest),
        0x9F => Ok(F64Sqrt),
        0xA0 => Ok(F64Add),
        0xA1 => Ok(F64Sub),
        0xA2 => Ok(F64Mul),
        0xA3 => Ok(F64Div),
        0xA4 => Ok(F64Min),
        0xA5 => Ok(F64Max),
        0xA6 => Ok(F64Copysign),
        0xA7 => Ok(I32Wrapi64),
        0xA8 => Ok(I32Truncf32_s),
        0xA9 => Ok(I32Truncf32_u),
        0xAA => Ok(I32Truncf64_s),
        0xAB => Ok(I32Truncf64_u),
        0xAC => Ok(I64Extendi32_s),
        0xAD => Ok(I64Extendi32_u),
        0xAE => Ok(I64Truncf32_s),
        0xAF => Ok(I64Truncf32_u),
        0xB0 => Ok(I64Truncf64_s),
        0xB1 => Ok(I64Truncf64_u),
        0xB2 => Ok(F32Converti32_s),
        0xB3 => Ok(F32Converti32_u),
        0xB4 => Ok(F32Converti64_s),
        0xB5 => Ok(F32Converti64_u),
        0xB6 => Ok(F32Demotef64),
        0xB7 => Ok(F64Converti32_s),
        0xB8 => Ok(F64Converti32_u),
        0xB9 => Ok(F64Converti64_s),
        0xBA => Ok(F64Converti64_u),
        0xBB => Ok(F64Promotef32),
        0xBC => Ok(I32Reinterpretf32),
        0xBD => Ok(I64Reinterpretf64),
        0xBE => Ok(F32Reinterpreti32),
        0xBF => Ok(F64Reinterpreti64),
        0xC0 => Ok(I32Extend8_s),
        0xC1 => Ok(I32Extend16_s),
        0xC2 => Ok(I64Extend8_s),
        0xC3 => Ok(I64Extend16_s),
        0xC4 => Ok(I64Extend32_s),
        0xFC => {
            match parser.consume_byte()? {
                0x00 => Ok(I32TruncSatf32_s),
                0x01 => Ok(I32TruncSatf32_u),
                0x02 => Ok(I32TruncSatf64_s),
                0x03 => Ok(I32TruncSatf64_u),
                0x04 => Ok(I64TruncSatf32_s),
                0x05 => Ok(I64TruncSatf32_u),
                0x06 => Ok(I64TruncSatf64_s),
                0x07 => Ok(I64TruncSatf64_u),
                _other => Err(ParseError::UnexpectedOpCode(0xFC)), // TODO show 'other'
            }
        }
        other => Err(ParseError::UnexpectedOpCode(other)),
    }
}

fn parse_memarg<'a>(parser: &mut Parser<'a>) -> Result<MemArg> {
    let align = parser.consume_uleb128()?;
    let offset = parser.consume_uleb128()?;
    Ok(MemArg { align, offset })
}

fn parse_block<'a>(parser: &mut Parser<'a>) -> Result<Block> {
    let ty = parse_block_type(parser)?;
    let mut instrs = vec![];
    while parser.byte()? != 0x0B {
        instrs.push(parse_instr(parser)?);
    }
    parser.skip(1)?; // consume 0x0B
    Ok(Block { ty, instrs })
}

fn parse_if<'a>(parser: &mut Parser<'a>) -> Result<If> {
    let ty = parse_block_type(parser)?;
    let mut then_instrs = vec![];
    let mut else_instrs = vec![];

    loop {
        let byte = parser.byte()?;
        if byte == 0x05 {
            parser.skip(1)?; // consume 0x05
            while parser.byte()? != 0x0B {
                else_instrs.push(parse_instr(parser)?);
            }
            break;
        } else if byte == 0x0B {
            break;
        } else {
            then_instrs.push(parse_instr(parser)?);
        }
    }

    parser.skip(1)?; // consume 0x0B

    Ok(If {
        ty,
        then_instrs,
        else_instrs,
    })
}

fn parse_br_table<'a>(parser: &mut Parser<'a>) -> Result<BrTable> {
    let vec_len = parser.consume_uleb128()?;
    let mut tbl = Vec::with_capacity(vec_len as usize);

    for _ in 0..vec_len {
        tbl.push(parser.consume_uleb128()?);
    }
    let def = parser.consume_uleb128()?;

    Ok(BrTable { tbl, def })
}

fn parse_block_type<'a>(parser: &mut Parser<'a>) -> Result<BlockType> {
    match parser.byte()? {
        0x40 => Ok(BlockType::Empty),
        0x7F => Ok(BlockType::ValType(ValType::I32)),
        0x7E => Ok(BlockType::ValType(ValType::I64)),
        0x7D => Ok(BlockType::ValType(ValType::F32)),
        0x7C => Ok(BlockType::ValType(ValType::F64)),
        _ => Ok(BlockType::TypeIdx(parser.consume_sleb128()? as u32)), // TODO: sleb128 is probably buggy
    }
}

// Ignore 'customsec'. Custom sections are for debug info or other third-party extensions, not
// important for semantics.
//
// Returns whether it skipped a customsec.
fn skip_customsec<'a>(parser: &mut Parser<'a>) -> Result<bool> {
    match parser.byte() {
        Ok(0) => {}
        _ => {
            return Ok(false);
        }
    }

    parser.skip(1)?;

    let section_size = parser.consume_uleb128()?;
    parser.skip(section_size as usize)?;

    Ok(true)
}

// Skip multiple 'customsecs'.
fn skip_customsecs<'a>(parser: &mut Parser<'a>) -> Result<()> {
    while skip_customsec(parser)? {}
    Ok(())
}

fn parse_resulttype<'a>(parser: &mut Parser<'a>) -> Result<ResultType> {
    let vec_len = parser.consume_uleb128()?;
    let mut result_type = Vec::with_capacity(vec_len as usize);

    for _ in 0..vec_len {
        result_type.push(parse_valtype(parser)?);
    }

    Ok(result_type)
}

fn parse_valtype<'a>(parser: &mut Parser<'a>) -> Result<ValType> {
    let byte = parser.consume_byte()?;

    match byte {
        0x7F => Ok(ValType::I32),
        0x7E => Ok(ValType::I64),
        0x7D => Ok(ValType::F32),
        0x7C => Ok(ValType::F64),
        _ => Err(ParseError::UnexpectedValType { found: byte }),
    }
}

fn parse_importdesc<'a>(parser: &mut Parser<'a>) -> Result<ImportDesc> {
    match parser.consume_byte()? {
        0x00 => Ok(ImportDesc::Func(parser.consume_uleb128()?)),
        0x01 => {
            parser.consume_const(&[0x70])?;
            Ok(ImportDesc::Table(parse_limits(parser)?))
        }
        0x02 => Ok(ImportDesc::MemType(parse_limits(parser)?)),
        0x03 => Ok(ImportDesc::Global(parse_global_type(parser)?)),
        other => todo!(),
    }
}

fn parse_global_type<'a>(parser: &mut Parser<'a>) -> Result<GlobalType> {
    let ty = parse_valtype(parser)?;
    let mut_ = parse_mutability(parser)?;
    Ok(GlobalType { ty, mut_ })
}

fn parse_limits<'a>(parser: &mut Parser<'a>) -> Result<Limits> {
    match parser.consume_byte()? {
        0x00 => Ok(Limits {
            min: parser.consume_uleb128()?,
            max: None,
        }),
        0x01 => {
            let min = parser.consume_uleb128()?;
            let max = parser.consume_uleb128()?;
            Ok(Limits {
                min,
                max: Some(max),
            })
        }
        other => todo!(),
    }
}

fn parse_mutability<'a>(parser: &mut Parser<'a>) -> Result<Mutability> {
    match parser.consume_byte()? {
        0x00 => Ok(Mutability::Const),
        0x01 => Ok(Mutability::Var),
        other => todo!(),
    }
}

fn parse_name<'a>(parser: &mut Parser<'a>) -> Result<String> {
    let str_size = parser.consume_uleb128()?;
    let str_bytes = parser.consume(str_size as usize)?;
    match ::std::str::from_utf8(str_bytes) {
        Ok(str) => Ok(str.to_owned()),
        Err(err) => Err(ParseError::Utf8Error(err)),
    }
}
