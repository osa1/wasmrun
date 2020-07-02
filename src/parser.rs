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

        _ => todo!(),
    }
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
