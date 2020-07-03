#![allow(non_camel_case_types)]

pub type TypeIdx = u32;
pub type FuncIdx = u32;
pub type TableIdx = u32;
pub type MemIdx = u32;
pub type GlobalIdx = u32;
pub type LocalIdx = u32;
pub type LabelIdx = u32;

#[derive(Debug)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

pub type ResultType = Vec<ValType>;

#[derive(Debug)]
pub struct FuncType {
    pub args: ResultType,
    pub ret: ResultType,
}

#[derive(Debug)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub desc: ImportDesc,
}

#[derive(Debug)]
pub enum ImportDesc {
    Func(FuncIdx),
    Table(Limits),
    MemType(Limits),
    Global(GlobalType),
}

#[derive(Debug)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

#[derive(Debug)]
pub struct GlobalType {
    pub ty: ValType,
    pub mut_: Mutability,
}

#[derive(Debug)]
pub enum Mutability {
    Const,
    Var,
}

#[derive(Debug)]
pub struct Global {
    pub ty: GlobalType,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Expr {
    pub instrs: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    //
    // Control instructions
    //

    // 0x00
    Unreachable,
    // 0x01
    Nop,
    // 0x02
    Block(Block),
    // 0x03
    Loop(Block),
    // 0x04
    If(If),
    // 0x0C
    Br(LabelIdx),
    // 0x0D
    BrIf(LabelIdx),
    // 0x0E
    BrTable(BrTable),
    // 0x0F
    Return,
    // 0x10
    Call(FuncIdx),
    // 0x11
    CallIndirect(TypeIdx),

    //
    // Parametric instructions
    //

    // 0x1A
    Drop,
    // 0x1B
    Select,

    //
    // Variable instructions
    //

    // 0x20
    LocalGet(LocalIdx),
    // 0x21
    LocalSet(LocalIdx),
    // 0x22
    LocalTee(LocalIdx),
    // 0x23
    GlobalGet(LocalIdx),
    // 0x24
    GlobalSet(LocalIdx),

    //
    // Memory instructions
    //

    // 0x28
    I32Load(MemArg),
    // 0x29
    I64Load(MemArg),
    // 0x2A
    F32Load(MemArg),
    // 0x2B
    F64Load(MemArg),
    // 0x2C
    I32Load8s(MemArg),
    // 0x2D
    I32Load8u(MemArg),
    // 0x2E
    I32Load16s(MemArg),
    // 0x2F
    I32Load16u(MemArg),
    // 0x30
    I64Load8s(MemArg),
    // 0x31
    I64Load8u(MemArg),
    // 0x32
    I64Load16s(MemArg),
    // 0x33
    I64Load16u(MemArg),
    // 0x34
    I64Load32s(MemArg),
    // 0x35
    I64Load32u(MemArg),
    // 0x36
    I32Store(MemArg),
    // 0x37
    I64Store(MemArg),
    // 0x38
    F32Store(MemArg),
    // 0x39
    F64Store(MemArg),
    // 0x3A
    I32Store8(MemArg),
    // 0x3B
    I32Store16(MemArg),
    // 0x3C
    I64Store8(MemArg),
    // 0x3D
    I64Store16(MemArg),
    // 0x3E
    I64Store32(MemArg),
    // 0x3F
    MemorySize,
    // 0x40
    MemoryGrow,

    //
    // Numeric instructions
    //

    // 0x41
    I32Const(i32),
    // 0x42
    I64Const(i64),
    // 0x43
    F32Const(f32),
    // 0x44
    F64Const(f64),
    // 0x45
    I32Eqz,
    // 0x46
    I32Eq,
    // 0x47
    I32Ne,
    // 0x48
    I32Lt_s,
    // 0x49
    I32Lt_u,
    // 0x4A
    I32Gt_s,
    // 0x4B
    I32Gt_u,
    // 0x4C
    I32Le_s,
    // 0x4D
    I32Le_u,
    // 0x4E
    I32Ge_s,
    // 0x4F
    I32Ge_u,
    // 0x50
    I64Eqz,
    // 0x51
    I64Eq,
    // 0x52
    I64Ne,
    // 0x53
    I64Lt_s,
    // 0x54
    I64Lt_u,
    // 0x55
    I64Gt_s,
    // 0x56
    I64Gt_u,
    // 0x57
    I64Le_s,
    // 0x58
    I64Le_u,
    // 0x59
    I64Ge_s,
    // 0x5A
    I64Ge_u,
    // 0x5B
    F32Eq,
    // 0x5C
    F32Ne,
    // 0x5D
    F32Lt,
    // 0x5E
    F32Gt,
    // 0x5F
    F32Le,
    // 0x60
    F32Ge,
    // 0x61
    F64Eq,
    // 0x62
    F64Ne,
    // 0x63
    F64Lt,
    // 0x64
    F64Gt,
    // 0x65
    F64Le,
    // 0x66
    F64Ge,
    // 0x67
    I32Clz,
    // 0x68
    I32Ctz,
    // 0x69
    I32Popcnt,
    // 0x6A
    I32Add,
    // 0x6B
    I32Sub,
    // 0x6C
    I32Mul,
    // 0x6D
    I32Div_s,
    // 0x6E
    I32Div_u,
    // 0x6F
    I32Rem_s,
    // 0x70
    I32Rem_u,
    // 0x71
    I32And,
    // 0x72
    I32Or,
    // 0x73
    I32Xor,
    // 0x74
    I32Shl,
    // 0x75
    I32Shr_s,
    // 0x76
    I32Shr_u,
    // 0x77
    I32Rotl,
    // 0x78
    I32Rotr,
    // 0x79
    I64Clz,
    // 0x7A
    I64Ctz,
    // 0x7B
    I64Popcnt,
    // 0x7C
    I64Add,
    // 0x7D
    I64Sub,
    // 0x7E
    I64Mul,
    // 0x7F
    I64Div_s,
    // 0x80
    I64Div_u,
    // 0x81
    I64Rem_s,
    // 0x82
    I64Rem_u,
    // 0x83
    I64And,
    // 0x84
    I64Or,
    // 0x85
    I64Xor,
    // 0x86
    I64Shl,
    // 0x87
    I64Shr_s,
    // 0x88
    I64Shr_u,
    // 0x89
    I64Rotl,
    // 0x8A
    I64Rotr,
    // 0x8B
    F32Abs,
    // 0x8C
    F32Neg,
    // 0x8D
    F32Ceil,
    // 0x8E
    F32Floor,
    // 0x8F
    F32Trunc,
    // 0x90
    F32Nearest,
    // 0x91
    F32Sqrt,
    // 0x92
    F32Add,
    // 0x93
    F32Sub,
    // 0x94
    F32Mul,
    // 0x95
    F32Div,
    // 0x96
    F32Min,
    // 0x97
    F32Max,
    // 0x98
    F32Copysign,
    // 0x99
    F64Abs,
    // 0x9A
    F64Neg,
    // 0x9B
    F64Ceil,
    // 0x9C
    F64Floor,
    // 0x9D
    F64Trunc,
    // 0x9E
    F64Nearest,
    // 0x9F
    F64Sqrt,
    // 0xA0
    F64Add,
    // 0xA1
    F64Sub,
    // 0xA2
    F64Mul,
    // 0xA3
    F64Div,
    // 0xA4
    F64Min,
    // 0xA5
    F64Max,
    // 0xA6
    F64Copysign,
    // 0xA7
    I32Wrapi64,
    // 0xA8
    I32Truncf32_s,
    // 0xA9
    I32Truncf32_u,
    // 0xAA
    I32Truncf64_s,
    // 0xAB
    I32Truncf64_u,
    // 0xAC
    I64Extendi32_s,
    // 0xAD
    I64Extendi32_u,
    // 0xAE
    I64Truncf32_s,
    // 0xAF
    I64Truncf32_u,
    // 0xB0
    I64Truncf64_s,
    // 0xB1
    I64Truncf64_u,
    // 0xB2
    F32Converti32_s,
    // 0xB3
    F32Converti32_u,
    // 0xB4
    F32Converti64_s,
    // 0xB5
    F32Converti64_u,
    // 0xB6
    F32Demotef64,
    // 0xB7
    F64Converti32_s,
    // 0xB8
    F64Converti32_u,
    // 0xB9
    F64Converti64_s,
    // 0xBA
    F64Converti64_u,
    // 0xBB
    F64Promotef32,
    // 0xBC
    I32Reinterpretf32,
    // 0xBD
    I64Reinterpretf64,
    // 0xBE
    F32Reinterpreti32,
    // 0xBF
    F64Reinterpreti64,
    // 0xC0
    I32Extend8_s,
    // 0xC1
    I32Extend16_s,
    // 0xC2
    I64Extend8_s,
    // 0xC3
    I64Extend16_s,
    // 0xC4
    I64Extend32_s,
    // 0xFC 0x00
    I32TruncSatf32_s,
    // 0xFC 0x01
    I32TruncSatf32_u,
    // 0xFC 0x02
    I32TruncSatf64_s,
    // 0xFC 0x03
    I32TruncSatf64_u,
    // 0xFC 0x04
    I64TruncSatf32_s,
    // 0xFC 0x05
    I64TruncSatf32_u,
    // 0xFC 0x06
    I64TruncSatf64_s,
    // 0xFC 0x07
    I64TruncSatf64_u,
}

#[derive(Debug)]
pub struct Block {
    pub ty: BlockType,
    pub instrs: Vec<Instruction>,
}

#[derive(Debug)]
pub struct If {
    pub ty: BlockType,
    pub then_instrs: Vec<Instruction>,
    pub else_instrs: Vec<Instruction>,
}

#[derive(Debug)]
pub enum BlockType {
    Empty, // 0x40
    ValType(ValType),
    TypeIdx(TypeIdx),
}

#[derive(Debug)]
pub struct BrTable {
    pub tbl: Vec<LabelIdx>,
    pub def: LabelIdx,
}

#[derive(Debug)]
pub struct MemArg {
    pub align: u32,
    pub offset: u32,
}

#[derive(Debug)]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}

#[derive(Debug)]
pub struct Export {
    pub nm: String,
    pub desc: ExportDesc,
}

#[derive(Debug)]
pub struct Element {
    pub table: TableIdx,
    pub expr: Expr,
    pub init: Vec<FuncIdx>,
}
