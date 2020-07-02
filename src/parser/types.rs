#![allow(non_camel_case_types)]

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

pub type FuncIdx = u32;
pub type TypeIdx = u32;
pub type LabelIdx = u32;
pub type LocalIdx = u32;

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
