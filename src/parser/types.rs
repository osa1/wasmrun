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
