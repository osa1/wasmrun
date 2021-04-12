use crate::ast::ValType;

type Var = String;

#[derive(Debug)]
pub enum Instr {
    Unreachable,
    Nop,
    Drop,
    Select(Option<Vec<ValType>>),
    Block(BlockType, Vec<Instr>),
    Loop(BlockType, Vec<Instr>),
    If(BlockType, Vec<Instr>, Vec<Instr>),
    Br(Var),
    BrIf(Var),
    BrTable(Vec<Var>, Var),
    Return,
    Call(Var),
    CallIndirect(Var, Var),
    LocalGet(Var),
    LocalSet(Var),
    LocalTee(Var),
    GlobalGet(Var),
    GlobalSet(Var),
    TableGet(Var),
    TableSet(Var),
    TableSize(Var),
    TableGrow(Var),
    TableFill(Var),
    TableCopy(Var, Var),
    TableInit(Var, Var),
    ElemDrop(Var),
    Load(MemOp),
    Store(MemOp),
    MemorySize,
    MemoryGrow,
    MemoryFill,
    MemoryCopy,
    MemoryInit(Var),
    DataDrop(Var),
    RefNull(Var),
    RefIsNull,
    Const(Const),
    Test(TestOp),
    Compare(RelOp),
    Unary(UnOp),
    Binary(BinOp),
    Convert(CvtOp),
}

#[derive(Debug)]
pub struct MemOp {
    pub ty: NumType,
    pub align: u32,
    pub offset: u32,
}

#[derive(Debug)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug)]
pub enum Const {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug)]
pub enum BlockType {
    Var(Var),
    Val(Option<ValType>),
}

#[derive(Debug)]
pub enum IntUnOp {
    Clz,
    Ctz,
    Popcnt,
}

#[derive(Debug)]
pub enum IntBinOp {
    Add,
    Sub,
    Mul,
    DivS,
    DivU,
    RemS,
    RemU,
    And,
    Or,
    Xor,
    Shl,
    ShrS,
    ShrU,
    Rotl,
    Rotr,
}

#[derive(Debug)]
pub enum FloatUnOp {
    Neg,
    Abs,
    Ceil,
    Floor,
    Trunc,
    Nearest,
    Sqrt,
}

#[derive(Debug)]
pub enum FloatBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Min,
    Max,
    CopySign,
}

#[derive(Debug)]
pub enum NumSize {
    B32,
    B64,
}

#[derive(Debug)]
pub enum BinOp {
    Int(NumSize, IntBinOp),
    Float(NumSize, FloatBinOp),
}

#[derive(Debug)]
pub enum TestOp {
    /// Only for integers
    Eqz(NumSize),
}

#[derive(Debug)]
pub enum IntRelOp {
    Eq,
    Ne,
    LtS,
    LtU,
    GtS,
    GtU,
    LeS,
    LeU,
    GeS,
    GeU,
}

#[derive(Debug)]
pub enum FloatRelOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug)]
pub enum RelOp {
    Int(NumSize, IntRelOp),
    Float(NumSize, FloatRelOp),
}

#[derive(Debug)]
pub enum UnOp {
    Int(NumSize, IntUnOp),
    Float(NumSize, FloatUnOp),
}

#[derive(Debug)]
pub enum IntCvtOp {
    ExtendSI32,
    ExtendUI32,
    WrapI64,
    TruncSF32,
    TruncUF32,
    TruncSF64,
    TruncUF64,
    TruncSatSF32,
    TruncSatUF32,
    TruncSatSF64,
    TruncSatUF64,
    ReinterpretFloat,
}

#[derive(Debug)]
pub enum FloatCvtOp {
    ConvertSI32,
    ConvertUI32,
    ConvertSI64,
    ConvertUI64,
    PromoteF32,
    DemoteF64,
    ReinterpretInt,
}

#[derive(Debug)]
pub enum CvtOp {
    Int(NumSize, IntCvtOp),
    Float(NumSize, FloatCvtOp),
}
