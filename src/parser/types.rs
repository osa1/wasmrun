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
