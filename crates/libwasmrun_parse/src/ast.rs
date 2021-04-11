/// A test file is a list of modules and assertions
pub type TestFile = Vec<ModuleOrAssert>;

#[derive(Debug)]
pub enum ModuleOrAssert {
    Module(Module),
    Assert(Assert),
}

/// A module is just a list of sections
pub type Module = Vec<Section>;

#[derive(Debug)]
pub enum Section {
    Type(TypeSection),
    Import(ImportSection),
    Func(FuncSection),
    Table(TableSection),
    Mem(MemSection),
    Global(GlobalSection),
    Export(ExportSection),
    Start(StartSection),
    Elem(ElemSection),
    Data(DataSection),
}

#[derive(Debug)]
pub struct TypeSection {
    pub id: Option<String>,
    pub ty: FuncType,
}

#[derive(Debug)]
pub struct FuncType {
    pub params: Vec<Param>,
    pub ret: Vec<ValType>,
}

#[derive(Debug)]
pub struct Param {
    pub id: Option<String>,
    pub ty: ValType,
}

#[derive(Debug, Clone, Copy)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
    FuncRef,
    ExternRef,
}

#[derive(Debug)]
pub struct ImportSection {
    pub mod_: String,
    pub name: String,
    pub desc: ImportDesc,
}

#[derive(Debug)]
pub enum ImportDesc {
    Func(FuncImportDesc),
    Table(TableImportDesc),
    Mem(MemImportDesc),
    Global(GlobalImportDesc),
}

#[derive(Debug)]
pub struct FuncImportDesc {
    pub id: Option<String>,
    pub type_use: TypeUse,
}

#[derive(Debug)]
pub struct TypeUse {
    pub type_idx: Option<u64>,
    pub params: Option<Vec<Param>>,
    pub results: Option<Vec<ValType>>,
}

#[derive(Debug)]
pub struct TableImportDesc {
    pub id: Option<String>,
    pub limits: Limits,
    pub ref_type: RefType,
}

#[derive(Debug)]
pub struct Limits {
    pub min: u64,
    pub max: Option<u64>,
}

#[derive(Debug)]
pub enum RefType {
    FuncRef,
    ExternRef,
}

#[derive(Debug)]
pub struct MemImportDesc {
    pub id: Option<String>,
    pub ty: Limits,
}

#[derive(Debug)]
pub struct GlobalImportDesc {
    pub id: Option<String>,
    pub mut_: bool,
    pub ty: ValType,
}

#[derive(Debug)]
pub struct FuncSection;

#[derive(Debug)]
pub enum TableSection {
    Table {
        id: Option<String>,
        limits: Limits,
        ty: RefType,
    },
    Elems {
        ty: RefType,
        elems: Vec<String>,
    }
}

#[derive(Debug)]
pub struct MemSection;

#[derive(Debug)]
pub struct GlobalSection;

#[derive(Debug)]
pub struct ExportSection;

#[derive(Debug)]
pub struct StartSection;

#[derive(Debug)]
pub struct ElemSection;

#[derive(Debug)]
pub struct DataSection;

#[derive(Debug)]
pub enum Assert {
    Return {
        fun: String,
        args: Vec<Value>,
        ret: Value,
    },
}

#[derive(Debug)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}
