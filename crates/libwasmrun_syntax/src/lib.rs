mod export_entry;
mod func;
mod global_entry;
mod import_entry;
mod index_map;
mod io;
mod module;
mod name_section;
mod ops;
mod primitives;
mod reloc_section;
mod section;
mod segment;
mod table;
mod types;

use self::primitives::{
    CountedList, Uint32, Uint64, VarInt32, VarInt64, VarUint1, VarUint32, VarUint64, VarUint7,
};

pub use self::{
    export_entry::{ExportEntry, Internal},
    func::{Func, FuncBody, Local},
    global_entry::GlobalEntry,
    import_entry::{
        External, GlobalType, ImportEntry, Limits, Limits32, Limits64, MemoryType, TableType,
    },
    index_map::IndexMap,
    module::{ImportCountType, Module},
    name_section::{
        FunctionNameSubsection, LocalNameSubsection, ModuleNameSubsection, NameMap, NameSection,
    },
    ops::{
        AtomicsInstruction, BrTableData, CatchKind, InitExpr, Instruction, Instructions, MemArg,
        SignExtInstruction, SimdInstruction, TryTableData,
    },
    reloc_section::{RelocSection, RelocationEntry},
    section::{
        CodeSection, CustomSection, DataSection, ElementSection, ExportSection, FunctionSection,
        GlobalSection, ImportSection, MemorySection, Section, TableSection, TagType, TypeSection,
    },
    segment::{DataSegment, DataSegmentMode, ElementSegment, ElementSegmentMode},
    table::Table,
    types::{
        BlockType, CompType, FunctionType, HeapType, RecType, ReferenceType, SubType, ValueType,
    },
};

use core::fmt;

pub trait Deserialize: Sized {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error>;
}

/// Deserialization/serialization error.
#[derive(Debug, Clone)]
pub enum Error {
    /// Unexpected end of input.
    UnexpectedEof,

    /// Invalid magic.
    InvalidMagic,

    /// Unsupported version.
    UnsupportedVersion(u32),

    /// Inconsistence between declared and actual length.
    InconsistentLength {
        /// Expected length of the definition.
        expected: usize,

        /// Actual length of the definition.
        actual: usize,
    },

    /// Other error.
    Other(String),

    /// Invalid/unknown value type declaration.
    UnknownValueType(u8),

    /// Invalid block type declaration.
    UnknownBlockType(i32),

    /// Invalid/unknown table element type declaration.
    UnknownTableElementType(i8),

    /// Non-utf8 string.
    NonUtf8String,

    /// Unknown external kind code.
    UnknownExternalKind(u8),

    /// Unknown internal kind code.
    UnknownInternalKind(u8),

    /// Unknown opcode encountered.
    UnknownOpcode(u32),

    /// Unknown SIMD opcode encountered.
    UnknownSimdOpcode(u32),

    /// Invalid VarUint1 value.
    InvalidVarUint1(u8),

    /// Invalid VarInt32 value.
    InvalidVarInt32,

    /// Invalid VarInt64 value.
    InvalidVarInt64,

    /// Invalid VarUint32 value.
    InvalidVarUint32,

    /// Invalid VarUint64 value.
    InvalidVarUint64,

    /// Inconsistent metadata.
    InconsistentMetadata,

    /// Invalid section id.
    InvalidSectionId(u8),

    /// Sections are out of order.
    SectionsOutOfOrder,

    /// Duplicated sections.
    DuplicatedSections(u8),

    /// Invalid value used for flags in limits type.
    InvalidLimitsFlags(u8),

    UnknownCompTypeForm(u8),

    /// Number of function body entries and signatures does not match.
    InconsistentCode,

    /// Only flags 0, 1, and 2 are accepted on segments.
    InvalidSegmentFlags(u32),

    /// Sum of counts of locals is greater than 2^32.
    TooManyLocals,

    /// Duplicated name subsections.
    DuplicatedNameSubsections(u8),

    /// Unknown name subsection type.
    UnknownNameSubsectionType(u8),

    /// Unknown reference type.
    UnknownReferenceType(u8),

    /// Unknown element kind.
    UnknownElementKind(u8),

    InvalidHeapType(i32),

    InvalidTagAttribute(u8),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::UnexpectedEof => write!(f, "Unexpected end of input"),
            Error::InvalidMagic => write!(f, "Invalid magic number at start of file"),
            Error::UnsupportedVersion(v) => write!(f, "Unsupported wasm version {}", v),
            Error::InconsistentLength { expected, actual } => {
                write!(f, "Expected length {}, found {}", expected, actual)
            }
            Error::Other(msg) => write!(f, "{}", msg),
            Error::UnknownValueType(ty) => write!(f, "Invalid or unknown value type {}", ty),
            Error::UnknownBlockType(ty) => write!(f, "Invalid or unknown block type {}", ty),
            Error::UnknownTableElementType(ty) => write!(f, "Unknown table element type {}", ty),
            Error::NonUtf8String => write!(f, "Non-UTF-8 string"),
            Error::UnknownExternalKind(kind) => write!(f, "Unknown external kind {}", kind),
            Error::UnknownInternalKind(kind) => write!(f, "Unknown internal kind {}", kind),
            Error::UnknownOpcode(opcode) => write!(f, "Unknown opcode {}", opcode),
            Error::UnknownSimdOpcode(opcode) => write!(f, "Unknown SIMD opcode {}", opcode),
            Error::InvalidVarUint1(val) => write!(f, "Not an unsigned 1-bit integer: {}", val),
            Error::InvalidVarInt32 => write!(f, "Not a signed 32-bit integer"),
            Error::InvalidVarUint32 => write!(f, "Not an unsigned 32-bit integer"),
            Error::InvalidVarInt64 => write!(f, "Not a signed 64-bit integer"),
            Error::InvalidVarUint64 => write!(f, "Not an unsigned 64-bit integer"),
            Error::InconsistentMetadata => write!(f, "Inconsistent metadata"),
            Error::InvalidSectionId(id) => write!(f, "Invalid section id: {}", id),
            Error::SectionsOutOfOrder => write!(f, "Sections out of order"),
            Error::DuplicatedSections(id) => write!(f, "Duplicated sections ({})", id),
            Error::InvalidLimitsFlags(flags) => write!(f, "Invalid limits flags ({})", flags),
            Error::UnknownCompTypeForm(form) => write!(f, "Unknown composite type form: {}", form),
            Error::InconsistentCode => {
                write!(
                    f,
                    "Number of function body entries and signatures does not match"
                )
            }
            Error::InvalidSegmentFlags(n) => write!(f, "Invalid segment flags: {}", n),
            Error::TooManyLocals => write!(f, "Too many locals"),
            Error::DuplicatedNameSubsections(n) => write!(f, "Duplicated name subsections: {}", n),
            Error::UnknownNameSubsectionType(n) => write!(f, "Unknown subsection type: {}", n),
            Error::UnknownReferenceType(n) => write!(f, "Unknown reference type: {}", n),
            Error::UnknownElementKind(n) => write!(f, "Unknown element kind: {}", n),
            Error::InvalidHeapType(n) => write!(f, "Invalid heap type: {}", n),
            Error::InvalidTagAttribute(n) => write!(f, "Invalid tag attribute: {}", n),
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match self {
            Error::UnexpectedEof => "Unexpected end of input",
            Error::InvalidMagic => "Invalid magic number at start of file",
            Error::UnsupportedVersion(_) => "Unsupported wasm version",
            Error::InconsistentLength { .. } => "Inconsistent length",
            Error::Other(msg) => msg,
            Error::UnknownValueType(_) => "Invalid or unknown value type",
            Error::UnknownBlockType(_) => "Invalid or unknown block type",
            Error::UnknownTableElementType(_) => "Unknown table element type",
            Error::NonUtf8String => "Non-UTF-8 string",
            Error::UnknownExternalKind(_) => "Unknown external kind",
            Error::UnknownInternalKind(_) => "Unknown internal kind",
            Error::UnknownOpcode(_) => "Unknown opcode",
            Error::UnknownSimdOpcode(_) => "Unknown SIMD opcode",
            Error::InvalidVarUint1(_) => "Not an unsigned 1-bit integer",
            Error::InvalidVarInt32 => "Not a signed 32-bit integer",
            Error::InvalidVarUint32 => "Not an unsigned 32-bit integer",
            Error::InvalidVarInt64 => "Not a signed 64-bit integer",
            Error::InvalidVarUint64 => "Not an unsigned 64-bit integer",
            Error::InconsistentMetadata => "Inconsistent metadata",
            Error::InvalidSectionId(_) => "Invalid section id",
            Error::SectionsOutOfOrder => "Sections out of order",
            Error::DuplicatedSections(_) => "Duplicated section",
            Error::InvalidLimitsFlags(_) => "Invalid limits flags",
            Error::UnknownCompTypeForm(_) => "Unknown composite type form",
            Error::InconsistentCode => {
                "Number of function body entries and signatures does not match"
            }
            Error::InvalidSegmentFlags(_) => "Invalid segment flags",
            Error::TooManyLocals => "Too many locals",
            Error::DuplicatedNameSubsections(_) => "Duplicated name subsections",
            Error::UnknownNameSubsectionType(_) => "Unknown name subsections type",
            Error::UnknownReferenceType(_) => "Unknown reference type",
            Error::UnknownElementKind(_) => "Unknown element kind",
            Error::InvalidHeapType(_) => "Invalid heap type",
            Error::InvalidTagAttribute(_) => "Invalid tag attribute",
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Other(format!("I/O Error: {:?}", err))
    }
}

/// Deserialize deserializable type from buffer.
pub fn deserialize_buffer<T: Deserialize>(mut contents: &[u8]) -> Result<T, Error> {
    let result = T::deserialize(&mut contents)?;
    if !contents.is_empty() {
        return Err(Error::Other("Trailing data after module".to_string()));
    }
    Ok(result)
}

/// Deserialize module from file.
pub fn deserialize_file<P: AsRef<std::path::Path>>(p: P) -> Result<Module, Error> {
    let mut f = std::fs::File::open(p)
        .map_err(|e| Error::Other(format!("Can't read from the file: {:?}", e)))?;

    Module::deserialize(&mut f)
}
