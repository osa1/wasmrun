#![allow(unused)] // temporary while implementing GC stuff

use crate::{io, CountedList, Deserialize, Error, VarInt32, VarUint7};

use std::fmt;

/// A type definition in a types section. (pre-GC version)
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Type {
    Function(FunctionType),
}

impl Deserialize for Type {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(Type::Function(FunctionType::deserialize(reader)?))
    }
}

/// An entry in a types section, defining a set of recursive types. (GC version)
#[derive(Debug, Clone)]
pub struct DefType {
    rec: Vec<SubType>,
}

/// A single type in a recursion group, possibly a subtype of others.
#[derive(Debug, Clone)]
pub struct SubType {
    /// Whether the type is final. Final types can't have subtypes.
    final_: bool,

    /// Type indices of the type's super types.
    ///
    /// Note: In the MVP GC spec at most one super type is allowed.
    supers: Vec<u32>,

    /// The type.
    ty: CompType,
}

/// A composite type.
#[derive(Debug, Clone)]
pub enum CompType {
    Func(FunctionType),
    Struct(StructType),
    Array(ArrayType),
}

#[derive(Debug, Clone)]
pub struct StructType {
    fields: Vec<FieldType>,
}

#[derive(Debug, Clone)]
pub struct ArrayType {
    field: FieldType,
}

#[derive(Debug, Clone)]
pub struct FieldType {
    mutability: Mutability,
    storage_ty: StorageType,
}

#[derive(Debug, Clone, Copy)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Debug, Clone)]
pub enum StorageType {
    Val(ValueType),
    Packed(PackedType),
}

#[derive(Debug, Clone, Copy)]
pub enum PackedType {
    I8,
    I16,
}

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub enum ValueType {
    /// 32-bit signed integer.
    I32,

    /// 64-bit signed integer.
    I64,

    /// 32-bit float.
    F32,

    /// 64-bit float.
    F64,

    /// 128-bit SIMD number.
    V128,

    /// A reference type.
    Reference(ReferenceType),
}

impl Deserialize for ValueType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let val = VarUint7::deserialize(reader)?;
        match val.into() {
            0x7f => Ok(ValueType::I32),
            0x7e => Ok(ValueType::I64),
            0x7d => Ok(ValueType::F32),
            0x7c => Ok(ValueType::F64),
            0x7b => Ok(ValueType::V128),

            // TODO: duplicate reftype parsing
            // -0x10
            0x70 => Ok(ValueType::Reference(ReferenceType::funcref())),

            // -0x11
            0x6F => Ok(ValueType::Reference(ReferenceType::externref())),

            // -0x1D
            0x63 => {
                let heap_ty = HeapType::deserialize(reader)?;
                Ok(ValueType::Reference(ReferenceType {
                    nullable: true,
                    heap_ty,
                }))
            }

            // -0x1C
            0x64 => {
                let heap_ty = HeapType::deserialize(reader)?;
                Ok(ValueType::Reference(ReferenceType {
                    nullable: false,
                    heap_ty,
                }))
            }

            _ => Err(Error::UnknownValueType(val.into())),
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ValueType::I32 => write!(f, "i32"),
            ValueType::I64 => write!(f, "i64"),
            ValueType::F32 => write!(f, "f32"),
            ValueType::F64 => write!(f, "f64"),
            ValueType::V128 => write!(f, "v128"),
            ValueType::Reference(ref_ty) => ref_ty.fmt(f),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BlockType {
    /// Shorthand for `[] -> []`.
    Empty,

    /// Shorthand for `[] -> [ty]`.
    Value(ValueType),

    /// Type of the block is the given function type with given index.
    TypeIndex(u32),
}

impl Deserialize for BlockType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        // https://webassembly.github.io/spec/core/binary/instructions.html#binary-blocktype
        //
        // > Unlike any other occurrence, the type index in a block type is encoded as a positive
        // > signed integer, so that its signed LEB128 bit pattern cannot collide with the encoding
        // > of value types or the special code 0x40, which correspond to the LEB128 encoding of
        // > negative integers. To avoid any loss in the range of allowed indices, it is treated as
        // > a 33 bit signed integer.

        // TODO: Shouldn't this be `VarInt33`?
        let val = VarInt32::deserialize(reader)?;

        // NB. The byte 0x40 is 64 in unsigned LEB128, -64 in signed LEB128. Byte values of value
        // types are negative numbers when interpreted as signed LEB128. For example, the byte 0x7F
        // for value type `i32` is -1 when interpreted as signed LEB128.
        match val.into() {
            -0x40 => Ok(BlockType::Empty),
            // TODO: Duplicate valtype parsing
            -0x01 => Ok(BlockType::Value(ValueType::I32)),
            -0x02 => Ok(BlockType::Value(ValueType::I64)),
            -0x03 => Ok(BlockType::Value(ValueType::F32)),
            -0x04 => Ok(BlockType::Value(ValueType::F64)),
            -0x05 => Ok(BlockType::Value(ValueType::V128)),

            // TODO: duplicate reftype parsing
            // -0x10
            -0x10 => Ok(BlockType::Value(ValueType::Reference(
                ReferenceType::funcref(),
            ))),

            // -0x11
            -0x11 => Ok(BlockType::Value(ValueType::Reference(
                ReferenceType::externref(),
            ))),

            // -0x1D
            -0x1D => {
                let heap_ty = HeapType::deserialize(reader)?;
                Ok(BlockType::Value(ValueType::Reference(ReferenceType {
                    nullable: true,
                    heap_ty,
                })))
            }

            // -0x1C
            -0x1C => {
                let heap_ty = HeapType::deserialize(reader)?;
                Ok(BlockType::Value(ValueType::Reference(ReferenceType {
                    nullable: false,
                    heap_ty,
                })))
            }

            idx => {
                let idx = u32::try_from(idx).map_err(|_| Error::UnknownBlockType(idx))?;
                Ok(BlockType::TypeIndex(idx))
            }
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Hash, Eq)]
pub struct FunctionType {
    params: Vec<ValueType>,
    results: Vec<ValueType>,
}

impl FunctionType {
    pub fn new(params: Vec<ValueType>, results: Vec<ValueType>) -> Self {
        FunctionType { params, results }
    }

    pub fn params(&self) -> &[ValueType] {
        &self.params
    }

    pub fn params_mut(&mut self) -> &mut Vec<ValueType> {
        &mut self.params
    }

    pub fn results(&self) -> &[ValueType] {
        &self.results
    }

    pub fn results_mut(&mut self) -> &mut Vec<ValueType> {
        &mut self.results
    }
}

impl Deserialize for FunctionType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let form: u8 = VarUint7::deserialize(reader)?.into();

        if form != 0x60 {
            return Err(Error::UnknownFunctionForm(form));
        }

        let params: Vec<ValueType> = CountedList::deserialize(reader)?.into_inner();
        let results: Vec<ValueType> = CountedList::deserialize(reader)?.into_inner();

        Ok(FunctionType { params, results })
    }
}

/// <https://webassembly.github.io/spec/core/syntax/types.html#syntax-reftype>.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ReferenceType {
    pub nullable: bool,
    pub heap_ty: HeapType,
}

impl ReferenceType {
    /// `ref null any`: nullable top type of all internal types.
    pub fn anyref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::Any,
        }
    }

    /// `ref null none`: nullable bottom type of all internal types.
    pub fn nullref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::None,
        }
    }

    /// `ref null extern`: nullable top type of all external types.
    pub fn externref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::Extern,
        }
    }

    /// `ref null noextern`: nullable bottom type of all external types.
    pub fn nullexternref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::NoExtern,
        }
    }

    /// `ref null func`: nullable top type of all function types.
    pub fn funcref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::Func,
        }
    }

    /// `ref null nofunc`: nullable bottom type of all function types.
    pub fn nullfuncref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::NoFunc,
        }
    }

    /// `ref null eq`: nullable `eq` type.
    pub fn eqref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::Eq,
        }
    }

    /// `ref null struct`: nullable top type of all struct types.
    pub fn structref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::Struct,
        }
    }

    /// `ref null array`: nullable top type of all array types.
    pub fn arrayref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::Array,
        }
    }

    /// `ref null i31`.
    pub fn i31ref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::I31,
        }
    }
}

impl Deserialize for ReferenceType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let val = u8::deserialize(reader)?;
        ReferenceType::deserialize_val(reader, val)
    }
}

impl ReferenceType {
    pub(crate) fn deserialize_val<R: io::Read>(reader: &mut R, val: u8) -> Result<Self, Error> {
        match val {
            // -0x10
            0x70 => Ok(ReferenceType::funcref()),

            // -0x11
            0x6F => Ok(ReferenceType::externref()),

            // -0x1D
            0x63 => {
                let heap_ty = HeapType::deserialize(reader)?;
                Ok(ReferenceType {
                    nullable: true,
                    heap_ty,
                })
            }

            // -0x1C
            0x64 => {
                let heap_ty = HeapType::deserialize(reader)?;
                Ok(ReferenceType {
                    nullable: false,
                    heap_ty,
                })
            }

            other => Err(Error::UnknownReferenceType(other)),
        }
    }
}

impl fmt::Display for ReferenceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.nullable {
            write!(f, "ref null {}", self.heap_ty)
        } else {
            write!(f, "ref {}", self.heap_ty)
        }
    }
}

/// <https://webassembly.github.io/function-references/core/binary/types.html#heap-types>, with GC
/// stuff from <https://github.com/WebAssembly/gc/blob/main/proposals/gc/MVP.md#types>.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HeapType {
    /// The top type for all internal types.
    Any,

    /// The bottom type for all internal types.
    None,

    /// The top type for all extern types.
    Extern,

    /// The bottom type for all external types.
    NoExtern,

    /// The top type for all function types.
    Func,

    /// The bottom type for all function types.
    NoFunc,

    /// The common supertype for all types comparable with `ref.eq`.
    Eq,

    /// The top type for all struct types.
    Struct,

    /// The top type for all array types.
    Array,

    /// A 31-bit unboxed scalar.
    I31,

    TypeIdx(u32),
}

impl Deserialize for HeapType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        // TODO: This should be signed 33-bit to allow entire type index range
        let val = VarInt32::deserialize(reader)?.into();

        Ok(match val {
            -0x11 => HeapType::Extern,
            -0x10 => HeapType::Func,
            other => HeapType::TypeIdx(other as u32),
        })
    }
}

impl fmt::Display for HeapType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HeapType::Any => write!(f, "any"),
            HeapType::None => write!(f, "none"),
            HeapType::Extern => write!(f, "extern"),
            HeapType::NoExtern => write!(f, "noextern"),
            HeapType::Func => write!(f, "func"),
            HeapType::NoFunc => write!(f, "nofunc"),
            HeapType::Eq => write!(f, "eq"),
            HeapType::Struct => write!(f, "struct"),
            HeapType::Array => write!(f, "struct"),
            HeapType::I31 => write!(f, "i31"),
            HeapType::TypeIdx(idx) => <u32 as fmt::Display>::fmt(idx, f),
        }
    }
}
