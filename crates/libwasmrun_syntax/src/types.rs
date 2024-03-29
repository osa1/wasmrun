#![allow(unused)] // temporary while implementing GC stuff

use crate::{io, CountedList, Deserialize, Error, VarInt32, VarUint32, VarUint7};

use std::fmt;

/// An entry in a types section, defining a set of recursive types. (GC version)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecType {
    pub tys: Vec<SubType>,
}

impl Deserialize for RecType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let form: u8 = VarUint7::deserialize(reader)?.into();
        match form {
            0x4E => {
                let tys: Vec<SubType> = CountedList::deserialize(reader)?.into_inner();
                Ok(RecType { tys })
            }
            other => {
                let ty: SubType = SubType::deserialize_val(reader, other)?;
                Ok(RecType { tys: vec![ty] })
            }
        }
    }
}

/// A single type in a recursion group, possibly a subtype of others.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SubType {
    /// Whether the type is final. Final types can't have subtypes.
    pub final_: bool,

    /// Type indices of the type's super types.
    ///
    /// Note: In the MVP GC spec at most one super type is allowed.
    pub supers: Vec<u32>,

    /// The type.
    pub comp_ty: CompType,
}

impl Deserialize for SubType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let form: u8 = VarUint7::deserialize(reader)?.into();
        SubType::deserialize_val(reader, form)
    }
}

impl SubType {
    fn deserialize_val<R: io::Read>(reader: &mut R, val: u8) -> Result<Self, Error> {
        match val {
            0x50 => {
                let supers: Vec<u32> = CountedList::<VarUint32>::deserialize(reader)?
                    .into_inner()
                    .into_iter()
                    .map(|i| i.into())
                    .collect();

                let comp_ty = CompType::deserialize(reader)?;

                Ok(SubType {
                    final_: false,
                    supers,
                    comp_ty,
                })
            }

            0x4F => {
                let supers: Vec<u32> = CountedList::<VarUint32>::deserialize(reader)?
                    .into_inner()
                    .into_iter()
                    .map(|i| i.into())
                    .collect();

                let comp_ty = CompType::deserialize(reader)?;

                Ok(SubType {
                    final_: true,
                    supers,
                    comp_ty,
                })
            }

            other => {
                let comp_ty = CompType::deserialize_val(reader, other)?;
                Ok(SubType {
                    final_: true,
                    supers: vec![],
                    comp_ty,
                })
            }
        }
    }

    pub fn as_function_type(&self) -> Option<&FunctionType> {
        self.comp_ty.as_function_type()
    }

    pub fn as_struct_type(&self) -> Option<&StructType> {
        self.comp_ty.as_struct_type()
    }

    pub fn as_array_type(&self) -> Option<&ArrayType> {
        self.comp_ty.as_array_type()
    }
}

/// A composite type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompType {
    Func(FunctionType),
    Struct(StructType),
    Array(ArrayType),
}

impl Deserialize for CompType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let form: u8 = VarUint7::deserialize(reader)?.into();
        CompType::deserialize_val(reader, form)
    }
}

impl CompType {
    fn deserialize_val<R: io::Read>(reader: &mut R, val: u8) -> Result<Self, Error> {
        match val {
            0x5e => Ok(CompType::Array(ArrayType::deserialize(reader)?)),

            0x5f => Ok(CompType::Struct(StructType::deserialize(reader)?)),

            0x60 => Ok(CompType::Func(FunctionType::deserialize(reader)?)),

            other => todo!(),
        }
    }

    pub fn as_function_type(&self) -> Option<&FunctionType> {
        match self {
            CompType::Func(func_ty) => Some(func_ty),
            CompType::Struct(_) | CompType::Array(_) => None,
        }
    }

    pub fn as_struct_type(&self) -> Option<&StructType> {
        match self {
            CompType::Struct(struct_ty) => Some(struct_ty),
            CompType::Func(_) | CompType::Array(_) => None,
        }
    }

    pub fn as_array_type(&self) -> Option<&ArrayType> {
        match self {
            CompType::Array(array_ty) => Some(array_ty),
            CompType::Func(_) | CompType::Struct(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub fields: Vec<FieldType>,
}

impl Deserialize for StructType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let fields: Vec<FieldType> = CountedList::<FieldType>::deserialize(reader)?.into_inner();
        Ok(StructType { fields })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType {
    pub field: FieldType,
}

impl Deserialize for ArrayType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let field = FieldType::deserialize(reader)?;
        Ok(ArrayType { field })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldType {
    pub mutability: Mutability,
    pub storage_ty: StorageType,
}

impl Deserialize for FieldType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let storage_ty = StorageType::deserialize(reader)?;
        let mutability = Mutability::deserialize(reader)?;
        Ok(FieldType {
            mutability,
            storage_ty,
        })
    }
}

// TODO: Use this type in `GlobalType`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Mutable,
    Immutable,
}

impl Deserialize for Mutability {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        match u8::deserialize(reader)? {
            0x00 => Ok(Mutability::Immutable),
            0x01 => Ok(Mutability::Mutable),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StorageType {
    Val(ValueType),
    Packed(PackedType),
}

impl Deserialize for StorageType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let val: u8 = VarUint7::deserialize(reader)?.into();
        match val {
            0x78 => Ok(StorageType::Packed(PackedType::I8)),

            0x77 => Ok(StorageType::Packed(PackedType::I16)),

            other => {
                // TODO: Convert error vlaue
                Ok(StorageType::Val(ValueType::deserialize_val(reader, other)?))
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
        let val = VarUint7::deserialize(reader)?.into();
        ValueType::deserialize_val(reader, val)
    }
}

impl ValueType {
    fn deserialize_val<R: io::Read>(reader: &mut R, val: u8) -> Result<Self, Error> {
        match val.into() {
            // -0x01
            0x7f => Ok(ValueType::I32),

            // -0x02
            0x7e => Ok(ValueType::I64),

            // -0x03
            0x7d => Ok(ValueType::F32),

            // -0x04
            0x7c => Ok(ValueType::F64),

            // -0x05
            0x7b => Ok(ValueType::V128),

            other => match ReferenceType::deserialize_val(reader, val) {
                Ok(ref_ty) => Ok(ValueType::Reference(ref_ty)),
                Err(_) => Err(Error::UnknownValueType(val)),
            },
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
        Ok(match val.into() {
            -0x40 => BlockType::Empty,
            // TODO: Duplicate valtype parsing
            // 0x7f
            -0x01 => BlockType::Value(ValueType::I32),

            // 0x7e
            -0x02 => BlockType::Value(ValueType::I64),

            // 0x7d
            -0x03 => BlockType::Value(ValueType::F32),

            // 0x7c
            -0x04 => BlockType::Value(ValueType::F64),

            // 0x7b
            -0x05 => BlockType::Value(ValueType::V128),

            // TODO: duplicate reftype parsing
            // -0x70
            -0x10 => BlockType::Value(ValueType::Reference(ReferenceType::funcref())),

            // -0x6f
            -0x11 => BlockType::Value(ValueType::Reference(ReferenceType::externref())),

            // 0x6e
            -0x12 => BlockType::Value(ValueType::Reference(ReferenceType::anyref())),

            // 0x6d
            -0x13 => BlockType::Value(ValueType::Reference(ReferenceType::eqref())),

            // 0x6c
            -0x14 => BlockType::Value(ValueType::Reference(ReferenceType::i31ref())),

            // 0x6b
            -0x15 => BlockType::Value(ValueType::Reference(ReferenceType::structref())),

            // 0x6a
            -0x16 => BlockType::Value(ValueType::Reference(ReferenceType::arrayref())),

            // -0x69
            -0x17 => BlockType::Value(ValueType::Reference(ReferenceType::exnref())),

            // -0x63
            -0x1D => {
                let heap_ty = HeapType::deserialize(reader)?;
                BlockType::Value(ValueType::Reference(ReferenceType {
                    nullable: true,
                    heap_ty,
                }))
            }

            // -0x64
            -0x1C => {
                let heap_ty = HeapType::deserialize(reader)?;
                BlockType::Value(ValueType::Reference(ReferenceType {
                    nullable: false,
                    heap_ty,
                }))
            }

            idx => {
                let idx = u32::try_from(idx).map_err(|_| Error::UnknownBlockType(idx))?;
                BlockType::TypeIndex(idx)
            }
        })
    }
}

#[derive(Debug, Default, Clone, PartialEq, Hash, Eq)]
pub struct FunctionType {
    pub params: Vec<ValueType>,
    pub results: Vec<ValueType>,
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

    /// `ref null exn`: nullable top type of all exception types.
    pub fn exnref() -> Self {
        ReferenceType {
            nullable: true,
            heap_ty: HeapType::Exn,
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
            // -0x0D
            0x73 => Ok(ReferenceType::nullfuncref()),

            // -0x0E
            0x72 => Ok(ReferenceType::nullexternref()),

            // -0x0F
            0x71 => Ok(ReferenceType::nullref()),

            // -0x10
            0x70 => Ok(ReferenceType::funcref()),

            // -0x11
            0x6F => Ok(ReferenceType::externref()),

            // -0x12
            0x6E => Ok(ReferenceType::anyref()),

            // -0x13
            0x6D => Ok(ReferenceType::eqref()),

            // -0x14
            0x6C => Ok(ReferenceType::i31ref()),

            // -0x15
            0x6B => Ok(ReferenceType::structref()),

            // -0x16
            0x6A => Ok(ReferenceType::arrayref()),

            // -0x17
            0x69 => Ok(ReferenceType::exnref()),

            // -0x1C
            0x64 => {
                let heap_ty = HeapType::deserialize(reader)?;
                Ok(ReferenceType {
                    nullable: false,
                    heap_ty,
                })
            }

            // -0x1D
            0x63 => {
                let heap_ty = HeapType::deserialize(reader)?;
                Ok(ReferenceType {
                    nullable: true,
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

    /// The top type for all exception types.
    Exn,

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
            // 0x73
            -0x0d => HeapType::NoFunc,

            // 0x72
            -0x0e => HeapType::NoExtern,

            // 0x71
            -0x0f => HeapType::None,

            // 0x70
            -0x10 => HeapType::Func,

            // 0x6f
            -0x11 => HeapType::Extern,

            // 0x6e
            -0x12 => HeapType::Any,

            // 0x6d
            -0x13 => HeapType::Eq,

            // 0x6c
            -0x14 => HeapType::I31,

            // 0x6b
            -0x15 => HeapType::Struct,

            // 0x6a
            -0x16 => HeapType::Array,

            // 0x69
            -0x17 => HeapType::Exn,

            other => {
                let idx = match u32::try_from(other) {
                    Ok(idx) => idx,
                    Err(_) => return Err(Error::InvalidHeapType(other)),
                };
                HeapType::TypeIdx(idx)
            }
        })
    }
}

impl fmt::Display for HeapType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HeapType::Any => write!(f, "any"),
            HeapType::None => write!(f, "none"),
            HeapType::Exn => write!(f, "exn"),
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
