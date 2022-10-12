use crate::{io, CountedList, Deserialize, Error, Uint8, VarInt32, VarUint7};

use std::fmt;

/// Type definition in types section. Currently can be only of the function type.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Type {
    Function(FunctionType),
}

impl Deserialize for Type {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(Type::Function(FunctionType::deserialize(reader)?))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Hash, Eq)]
pub enum ValueType {
    /// 32-bit signed integer
    I32,

    /// 64-bit signed integer
    I64,

    /// 32-bit float
    F32,

    /// 64-bit float
    F64,

    /// 128-bit SIMD register
    V128,

    /// Reference to an embedder object
    ExternRef,

    /// Reference to a function
    FuncRef,
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
            0x70 => Ok(ValueType::FuncRef),
            0x6f => Ok(ValueType::ExternRef),
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
            ValueType::ExternRef => write!(f, "externref"),
            ValueType::FuncRef => write!(f, "funcref"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BlockType {
    /// Shorthand for `[] -> []`
    Empty,

    /// Shorthand for `[] -> [ty]`
    Value(ValueType),

    /// Type of the block is the given function type with given index
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
            -0x01 => Ok(BlockType::Value(ValueType::I32)),
            -0x02 => Ok(BlockType::Value(ValueType::I64)),
            -0x03 => Ok(BlockType::Value(ValueType::F32)),
            -0x04 => Ok(BlockType::Value(ValueType::F64)),
            -0x05 => Ok(BlockType::Value(ValueType::V128)),
            -0x10 => Ok(BlockType::Value(ValueType::FuncRef)),
            -0x11 => Ok(BlockType::Value(ValueType::ExternRef)),
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
    /// New function type given the params and results as vectors
    pub fn new(params: Vec<ValueType>, results: Vec<ValueType>) -> Self {
        FunctionType { params, results }
    }

    /// Parameters in the function signature.
    pub fn params(&self) -> &[ValueType] {
        &self.params
    }

    /// Mutable parameters in the function signature.
    pub fn params_mut(&mut self) -> &mut Vec<ValueType> {
        &mut self.params
    }

    /// Results in the function signature, if any.
    pub fn results(&self) -> &[ValueType] {
        &self.results
    }

    /// Mutable type in the function signature, if any.
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

/// <https://webassembly.github.io/spec/core/syntax/types.html#syntax-reftype>
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReferenceType {
    FuncRef,
    ExternRef,
    RefNull(HeapType),
    RefNonNull(HeapType),
}

impl Deserialize for ReferenceType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let val = Uint8::deserialize(reader)?;

        match val.into() {
            0x70 => Ok(ReferenceType::FuncRef),
            0x6F => Ok(ReferenceType::ExternRef),
            0x6B => Ok(ReferenceType::RefNonNull(HeapType::deserialize(reader)?)),
            0x6C => Ok(ReferenceType::RefNull(HeapType::deserialize(reader)?)),
            other => Err(Error::UnknownReferenceType(other)),
        }
    }
}

impl fmt::Display for ReferenceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReferenceType::FuncRef => write!(f, "funcref"),
            ReferenceType::ExternRef => write!(f, "externref"),
            ReferenceType::RefNull(heap_ty) => write!(f, "ref null {}", heap_ty),
            ReferenceType::RefNonNull(heap_ty) => write!(f, "ref {}", heap_ty),
        }
    }
}

/// <https://webassembly.github.io/function-references/core/binary/types.html#heap-types>
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HeapType {
    Extern,
    Func,
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
            HeapType::Extern => write!(f, "extern"),
            HeapType::Func => write!(f, "func"),
            HeapType::TypeIdx(idx) => <u32 as fmt::Display>::fmt(idx, f),
        }
    }
}
