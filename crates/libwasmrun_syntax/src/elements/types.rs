use super::{CountedList, Deserialize, Error, VarInt32, VarInt7, VarUint7};
use crate::io;
use core::fmt;

/// Type definition in types section. Currently can be only of the function type.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Type {
    /// Function type.
    Function(FunctionType),
}

impl Deserialize for Type {
    type Error = Error;

    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Self::Error> {
        Ok(Type::Function(FunctionType::deserialize(reader)?))
    }
}

/// Value type.
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
}

impl Deserialize for ValueType {
    type Error = Error;

    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Self::Error> {
        let val = VarInt7::deserialize(reader)?;

        match val.into() {
            -0x01 => Ok(ValueType::I32),
            -0x02 => Ok(ValueType::I64),
            -0x03 => Ok(ValueType::F32),
            -0x04 => Ok(ValueType::F64),
            -0x05 => Ok(ValueType::V128),
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
        }
    }
}

/// Block type which is basically `ValueType` + NoResult (to define blocks that have no return type)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BlockType {
    /// No specified block type
    NoResult,
    /// Inline value type.
    Value(ValueType),
    /// Reference to a signature.
    TypeIndex(u32),
}

impl Deserialize for BlockType {
    type Error = Error;

    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Self::Error> {
        let val = VarInt32::deserialize(reader)?;

        match val.into() {
            -0x40 => Ok(BlockType::NoResult),
            -0x01 => Ok(BlockType::Value(ValueType::I32)),
            -0x02 => Ok(BlockType::Value(ValueType::I64)),
            -0x03 => Ok(BlockType::Value(ValueType::F32)),
            -0x04 => Ok(BlockType::Value(ValueType::F64)),
            -0x05 => Ok(BlockType::Value(ValueType::V128)),
            idx => {
                let idx = idx.try_into().map_err(|_| Error::UnknownBlockType(idx))?;
                Ok(BlockType::TypeIndex(idx))
            }
        }
    }
}

/// Function signature type.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FunctionType {
    form: u8,
    params: Vec<ValueType>,
    results: Vec<ValueType>,
}

impl Default for FunctionType {
    fn default() -> Self {
        FunctionType {
            form: 0x60,
            params: Vec::new(),
            results: Vec::new(),
        }
    }
}

impl FunctionType {
    /// New function type given the params and results as vectors
    pub fn new(params: Vec<ValueType>, results: Vec<ValueType>) -> Self {
        FunctionType {
            form: 0x60,
            params,
            results,
        }
    }
    /// Function form (currently only valid value is `0x60`)
    pub fn form(&self) -> u8 {
        self.form
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
    type Error = Error;

    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Self::Error> {
        let form: u8 = VarUint7::deserialize(reader)?.into();

        if form != 0x60 {
            return Err(Error::UnknownFunctionForm(form));
        }

        let params: Vec<ValueType> = CountedList::deserialize(reader)?.into_inner();
        let results: Vec<ValueType> = CountedList::deserialize(reader)?.into_inner();

        Ok(FunctionType {
            form,
            params,
            results,
        })
    }
}

/// Table element type.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TableElementType {
    /// A reference to a function with any signature.
    AnyFunc,
}

impl Deserialize for TableElementType {
    type Error = Error;

    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Self::Error> {
        let val = VarInt7::deserialize(reader)?;

        match val.into() {
            -0x10 => Ok(TableElementType::AnyFunc),
            _ => Err(Error::UnknownTableElementType(val.into())),
        }
    }
}
