use super::{CountedList, Deserialize, Error, InitExpr, VarUint32};
use crate::io;

const FLAG_MEMZERO: u32 = 0;
const FLAG_PASSIVE: u32 = 1;
const FLAG_MEM_NONZERO: u32 = 2;

const VALUES_BUFFER_LENGTH: usize = 16384;

/// Entry in the element section.
#[derive(Debug, Clone, PartialEq)]
pub struct ElementSegment {
    index: u32,
    offset: Option<InitExpr>,
    members: Vec<u32>,
    passive: bool,
}

impl ElementSegment {
    /// New element segment.
    pub fn new(index: u32, offset: Option<InitExpr>, members: Vec<u32>) -> Self {
        ElementSegment {
            index,
            offset,
            members,
            passive: false,
        }
    }

    /// Sequence of function indices.
    pub fn members(&self) -> &[u32] {
        &self.members
    }

    /// Sequence of function indices (mutable)
    pub fn members_mut(&mut self) -> &mut Vec<u32> {
        &mut self.members
    }

    /// Table index (currently valid only value of `0`)
    pub fn index(&self) -> u32 {
        self.index
    }

    /// An i32 initializer expression that computes the offset at which to place the elements.
    ///
    /// Note that this return `None` if the segment is `passive`.
    pub fn offset(&self) -> &Option<InitExpr> {
        &self.offset
    }

    /// An i32 initializer expression that computes the offset at which to place the elements (mutable)
    ///
    /// Note that this return `None` if the segment is `passive`.
    pub fn offset_mut(&mut self) -> &mut Option<InitExpr> {
        &mut self.offset
    }
}

impl ElementSegment {
    /// Whether or not this table segment is "passive"
    pub fn passive(&self) -> bool {
        self.passive
    }

    /// Whether or not this table segment is "passive"
    pub fn passive_mut(&mut self) -> &mut bool {
        &mut self.passive
    }

    /// Set whether or not this table segment is "passive"
    pub fn set_passive(&mut self, passive: bool) {
        self.passive = passive;
    }
}

impl Deserialize for ElementSegment {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        // This piece of data was treated as `index` [of the table], but was repurposed
        // for flags in bulk-memory operations proposal.
        let flags: u32 = VarUint32::deserialize(reader)?.into();
        let index = if flags == FLAG_MEMZERO || flags == FLAG_PASSIVE {
            0u32
        } else if flags == FLAG_MEM_NONZERO {
            VarUint32::deserialize(reader)?.into()
        } else {
            return Err(Error::InvalidSegmentFlags(flags));
        };
        let offset = if flags == FLAG_PASSIVE {
            None
        } else {
            Some(InitExpr::deserialize(reader)?)
        };

        let members: Vec<u32> = CountedList::<VarUint32>::deserialize(reader)?
            .into_inner()
            .into_iter()
            .map(Into::into)
            .collect();

        Ok(ElementSegment {
            index,
            offset,
            members,
            passive: flags == FLAG_PASSIVE,
        })
    }
}

/// Data segment definition.
#[derive(Clone, Debug, PartialEq)]
pub struct DataSegment {
    index: u32,
    offset: Option<InitExpr>,
    value: Vec<u8>,
    passive: bool,
}

impl DataSegment {
    /// New data segments.
    pub fn new(index: u32, offset: Option<InitExpr>, value: Vec<u8>) -> Self {
        DataSegment {
            index,
            offset,
            value,
            passive: false,
        }
    }

    /// Linear memory index (currently the only valid value is `0`).
    pub fn index(&self) -> u32 {
        self.index
    }

    /// An i32 initializer expression that computes the offset at which to place the data.
    ///
    /// Note that this return `None` if the segment is `passive`.
    pub fn offset(&self) -> &Option<InitExpr> {
        &self.offset
    }

    /// An i32 initializer expression that computes the offset at which to place the data (mutable)
    ///
    /// Note that this return `None` if the segment is `passive`.
    pub fn offset_mut(&mut self) -> &mut Option<InitExpr> {
        &mut self.offset
    }

    /// Initial value of the data segment.
    pub fn value(&self) -> &[u8] {
        &self.value
    }

    /// Initial value of the data segment (mutable).
    pub fn value_mut(&mut self) -> &mut Vec<u8> {
        &mut self.value
    }
}

impl DataSegment {
    /// Whether or not this data segment is "passive".
    pub fn passive(&self) -> bool {
        self.passive
    }

    /// Whether or not this data segment is "passive" (mutable).
    pub fn passive_mut(&mut self) -> &mut bool {
        &mut self.passive
    }

    /// Set whether or not this table segment is "passive"
    pub fn set_passive(&mut self, passive: bool) {
        self.passive = passive;
    }
}

impl Deserialize for DataSegment {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let flags: u32 = VarUint32::deserialize(reader)?.into();
        let index = if flags == FLAG_MEMZERO || flags == FLAG_PASSIVE {
            0u32
        } else if flags == FLAG_MEM_NONZERO {
            VarUint32::deserialize(reader)?.into()
        } else {
            return Err(Error::InvalidSegmentFlags(flags));
        };
        let offset = if flags == FLAG_PASSIVE {
            None
        } else {
            Some(InitExpr::deserialize(reader)?)
        };
        let value_len = u32::from(VarUint32::deserialize(reader)?) as usize;
        let value = buffered_read!(VALUES_BUFFER_LENGTH, value_len, reader);

        Ok(DataSegment {
            index,
            offset,
            value,
            passive: flags == FLAG_PASSIVE,
        })
    }
}
