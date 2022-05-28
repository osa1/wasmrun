use crate::elements::{
    CountedList, Deserialize, Error, InitExpr, Instruction, ReferenceType, Uint8, VarUint32,
};
use crate::io;

const FLAG_MEMZERO: u32 = 0;
const FLAG_PASSIVE: u32 = 1;
const FLAG_MEM_NONZERO: u32 = 2;

const VALUES_BUFFER_LENGTH: usize = 16384;

/// Entry in an element section
#[derive(Debug, Clone, PartialEq)]
pub struct ElementSegment {
    pub ref_type: ReferenceType,
    pub init: Vec<InitExpr>,
    pub mode: ElementSegmentMode,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElementSegmentMode {
    Passive,
    Active { table_idx: u32, offset: InitExpr },
    Declarative,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElementKind {
    FuncRef,
}

impl ElementKind {
    fn to_ref_type(&self) -> ReferenceType {
        match self {
            ElementKind::FuncRef => ReferenceType::FuncRef,
        }
    }
}

/*
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
*/

fn func_idx_vec_to_init(func_idxs: &[u32]) -> Vec<InitExpr> {
    func_idxs
        .iter()
        .map(|func_idx| InitExpr::new(vec![Instruction::RefFunc(*func_idx), Instruction::End]))
        .collect()
}

impl Deserialize for ElementSegment {
    // https://webassembly.github.io/spec/core/binary/modules.html#element-section
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let flags: u32 = VarUint32::deserialize(reader)?.into();

        let bit_0 = flags & 0b1 != 0;
        let bit_1 = flags & 0b10 != 0;
        let bit_2 = flags & 0b100 != 0;

        // Passive or declarative if set, active otherwise
        let passive_or_declarative = bit_0;

        // Use element type and expr instead of element kind and element indices
        let element_type_and_expr = bit_2;

        if passive_or_declarative {
            let declerative = bit_1;
            if element_type_and_expr {
                let element_type = ReferenceType::deserialize(reader)?;
                let init: Vec<InitExpr> = CountedList::deserialize(reader)?.into_inner();
                Ok(ElementSegment {
                    ref_type: element_type,
                    init,
                    mode: if declerative {
                        ElementSegmentMode::Declarative
                    } else {
                        ElementSegmentMode::Passive
                    },
                })
            } else {
                let element_kind = ElementKind::deserialize(reader)?;
                let func_idxs: Vec<u32> = CountedList::<VarUint32>::deserialize(reader)?
                    .into_inner()
                    .into_iter()
                    .map(Into::into)
                    .collect();
                Ok(ElementSegment {
                    ref_type: element_kind.to_ref_type(),
                    init: func_idx_vec_to_init(&func_idxs),
                    mode: if declerative {
                        ElementSegmentMode::Declarative
                    } else {
                        ElementSegmentMode::Passive
                    },
                })
            }
        } else {
            // Active
            let with_table_idx = bit_1;
            let table_idx: u32 = if with_table_idx {
                VarUint32::deserialize(reader)?.into()
            } else {
                0
            };
            if element_type_and_expr {
                let offset = InitExpr::deserialize(reader)?;
                let ref_type = ReferenceType::deserialize(reader)?;
                let init = CountedList::<InitExpr>::deserialize(reader)?.into_inner();
                Ok(ElementSegment {
                    ref_type,
                    init,
                    mode: ElementSegmentMode::Active { table_idx, offset },
                })
            } else {
                // Element kind and element indices
                let offset = InitExpr::deserialize(reader)?;
                let func_idxs: Vec<u32> = CountedList::<VarUint32>::deserialize(reader)?
                    .into_inner()
                    .into_iter()
                    .map(Into::into)
                    .collect();
                Ok(ElementSegment {
                    ref_type: ReferenceType::FuncRef,
                    init: func_idx_vec_to_init(&func_idxs),
                    mode: ElementSegmentMode::Active { table_idx, offset },
                })
            }
        }
    }
}

impl Deserialize for ElementKind {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let kind: u8 = Uint8::deserialize(reader)?.into();
        match kind {
            0 => Ok(ElementKind::FuncRef),
            _ => Err(Error::UnknownElementKind(kind)),
        }
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

#[test]
fn element_segment_flags_000() {
    let section: [u8; 5] = [
        0x00, // tag = 0
        0x41, 0x00, // i32.const 0
        0x0b, // end
        0x00, // vec(funcidx) size = 0
    ];

    let mut buffer = io::Cursor::new(section);
    assert_eq!(
        ElementSegment::deserialize(&mut buffer).unwrap(),
        ElementSegment {
            ref_type: ReferenceType::FuncRef,
            init: vec![],
            mode: ElementSegmentMode::Active {
                table_idx: 0,
                offset: InitExpr::new(vec![Instruction::I32Const(0), Instruction::End]),
            },
        }
    );
}
