use crate::{
    io, CountedList, Deserialize, Error, InitExpr, Instruction, ReferenceType, Uint8, VarUint32,
};

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
            let offset = InitExpr::deserialize(reader)?;
            if element_type_and_expr {
                let init = CountedList::<InitExpr>::deserialize(reader)?.into_inner();
                Ok(ElementSegment {
                    ref_type: ReferenceType::FuncRef,
                    init,
                    mode: ElementSegmentMode::Active { table_idx, offset },
                })
            } else {
                // Element kind and element indices
                let _elem_kind = ElementKind::deserialize(reader)?;
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
    pub data: Vec<u8>,
    pub mode: DataSegmentMode,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataSegmentMode {
    Passive,
    Active { mem_idx: usize, offset: InitExpr },
}

impl Deserialize for DataSegment {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let flags: u32 = VarUint32::deserialize(reader)?.into();
        if flags >> 2 != 0 {
            return Err(Error::InvalidSegmentFlags(flags));
        }

        let bit_0 = flags & 0b1 != 0;
        let bit_1 = flags & 0b10 != 0;

        let passive = bit_0;
        let explicit_memory_idx = bit_1;

        if passive {
            let data_len = u32::from(VarUint32::deserialize(reader)?) as usize;
            let data = buffered_read!(VALUES_BUFFER_LENGTH, data_len, reader);
            Ok(DataSegment {
                data,
                mode: DataSegmentMode::Passive,
            })
        } else {
            let mem_idx = if explicit_memory_idx {
                VarUint32::deserialize(reader)?.into()
            } else {
                0
            };
            let offset = InitExpr::deserialize(reader)?;
            let data_len = u32::from(VarUint32::deserialize(reader)?) as usize;
            let data = buffered_read!(VALUES_BUFFER_LENGTH, data_len, reader);
            Ok(DataSegment {
                data,
                mode: DataSegmentMode::Active { mem_idx, offset },
            })
        }
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
