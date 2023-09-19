use crate::{io, Deserialize, Error, InitExpr, Limits32, ReferenceType, TableType, Uint8};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table {
    pub ty: TableType,
    pub init: Option<InitExpr>,
}

impl Deserialize for Table {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let val = Uint8::deserialize(reader)?;
        let val_u8: u8 = val.into();
        if val_u8 == 0x40 {
            let val: u8 = Uint8::deserialize(reader)?.into();
            if val != 0 {
                return Err(Error::Other("")); // TODO
            }
            let elem_type = ReferenceType::deserialize(reader)?;
            let limits = Limits32::deserialize(reader)?;
            let ty = TableType { elem_type, limits };
            let init = InitExpr::deserialize(reader)?;
            Ok(Table {
                ty,
                init: Some(init),
            })
        } else {
            let elem_type = ReferenceType::deserialize_val(reader, val_u8)?;
            let limits = Limits32::deserialize(reader)?;
            let ty = TableType { elem_type, limits };
            Ok(Table { ty, init: None })
        }
    }
}
