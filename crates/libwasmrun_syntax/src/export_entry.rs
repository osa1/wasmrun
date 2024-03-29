use crate::{io, Deserialize, Error, VarUint32, VarUint7};

/// Internal reference of the exported entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Internal {
    /// Function reference.
    Function(u32),
    /// Table reference.
    Table(u32),
    /// Memory reference.
    Memory(u32),
    /// Global reference.
    Global(u32),

    Tag(u32),
}

impl Deserialize for Internal {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let kind = VarUint7::deserialize(reader)?;
        match kind.into() {
            0x00 => Ok(Internal::Function(VarUint32::deserialize(reader)?.into())),
            0x01 => Ok(Internal::Table(VarUint32::deserialize(reader)?.into())),
            0x02 => Ok(Internal::Memory(VarUint32::deserialize(reader)?.into())),
            0x03 => Ok(Internal::Global(VarUint32::deserialize(reader)?.into())),
            0x04 => Ok(Internal::Tag(VarUint32::deserialize(reader)?.into())),
            _ => Err(Error::UnknownInternalKind(kind.into())),
        }
    }
}

/// Export entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportEntry {
    field: String,
    internal: Internal,
}

impl ExportEntry {
    /// New export entry.
    pub fn new(field: String, internal: Internal) -> Self {
        ExportEntry { field, internal }
    }

    /// Public name.
    pub fn field(&self) -> &str {
        &self.field
    }

    /// Public name (mutable).
    pub fn field_mut(&mut self) -> &mut String {
        &mut self.field
    }

    /// Internal reference of the export entry.
    pub fn internal(&self) -> &Internal {
        &self.internal
    }

    /// Internal reference of the export entry (mutable).
    pub fn internal_mut(&mut self) -> &mut Internal {
        &mut self.internal
    }
}

impl Deserialize for ExportEntry {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let field_str = String::deserialize(reader)?;
        let internal = Internal::deserialize(reader)?;

        Ok(ExportEntry {
            field: field_str,
            internal,
        })
    }
}
