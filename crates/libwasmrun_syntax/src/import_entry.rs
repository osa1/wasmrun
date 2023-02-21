use crate::{
    io, Deserialize, Error, ReferenceType, TagType, Uint8, ValueType, VarUint1, VarUint32,
    VarUint64, VarUint7,
};

/// Global definition struct
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct GlobalType {
    content_type: ValueType,
    is_mutable: bool,
}

impl GlobalType {
    /// New global type
    pub fn new(content_type: ValueType, is_mutable: bool) -> Self {
        GlobalType {
            content_type,
            is_mutable,
        }
    }

    /// Type of the global entry
    pub fn content_type(&self) -> ValueType {
        self.content_type
    }

    /// Is global entry is declared as mutable
    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}

impl Deserialize for GlobalType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let content_type = ValueType::deserialize(reader)?;
        let is_mutable = VarUint1::deserialize(reader)?;
        Ok(GlobalType {
            content_type,
            is_mutable: is_mutable.into(),
        })
    }
}

/// Table entry
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TableType {
    pub elem_type: ReferenceType,
    pub limits: Limits32,
}

impl TableType {
    /// New table definition
    pub fn new(elem_type: ReferenceType, min: u32, max: Option<u32>) -> Self {
        TableType {
            elem_type,
            limits: Limits32::new(min, max),
        }
    }

    /// Table memory specification
    pub fn limits(&self) -> &Limits32 {
        &self.limits
    }

    /// Table element type
    pub fn elem_type(&self) -> ReferenceType {
        self.elem_type
    }
}

impl Deserialize for TableType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let elem_type = ReferenceType::deserialize(reader)?;
        let limits = Limits32::deserialize(reader)?;
        Ok(TableType { elem_type, limits })
    }
}

/// Memory entry.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MemoryType(Limits);

impl MemoryType {
    /// New memory definition
    pub fn new(limits: Limits) -> Self {
        MemoryType(limits)
    }

    /// Limits of the memory entry.
    pub fn limits(&self) -> &Limits {
        &self.0
    }
}

impl Deserialize for MemoryType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(MemoryType(Limits::deserialize(reader)?))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Limits32 {
    initial: u32,
    maximum: Option<u32>,
    shared: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Limits64 {
    initial: u64,
    maximum: Option<u64>,
    shared: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Limits {
    Limits32(Limits32),
    Limits64(Limits64),
}

impl Limits {
    pub fn new_32(min: u32, max: Option<u32>) -> Self {
        Limits::Limits32(Limits32::new(min, max))
    }

    pub fn new_64(min: u64, max: Option<u64>) -> Self {
        Limits::Limits64(Limits64::new(min, max))
    }
}

impl Limits32 {
    /// New memory limits definition.
    pub fn new(min: u32, max: Option<u32>) -> Self {
        Limits32 {
            initial: min,
            maximum: max,
            shared: false,
        }
    }

    /// Initial size.
    pub fn initial(&self) -> u32 {
        self.initial
    }

    /// Maximum size.
    pub fn maximum(&self) -> Option<u32> {
        self.maximum
    }

    /// Whether the table or memory is shared.
    pub fn shared(&self) -> bool {
        self.shared
    }
}

impl Limits64 {
    pub fn new(min: u64, max: Option<u64>) -> Self {
        Limits64 {
            initial: min,
            maximum: max,
            shared: false,
        }
    }

    /// Initial size.
    pub fn initial(&self) -> u64 {
        self.initial
    }

    /// Maximum size.
    pub fn maximum(&self) -> Option<u64> {
        self.maximum
    }

    /// Whether the table or memory is shared.
    pub fn shared(&self) -> bool {
        self.shared
    }
}

impl Deserialize for Limits {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let flags: u8 = Uint8::deserialize(reader)?.into();

        if flags >> 3 != 0 {
            return Err(Error::InvalidLimitsFlags(flags));
        }

        let has_max = flags & 0b001 != 0;
        let shared = flags & 0b010 != 0;
        let bit64 = flags & 0b100 != 0;

        if bit64 {
            Ok(Limits::Limits64(Limits64::deserialize_rest(
                reader, has_max, shared,
            )?))
        } else {
            Ok(Limits::Limits32(Limits32::deserialize_rest(
                reader, has_max, shared,
            )?))
        }
    }
}

impl Limits32 {
    fn deserialize_rest<R: io::Read>(
        reader: &mut R,
        has_max: bool,
        shared: bool,
    ) -> Result<Self, Error> {
        let initial = VarUint32::deserialize(reader)?;
        let maximum = if has_max {
            Some(VarUint32::deserialize(reader)?.into())
        } else {
            None
        };
        Ok(Limits32 {
            initial: initial.into(),
            maximum,
            shared,
        })
    }
}

impl Deserialize for Limits32 {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let flags: u8 = Uint8::deserialize(reader)?.into();

        if flags >> 2 != 0 {
            return Err(Error::InvalidLimitsFlags(flags));
        }

        let has_max = flags & 0b001 != 0;
        let shared = flags & 0b010 != 0;
        Limits32::deserialize_rest(reader, has_max, shared)
    }
}

impl Limits64 {
    fn deserialize_rest<R: io::Read>(
        reader: &mut R,
        has_max: bool,
        shared: bool,
    ) -> Result<Self, Error> {
        let initial = VarUint64::deserialize(reader)?;
        let maximum = if has_max {
            Some(VarUint64::deserialize(reader)?.into())
        } else {
            None
        };
        Ok(Limits64 {
            initial: initial.into(),
            maximum,
            shared,
        })
    }
}

/// External to local binding.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum External {
    /// Binds to a function whose type is associated with the given index in the
    /// type section.
    Function(u32),
    /// Describes local table definition to be imported as.
    Table(TableType),
    /// Describes local memory definition to be imported as.
    Memory(MemoryType),
    /// Describes local global entry to be imported as.
    Global(GlobalType),

    Tag(TagType),
}

impl Deserialize for External {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let kind = VarUint7::deserialize(reader)?;
        match kind.into() {
            0x00 => Ok(External::Function(VarUint32::deserialize(reader)?.into())),
            0x01 => Ok(External::Table(TableType::deserialize(reader)?)),
            0x02 => Ok(External::Memory(MemoryType::deserialize(reader)?)),
            0x03 => Ok(External::Global(GlobalType::deserialize(reader)?)),
            0x04 => Ok(External::Tag(TagType::deserialize(reader)?)),
            _ => Err(Error::UnknownExternalKind(kind.into())),
        }
    }
}

/// Import entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportEntry {
    module_str: String,
    field_str: String,
    external: External,
}

impl ImportEntry {
    /// New import entry.
    pub fn new(module_str: String, field_str: String, external: External) -> Self {
        ImportEntry {
            module_str,
            field_str,
            external,
        }
    }

    /// Module reference of the import entry.
    pub fn module(&self) -> &str {
        &self.module_str
    }

    /// Module reference of the import entry (mutable).
    pub fn module_mut(&mut self) -> &mut String {
        &mut self.module_str
    }

    /// Field reference of the import entry.
    pub fn field(&self) -> &str {
        &self.field_str
    }

    /// Field reference of the import entry (mutable)
    pub fn field_mut(&mut self) -> &mut String {
        &mut self.field_str
    }

    /// Local binidng of the import entry.
    pub fn external(&self) -> &External {
        &self.external
    }

    /// Local binidng of the import entry (mutable)
    pub fn external_mut(&mut self) -> &mut External {
        &mut self.external
    }
}

impl Deserialize for ImportEntry {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let module_str = String::deserialize(reader)?;
        let field_str = String::deserialize(reader)?;
        let external = External::deserialize(reader)?;

        Ok(ImportEntry {
            module_str,
            field_str,
            external,
        })
    }
}
