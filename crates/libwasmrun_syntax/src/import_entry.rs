use super::{Deserialize, Error, ReferenceType, Uint8, ValueType, VarUint1, VarUint32, VarUint7};
use crate::io;

const FLAG_HAS_MAX: u8 = 0x01;
const FLAG_SHARED: u8 = 0x02;

/// Global definition struct
#[derive(Debug, Copy, Clone, PartialEq)]
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
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TableType {
    pub elem_type: ReferenceType,
    pub limits: ResizableLimits,
}

impl TableType {
    /// New table definition
    pub fn new(elem_type: ReferenceType, min: u32, max: Option<u32>) -> Self {
        TableType {
            elem_type,
            limits: ResizableLimits::new(min, max),
        }
    }

    /// Table memory specification
    pub fn limits(&self) -> &ResizableLimits {
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
        let limits = ResizableLimits::deserialize(reader)?;
        Ok(TableType { elem_type, limits })
    }
}

/// Memory and table limits.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ResizableLimits {
    initial: u32,
    maximum: Option<u32>,
    shared: bool,
}

impl ResizableLimits {
    /// New memory limits definition.
    pub fn new(min: u32, max: Option<u32>) -> Self {
        ResizableLimits {
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

    /// Whether or not this is a shared array buffer.
    pub fn shared(&self) -> bool {
        self.shared
    }
}

impl Deserialize for ResizableLimits {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let flags: u8 = Uint8::deserialize(reader)?.into();
        match flags {
            // Default flags are always supported. This is simply: FLAG_HAS_MAX={true, false}.
            0x00 | 0x01 => {}

            // Atomics proposal introduce FLAG_SHARED (0x02). Shared memories can be used only
            // together with FLAG_HAS_MAX (0x01), hence 0x03.
            0x03 => {}

            _ => return Err(Error::InvalidLimitsFlags(flags)),
        }

        let initial = VarUint32::deserialize(reader)?;
        let maximum = if flags & FLAG_HAS_MAX != 0 {
            Some(VarUint32::deserialize(reader)?.into())
        } else {
            None
        };

        Ok(ResizableLimits {
            initial: initial.into(),
            maximum,
            shared: flags & FLAG_SHARED != 0,
        })
    }
}

/// Memory entry.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct MemoryType(ResizableLimits);

impl MemoryType {
    /// New memory definition
    pub fn new(min: u32, max: Option<u32>) -> Self {
        let r = ResizableLimits::new(min, max);
        MemoryType(r)
    }

    /// Set the `shared` flag that denotes a memory that can be shared between threads.
    ///
    /// `false` by default. This is only available if the `atomics` feature is enabled.
    pub fn set_shared(&mut self, shared: bool) {
        self.0.shared = shared;
    }

    /// Limits of the memory entry.
    pub fn limits(&self) -> &ResizableLimits {
        &self.0
    }
}

impl Deserialize for MemoryType {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(MemoryType(ResizableLimits::deserialize(reader)?))
    }
}

/// External to local binding.
#[derive(Debug, Copy, Clone, PartialEq)]
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
}

impl Deserialize for External {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let kind = VarUint7::deserialize(reader)?;
        match kind.into() {
            0x00 => Ok(External::Function(VarUint32::deserialize(reader)?.into())),
            0x01 => Ok(External::Table(TableType::deserialize(reader)?)),
            0x02 => Ok(External::Memory(MemoryType::deserialize(reader)?)),
            0x03 => Ok(External::Global(GlobalType::deserialize(reader)?)),
            _ => Err(Error::UnknownExternalKind(kind.into())),
        }
    }
}

/// Import entry.
#[derive(Debug, Clone, PartialEq)]
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
