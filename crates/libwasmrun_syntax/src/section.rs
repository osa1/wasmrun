use crate::{
    io, name_section::NameSection, reloc_section::RelocSection, types::Type, CountedList,
    DataSegment, Deserialize, ElementSegment, Error, ExportEntry, Func, FuncBody, GlobalEntry,
    ImportEntry, MemoryType, TableType, VarUint32, VarUint7,
};

const ENTRIES_BUFFER_LENGTH: usize = 16384;

/// A section in a WebAssembly module
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Section {
    Type(TypeSection),
    Import(ImportSection),
    Function(FunctionSection),
    Table(TableSection),
    Memory(MemorySection),
    Global(GlobalSection),
    Export(ExportSection),
    Start(u32),
    Element(ElementSection),
    DataCount(u32),
    Code(CodeSection),
    Data(DataSection),

    /// A custom section
    Custom(CustomSection),

    /// A name section.
    ///
    /// Note that initially it is not parsed until `parse_names` is called explicitly.
    Name(NameSection),

    /// A relocation section.
    ///
    /// Note that relocation sections are not parsed until `parse_reloc` is called explicitly.
    /// Also, currently there are serialization (but not de-serialization) issues with this section
    /// (parity-wasm#198).
    Reloc(RelocSection),
}

impl Deserialize for Section {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let id = match VarUint7::deserialize(reader) {
            Err(_) => return Err(Error::UnexpectedEof),
            Ok(id) => id,
        };

        Ok(match id.into() {
            0 => Section::Custom(CustomSection::deserialize(reader)?),
            1 => Section::Type(TypeSection::deserialize(reader)?),
            2 => Section::Import(ImportSection::deserialize(reader)?),
            3 => Section::Function(FunctionSection::deserialize(reader)?),
            4 => Section::Table(TableSection::deserialize(reader)?),
            5 => Section::Memory(MemorySection::deserialize(reader)?),
            6 => Section::Global(GlobalSection::deserialize(reader)?),
            7 => Section::Export(ExportSection::deserialize(reader)?),
            8 => {
                let mut section_reader = SectionReader::new(reader)?;
                let start_idx = VarUint32::deserialize(&mut section_reader)?;
                section_reader.close()?;
                Section::Start(start_idx.into())
            }
            9 => Section::Element(ElementSection::deserialize(reader)?),
            10 => Section::Code(CodeSection::deserialize(reader)?),
            11 => Section::Data(DataSection::deserialize(reader)?),
            12 => {
                let mut section_reader = SectionReader::new(reader)?;
                let count = VarUint32::deserialize(&mut section_reader)?;
                section_reader.close()?;
                Section::DataCount(count.into())
            }
            invalid_id => return Err(Error::InvalidSectionId(invalid_id)),
        })
    }
}

impl Section {
    /// Order of the section in a valid Wasm module. Custom sections are the last and have
    /// `u8::MAX` as the order.
    ///
    /// Note that order or a section is not the same as its id. For example, data count section id
    /// is smaller than code section id, but data count section needs to come before code section
    /// in a valid Wasm module.
    pub(crate) fn order(&self) -> u8 {
        match *self {
            Section::Type(_) => 0,
            Section::Import(_) => 1,
            Section::Function(_) => 2,
            Section::Table(_) => 3,
            Section::Memory(_) => 4,
            Section::Global(_) => 5,
            Section::Export(_) => 6,
            Section::Start(_) => 7,
            Section::Element(_) => 8,
            Section::DataCount(_) => 9,
            Section::Code(_) => 10,
            Section::Data(_) => 11,

            // Custom sections come last
            Section::Custom(_) | Section::Name(_) | Section::Reloc(_) => u8::MAX,
        }
    }
}

pub(crate) struct SectionReader {
    cursor: io::Cursor<Vec<u8>>,
    declared_length: usize,
}

impl SectionReader {
    pub fn new<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let length = u32::from(VarUint32::deserialize(reader)?) as usize;
        let inner_buffer = buffered_read!(ENTRIES_BUFFER_LENGTH, length, reader);
        let declared_length = inner_buffer.len();
        let cursor = io::Cursor::new(inner_buffer);

        Ok(SectionReader {
            cursor,
            declared_length,
        })
    }

    pub fn close(self) -> Result<(), io::Error> {
        let cursor = self.cursor;
        let buf_length = self.declared_length;

        if cursor.position() != buf_length {
            Err(io::Error::InvalidData)
        } else {
            Ok(())
        }
    }
}

impl io::Read for SectionReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<()> {
        self.cursor.read(buf)?;
        Ok(())
    }
}

fn read_entries<R: io::Read, T: Deserialize>(reader: &mut R) -> Result<Vec<T>, Error> {
    let mut section_reader = SectionReader::new(reader)?;
    let result = CountedList::<T>::deserialize(&mut section_reader)?.into_inner();
    section_reader.close()?;
    Ok(result)
}

/// Custom section.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CustomSection {
    name: String,
    payload: Vec<u8>,
}

impl CustomSection {
    /// Creates a new custom section with the given name and payload.
    pub fn new(name: String, payload: Vec<u8>) -> CustomSection {
        CustomSection { name, payload }
    }

    /// Name of the custom section.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Payload of the custom section.
    pub fn payload(&self) -> &[u8] {
        &self.payload
    }

    /// Name of the custom section (mutable).
    pub fn name_mut(&mut self) -> &mut String {
        &mut self.name
    }

    /// Payload of the custom section (mutable).
    pub fn payload_mut(&mut self) -> &mut Vec<u8> {
        &mut self.payload
    }
}

impl Deserialize for CustomSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        let section_length: usize = u32::from(VarUint32::deserialize(reader)?) as usize;
        let buf = buffered_read!(ENTRIES_BUFFER_LENGTH, section_length, reader);
        let mut cursor = io::Cursor::new(&buf[..]);
        let name = String::deserialize(&mut cursor)?;
        let payload = buf[cursor.position() as usize..].to_vec();
        Ok(CustomSection { name, payload })
    }
}

/// Section with type declarations.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct TypeSection(Vec<Type>);

impl TypeSection {
    pub fn with_entries(types: Vec<Type>) -> Self {
        TypeSection(types)
    }

    pub fn entries(&self) -> &[Type] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<Type> {
        &mut self.0
    }
}

impl Deserialize for TypeSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(TypeSection(read_entries(reader)?))
    }
}

/// Section of the imports definition.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ImportSection(Vec<ImportEntry>);

impl ImportSection {
    pub fn with_entries(entries: Vec<ImportEntry>) -> Self {
        ImportSection(entries)
    }

    pub fn entries(&self) -> &[ImportEntry] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<ImportEntry> {
        &mut self.0
    }
}

impl Deserialize for ImportSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(ImportSection(read_entries(reader)?))
    }
}

/// Section with function signatures definition.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct FunctionSection(Vec<Func>);

impl FunctionSection {
    pub fn with_entries(entries: Vec<Func>) -> Self {
        FunctionSection(entries)
    }

    pub fn entries(&self) -> &[Func] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<Func> {
        &mut self.0
    }
}

impl Deserialize for FunctionSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(FunctionSection(read_entries(reader)?))
    }
}

/// Section with table definition (currently only one is allowed).
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TableSection(Vec<TableType>);

impl TableSection {
    pub fn with_entries(entries: Vec<TableType>) -> Self {
        TableSection(entries)
    }

    pub fn entries(&self) -> &[TableType] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<TableType> {
        &mut self.0
    }
}

impl Deserialize for TableSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(TableSection(read_entries(reader)?))
    }
}

/// Section with table definition (currently only one entry is allowed).
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct MemorySection(Vec<MemoryType>);

impl MemorySection {
    pub fn with_entries(entries: Vec<MemoryType>) -> Self {
        MemorySection(entries)
    }

    pub fn entries(&self) -> &[MemoryType] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<MemoryType> {
        &mut self.0
    }
}

impl Deserialize for MemorySection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(MemorySection(read_entries(reader)?))
    }
}

/// Globals definition section.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct GlobalSection(Vec<GlobalEntry>);

impl GlobalSection {
    pub fn with_entries(entries: Vec<GlobalEntry>) -> Self {
        GlobalSection(entries)
    }

    pub fn entries(&self) -> &[GlobalEntry] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<GlobalEntry> {
        &mut self.0
    }
}

impl Deserialize for GlobalSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(GlobalSection(read_entries(reader)?))
    }
}

/// List of exports definition.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ExportSection(Vec<ExportEntry>);

impl ExportSection {
    pub fn with_entries(entries: Vec<ExportEntry>) -> Self {
        ExportSection(entries)
    }

    pub fn entries(&self) -> &[ExportEntry] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<ExportEntry> {
        &mut self.0
    }
}

impl Deserialize for ExportSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(ExportSection(read_entries(reader)?))
    }
}

/// Section with function bodies of the module.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct CodeSection(Vec<FuncBody>);

impl CodeSection {
    pub fn with_entries(entries: Vec<FuncBody>) -> Self {
        CodeSection(entries)
    }

    pub fn entries(&self) -> &[FuncBody] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<FuncBody> {
        &mut self.0
    }
}

impl Deserialize for CodeSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(CodeSection(read_entries(reader)?))
    }
}

/// Element entries section.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ElementSection(Vec<ElementSegment>);

impl ElementSection {
    pub fn with_entries(entries: Vec<ElementSegment>) -> Self {
        ElementSection(entries)
    }

    pub fn entries(&self) -> &[ElementSegment] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<ElementSegment> {
        &mut self.0
    }
}

impl Deserialize for ElementSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(ElementSection(read_entries::<R, ElementSegment>(reader)?))
    }
}

/// Data entries definitions.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct DataSection(Vec<DataSegment>);

impl DataSection {
    pub fn with_entries(entries: Vec<DataSegment>) -> Self {
        DataSection(entries)
    }

    pub fn entries(&self) -> &[DataSegment] {
        &self.0
    }

    pub fn entries_mut(&mut self) -> &mut Vec<DataSegment> {
        &mut self.0
    }
}

impl Deserialize for DataSection {
    fn deserialize<R: io::Read>(reader: &mut R) -> Result<Self, Error> {
        Ok(DataSection(read_entries(reader)?))
    }
}

#[cfg(test)]
mod tests {
    use crate::{deserialize_buffer, deserialize_file, Section, Type, TypeSection, ValueType};

    #[test]
    fn import_section() {
        let module = deserialize_file("./res/cases/v1/test5.wasm").expect("Should be deserialized");
        let mut found = false;
        for section in module.sections() {
            if let Section::Import(ref import_section) = *section {
                assert_eq!(25, import_section.entries().len());
                found = true
            }
        }
        assert!(found, "There should be import section in test5.wasm");
    }

    const FUNCTIONS_TEST_PAYLOAD: [u8; 13] = [
        3, // section id
        0x87, 0x80, 0x80, 0x80, 0x0,  // section length
        4,    // number of functions
        0x01, // type ref 1
        0x86, 0x80, 0x00, // type ref 2
        0x09, // type ref 3
        0x33, // type ref 4
    ];

    #[test]
    fn fn_section_detect() {
        let section: Section =
            deserialize_buffer(&FUNCTIONS_TEST_PAYLOAD).expect("section to be deserialized");

        assert!(
            matches!(section, Section::Function(_)),
            "Payload should be recognized as functions section"
        );
    }

    #[test]
    fn fn_section_number() {
        let section: Section =
            deserialize_buffer(&FUNCTIONS_TEST_PAYLOAD).expect("section to be deserialized");

        if let Section::Function(fn_section) = section {
            assert_eq!(
                4,
                fn_section.entries().len(),
                "There should be 4 functions total"
            );
        }
    }

    #[test]
    fn fn_section_ref() {
        let section: Section =
            deserialize_buffer(&FUNCTIONS_TEST_PAYLOAD).expect("section to be deserialized");

        if let Section::Function(fn_section) = section {
            assert_eq!(6, fn_section.entries()[1].type_ref());
        }
    }

    #[rustfmt::skip]
    const TYPE_PAYLOAD: [u8; 12] = [
        11,     // section size
        2,      // 2 functions
        0x60,   // constant
        1,      // 1 arg
        0x7e,
        0x00,   // 0 return
        0x60,   // constant
        2,      // 2 args
        0x7e,
        0x7d,
        0x01,   // 1 return
        0x7e,
    ];

    #[test]
    fn type_section_len() {
        let type_section: TypeSection =
            deserialize_buffer(&TYPE_PAYLOAD).expect("type_section be deserialized");

        assert_eq!(type_section.entries().len(), 2);
    }

    #[test]
    fn type_section_infer() {
        let type_section: TypeSection =
            deserialize_buffer(&TYPE_PAYLOAD).expect("type_section be deserialized");

        let Type::Function(ref t1) = type_section.entries()[1];
        assert_eq!(vec![ValueType::I64], t1.results());
        assert_eq!(2, t1.params().len());
    }

    #[test]
    fn export_detect() {
        let export_payload = [
            0x07, // section id = 7
            28,   // section length = 28
            6,    // num entries
            // [name_len(1-5 bytes), name_bytes(name_len, internal_kind(1byte), internal_index(1-5 bytes)])
            0x01, 0x41, 0x01, 0x86, 0x80, 0x00, // func "A", index 6
            0x01, 0x42, 0x01, 0x86, 0x00, // func "B", index 8
            0x01, 0x43, 0x01, 0x07, // func "C", index 7
            0x01, 0x44, 0x02, 0x00, // memory "D", index 0
            0x01, 0x45, 0x01, 0x01, // func "E", index 1
            0x01, 0x46, 0x01, 0x02, // func "F", index 2
        ];

        let section: Section =
            deserialize_buffer(&export_payload).expect("section to be deserialized");

        assert!(
            matches!(section, Section::Export(_)),
            "Payload should be recognized as export section"
        );
    }

    #[test]
    fn code_detect() {
        let code_payload = [
            0x0A, // section id = 10
            32,   // section length
            1,    // function count
            30,   // function body size
            1,    // local vedc length
            0x01, 0x7F, // one local of type i32
            0x02, 0x7F, // block i32
            0x23, 0x00, // get_global 0
            0x21, 0x01, // set_local 1
            0x23, 0x00, // get_global 0
            0x20, 0x00, // get_local 0
            0x6A, // i32.add
            0x24, 0x00, // set_global 0
            0x23, 0x00, // get_global 0
            0x41, 0x0F, // i32.const 15
            0x6A, // i32.add
            0x41, 0x70, // i32.const -16
            0x71, // i32.and
            0x24, 0x00, // set_global 0
            0x20, 0x01, // get_local 1
            0x0B, // end block
            0x0B, // end expr
        ];

        let section: Section =
            deserialize_buffer(&code_payload).expect("section to be deserialized");

        assert!(
            matches!(section, Section::Code(_)),
            "Payload should be recognized as a code section"
        );
    }

    fn data_payload() -> &'static [u8] {
        &[
            0x0bu8, // section id
            20,     // section length
            0x01,   // number of segments
            0x00,   // index
            0x0b,   // end
            0x10,   // 16x 0x00
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ]
    }

    #[test]
    fn data_section_detect() {
        let section: Section =
            deserialize_buffer(data_payload()).expect("section to be deserialized");

        assert!(
            matches!(section, Section::Data(_)),
            "Payload should be recognized as a data section"
        );
    }
}
