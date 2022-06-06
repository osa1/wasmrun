use crate::io;

use super::{index_map::IndexMap, Deserialize, Error, Module, Type, VarUint32, VarUint7};

const NAME_TYPE_MODULE: u8 = 0;
const NAME_TYPE_FUNCTION: u8 = 1;
const NAME_TYPE_LOCAL: u8 = 2;

/// Debug name information.
#[derive(Clone, Debug, PartialEq)]
pub struct NameSection {
    /// Module name subsection.
    module: Option<ModuleNameSubsection>,

    /// Function name subsection.
    functions: Option<FunctionNameSubsection>,

    /// Local name subsection.
    locals: Option<LocalNameSubsection>,
}

impl NameSection {
    /// Creates a new name section.
    pub fn new(
        module: Option<ModuleNameSubsection>,
        functions: Option<FunctionNameSubsection>,
        locals: Option<LocalNameSubsection>,
    ) -> Self {
        Self {
            module,
            functions,
            locals,
        }
    }

    /// Module name subsection of this section.
    pub fn module(&self) -> Option<&ModuleNameSubsection> {
        self.module.as_ref()
    }

    /// Module name subsection of this section (mutable).
    pub fn module_mut(&mut self) -> &mut Option<ModuleNameSubsection> {
        &mut self.module
    }

    /// Functions name subsection of this section.
    pub fn functions(&self) -> Option<&FunctionNameSubsection> {
        self.functions.as_ref()
    }

    /// Functions name subsection of this section (mutable).
    pub fn functions_mut(&mut self) -> &mut Option<FunctionNameSubsection> {
        &mut self.functions
    }

    /// Local name subsection of this section.
    pub fn locals(&self) -> Option<&LocalNameSubsection> {
        self.locals.as_ref()
    }

    /// Local name subsection of this section (mutable).
    pub fn locals_mut(&mut self) -> &mut Option<LocalNameSubsection> {
        &mut self.locals
    }
}

impl NameSection {
    /// Deserialize a name section.
    pub fn deserialize<R: io::Read>(module: &Module, rdr: &mut R) -> Result<Self, Error> {
        let mut module_name: Option<ModuleNameSubsection> = None;
        let mut function_names: Option<FunctionNameSubsection> = None;
        let mut local_names: Option<LocalNameSubsection> = None;

        while let Ok(raw_subsection_type) = VarUint7::deserialize(rdr) {
            let subsection_type = raw_subsection_type.into();
            // deserialize the section size
            let size: usize = VarUint32::deserialize(rdr)?.into();

            match subsection_type {
                NAME_TYPE_MODULE => {
                    if module_name.is_some() {
                        return Err(Error::DuplicatedNameSubsections(NAME_TYPE_FUNCTION));
                    }
                    module_name = Some(ModuleNameSubsection::deserialize(rdr)?);
                }

                NAME_TYPE_FUNCTION => {
                    if function_names.is_some() {
                        return Err(Error::DuplicatedNameSubsections(NAME_TYPE_FUNCTION));
                    }
                    function_names = Some(FunctionNameSubsection::deserialize(module, rdr)?);
                }

                NAME_TYPE_LOCAL => {
                    if local_names.is_some() {
                        return Err(Error::DuplicatedNameSubsections(NAME_TYPE_LOCAL));
                    }
                    local_names = Some(LocalNameSubsection::deserialize(module, rdr)?);
                }

                _ => {
                    // Consume the entire subsection size and drop it. This allows other sections to still be
                    // consumed if there are any.
                    let mut buf = vec![0; size];
                    rdr.read(&mut buf)?;
                }
            };
        }

        Ok(Self {
            module: module_name,
            functions: function_names,
            locals: local_names,
        })
    }
}

/// The name of this module.
#[derive(Clone, Debug, PartialEq)]
pub struct ModuleNameSubsection {
    name: String,
}

impl ModuleNameSubsection {
    /// Create a new module name section with the specified name.
    pub fn new<S: Into<String>>(name: S) -> ModuleNameSubsection {
        ModuleNameSubsection { name: name.into() }
    }

    /// The name of this module.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// The name of this module (mutable).
    pub fn name_mut(&mut self) -> &mut String {
        &mut self.name
    }
}

impl Deserialize for ModuleNameSubsection {
    fn deserialize<R: io::Read>(rdr: &mut R) -> Result<ModuleNameSubsection, Error> {
        let name = String::deserialize(rdr)?;
        Ok(ModuleNameSubsection { name })
    }
}

/// The names of the functions in this module.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct FunctionNameSubsection {
    names: NameMap,
}

impl FunctionNameSubsection {
    /// A map from function indices to names.
    pub fn names(&self) -> &NameMap {
        &self.names
    }

    /// A map from function indices to names (mutable).
    pub fn names_mut(&mut self) -> &mut NameMap {
        &mut self.names
    }

    /// Deserialize names, making sure that all names correspond to functions.
    pub fn deserialize<R: io::Read>(
        module: &Module,
        rdr: &mut R,
    ) -> Result<FunctionNameSubsection, Error> {
        let names = IndexMap::deserialize(module.functions_space(), rdr)?;
        Ok(FunctionNameSubsection { names })
    }
}

/// The names of the local variables in this module's functions.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct LocalNameSubsection {
    local_names: IndexMap<NameMap>,
}

impl LocalNameSubsection {
    /// A map from function indices to a map from variables indices to names.
    pub fn local_names(&self) -> &IndexMap<NameMap> {
        &self.local_names
    }

    /// A map from function indices to a map from variables indices to names
    /// (mutable).
    pub fn local_names_mut(&mut self) -> &mut IndexMap<NameMap> {
        &mut self.local_names
    }

    /// Deserialize names, making sure that all names correspond to local
    /// variables.
    pub fn deserialize<R: io::Read>(
        module: &Module,
        rdr: &mut R,
    ) -> Result<LocalNameSubsection, Error> {
        let max_entry_space = module.functions_space();

        let max_signature_args = module
            .type_section()
            .map(|ts| {
                ts.types()
                    .iter()
                    .map(|x| {
                        let Type::Function(ref func) = *x;
                        func.params().len()
                    })
                    .max()
                    .unwrap_or(0)
            })
            .unwrap_or(0);

        let max_locals = module
            .code_section()
            .map(|cs| {
                cs.bodies()
                    .iter()
                    .map(|f| f.locals().iter().map(|l| l.count() as usize).sum())
                    .max()
                    .unwrap_or(0)
            })
            .unwrap_or(0);

        let max_space = max_signature_args + max_locals;

        let deserialize_locals = |_: u32, rdr: &mut R| IndexMap::deserialize(max_space, rdr);

        let local_names = IndexMap::deserialize_with(max_entry_space, &deserialize_locals, rdr)?;
        Ok(LocalNameSubsection { local_names })
    }
}

/// A map from indices to names.
pub type NameMap = IndexMap<String>;

#[cfg(test)]
mod tests {
    #[test]
    fn deserialize_local_names() {
        let module = super::super::deserialize_file("./res/cases/v1/names_with_imports.wasm")
            .expect("Should be deserialized")
            .parse_names()
            .expect("Names to be parsed");

        let name_section = module
            .names_section()
            .expect("name_section should be present");
        let local_names = name_section
            .locals()
            .expect("local_name_section should be present");

        let locals = local_names
            .local_names()
            .get(0)
            .expect("entry #0 should be present");
        assert_eq!(locals.get(0).expect("entry #0 should be present"), "abc");

        let locals = local_names
            .local_names()
            .get(1)
            .expect("entry #1 should be present");
        assert_eq!(locals.get(0).expect("entry #0 should be present"), "def");
    }
}
