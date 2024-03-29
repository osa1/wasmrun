mod table;

use crate::exec::Runtime;
use crate::fun::{Fun, FunKind};
use crate::mem::Mem;
use crate::module::{Module, TypeIdx};
use crate::value::{Ref, Value};
use crate::Result;
pub use table::Table;

use libwasmrun_syntax::{self as wasm, IndexMap};

use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleAddr(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunAddr(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExternAddr(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct TableAddr(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemAddr(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct GlobalAddr(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct DataAddr(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct ElemAddr(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TagAddr(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExnAddr(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArrayAddr(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructAddr(pub(crate) u32);

#[derive(Default)]
pub struct Store {
    modules: Vec<Module>,
    funs: Vec<Fun>,
    tables: Vec<Table>,
    mems: Vec<Mem>,
    globals: Vec<Global>,
    tags: Vec<Tag>,
    datas: Vec<wasm::DataSegment>,
    elems: Vec<wasm::ElementSegment>,
    exceptions: Vec<Exception>,
    arrays: Vec<Array>,
    structs: Vec<Struct>,

    /// Internal references converted to `extern` with `extern.convert_any`.
    externalized_refs: Vec<Ref>,
}

#[derive(Debug, Clone)]
pub(crate) struct Exception {
    /// Address of the exception's tag.
    pub(crate) tag_addr: TagAddr,

    /// Arguments of the exception. Used by `throw` blocks.
    pub(crate) args: Vec<Value>,
}

#[derive(Debug, Clone)]
pub(crate) struct Array {
    /// Address of the module that defines the array's type.
    pub(crate) module_addr: ModuleAddr,

    /// Index of the array's type in its module.
    pub(crate) ty_idx: TypeIdx,

    /// The array contents.
    ///
    /// For now store as `Value` to keep things simple.
    pub(crate) elems: Vec<Value>,
}

impl Array {
    pub(crate) fn rtt(&self) -> (ModuleAddr, wasm::HeapType) {
        (self.module_addr, wasm::HeapType::TypeIdx(self.ty_idx.0))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Struct {
    /// Address of the module that defines the struct's type.
    pub(crate) module_addr: ModuleAddr,

    /// Index of the struct's type in its module.
    pub(crate) ty_idx: TypeIdx,

    pub(crate) fields: Vec<Value>,
}

impl Struct {
    pub(crate) fn rtt(&self) -> (ModuleAddr, wasm::HeapType) {
        (self.module_addr, wasm::HeapType::TypeIdx(self.ty_idx.0))
    }
}

impl Store {
    pub(crate) fn allocate_module(&mut self, module: Module) -> ModuleAddr {
        let ret = self.next_module_addr();
        self.modules.push(module);
        ret
    }

    pub(crate) fn next_module_addr(&self) -> ModuleAddr {
        ModuleAddr(self.modules.len() as u32)
    }

    pub(crate) fn get_module(&self, module_addr: ModuleAddr) -> &Module {
        &self.modules[module_addr.0 as usize]
    }

    pub(crate) fn get_module_mut(&mut self, module_addr: ModuleAddr) -> &mut Module {
        &mut self.modules[module_addr.0 as usize]
    }

    pub(crate) fn allocate_fun(
        &mut self,
        module_addr: ModuleAddr,
        ty_idx: TypeIdx,
        fun: wasm::FuncBody,
        name: Option<String>,
        local_names: Option<IndexMap<String>>,
    ) -> Result<FunAddr> {
        let fun_addr = FunAddr(self.funs.len() as u32);
        self.funs.push(Fun::new(
            module_addr,
            ty_idx,
            fun_addr,
            fun,
            name,
            local_names,
        )?);
        Ok(fun_addr)
    }

    pub(crate) fn allocate_host_fun(
        &mut self,
        module_addr: ModuleAddr,
        ty_idx: TypeIdx,
        fun: Rc<dyn Fn(&mut Runtime, Option<MemAddr>) -> Result<Vec<Value>>>,
    ) -> FunAddr {
        let fun_addr = FunAddr(self.funs.len() as u32);
        self.funs.push(Fun {
            module_addr,
            ty_idx,
            fun_addr,
            kind: FunKind::Host(fun),
        });
        fun_addr
    }

    pub fn get_fun(&self, fun_addr: FunAddr) -> &Fun {
        &self.funs[fun_addr.0 as usize]
    }

    pub(crate) fn allocate_table(&mut self, table: Table) -> TableAddr {
        let ret = self.tables.len() as u32;
        self.tables.push(table);
        TableAddr(ret)
    }

    pub(crate) fn get_table(&self, table_addr: TableAddr) -> &Table {
        &self.tables[table_addr.0 as usize]
    }

    pub(crate) fn get_table_mut(&mut self, table_addr: TableAddr) -> &mut Table {
        &mut self.tables[table_addr.0 as usize]
    }

    pub(crate) fn allocate_mem(&mut self, mem: Mem) -> MemAddr {
        let ret = self.mems.len() as u32;
        self.mems.push(mem);
        MemAddr(ret)
    }

    pub(crate) fn get_mem(&self, mem_addr: MemAddr) -> &Mem {
        &self.mems[mem_addr.0 as usize]
    }

    pub(crate) fn get_mem_mut(&mut self, mem_addr: MemAddr) -> &mut Mem {
        &mut self.mems[mem_addr.0 as usize]
    }

    pub(crate) fn allocate_global(&mut self, global: Global) -> GlobalAddr {
        let ret = self.globals.len() as u32;
        self.globals.push(global);
        GlobalAddr(ret)
    }

    pub(crate) fn get_global(&self, global_addr: GlobalAddr) -> &Global {
        &self.globals[global_addr.0 as usize]
    }

    pub(crate) fn get_global_mut(&mut self, global_addr: GlobalAddr) -> &mut Global {
        &mut self.globals[global_addr.0 as usize]
    }

    pub(crate) fn allocate_data(&mut self, data: wasm::DataSegment) -> DataAddr {
        let ret = self.datas.len() as u32;
        self.datas.push(data);
        DataAddr(ret)
    }

    pub(crate) fn get_data(&self, data_addr: DataAddr) -> &wasm::DataSegment {
        &self.datas[data_addr.0 as usize]
    }

    pub(crate) fn get_data_mut(&mut self, data_addr: DataAddr) -> &mut wasm::DataSegment {
        &mut self.datas[data_addr.0 as usize]
    }

    pub(crate) fn allocate_elem(&mut self, elem: wasm::ElementSegment) -> ElemAddr {
        let ret = self.elems.len() as u32;
        self.elems.push(elem);
        ElemAddr(ret)
    }

    pub(crate) fn get_elem(&self, elem_addr: ElemAddr) -> &wasm::ElementSegment {
        &self.elems[elem_addr.0 as usize]
    }

    pub(crate) fn get_elem_mut(&mut self, elem_addr: ElemAddr) -> &mut wasm::ElementSegment {
        &mut self.elems[elem_addr.0 as usize]
    }

    pub(crate) fn allocate_tag(&mut self, module_addr: ModuleAddr, ty_idx: TypeIdx) -> TagAddr {
        let ret = self.tags.len() as u32;
        self.tags.push(Tag {
            id: ret,
            module_addr,
            ty_idx,
        });
        TagAddr(ret)
    }

    pub(crate) fn get_tag(&self, tag_addr: TagAddr) -> &Tag {
        &self.tags[tag_addr.0 as usize]
    }

    pub(crate) fn allocate_exn(&mut self, exn: Exception) -> ExnAddr {
        let idx = self.exceptions.len() as u32;
        self.exceptions.push(exn);
        ExnAddr(idx)
    }

    pub(crate) fn get_exn(&self, exn_addr: ExnAddr) -> &Exception {
        &self.exceptions[exn_addr.0 as usize]
    }

    pub(crate) fn allocate_array(
        &mut self,
        module_addr: ModuleAddr,
        ty_idx: TypeIdx,
        elems: Vec<Value>,
    ) -> ArrayAddr {
        let idx = self.arrays.len() as u32;
        self.arrays.push(Array {
            module_addr,
            ty_idx,
            elems,
        });
        ArrayAddr(idx)
    }

    pub(crate) fn get_array(&self, array_addr: ArrayAddr) -> &Array {
        &self.arrays[array_addr.0 as usize]
    }

    pub(crate) fn get_array_mut(&mut self, array_addr: ArrayAddr) -> &mut Array {
        &mut self.arrays[array_addr.0 as usize]
    }

    pub(crate) fn allocate_struct(
        &mut self,
        module_addr: ModuleAddr,
        ty_idx: TypeIdx,
        fields: Vec<Value>,
    ) -> StructAddr {
        let idx = self.structs.len() as u32;
        self.structs.push(Struct {
            module_addr,
            ty_idx,
            fields,
        });
        StructAddr(idx)
    }

    pub(crate) fn get_struct(&self, struct_addr: StructAddr) -> &Struct {
        &self.structs[struct_addr.0 as usize]
    }

    pub(crate) fn get_struct_mut(&mut self, struct_addr: StructAddr) -> &mut Struct {
        &mut self.structs[struct_addr.0 as usize]
    }

    pub(crate) fn externalize(&mut self, ref_: Ref) -> ExternAddr {
        let addr = self.externalized_refs.len() as u32;
        self.externalized_refs.push(ref_);
        ExternAddr(addr)
    }

    #[allow(unused)]
    pub(crate) fn internalize(&self, extern_addr: ExternAddr) -> Ref {
        self.externalized_refs[extern_addr.0 as usize]
    }
}

#[derive(Debug)]
pub(crate) struct Global {
    pub(crate) value: Value,

    #[allow(unused)]
    pub(crate) mutable: bool, // Only needed for validation
}

#[derive(Debug)]
pub(crate) struct Tag {
    /// Unique id of the tag.
    #[allow(unused)]
    pub(crate) id: u32,

    /// Address of the module that defines the tag type.
    pub(crate) module_addr: ModuleAddr,

    /// Index of the exception's type in its module.
    ///
    /// The type at this index will be a function type.
    pub(crate) ty_idx: TypeIdx,
}

impl Tag {
    pub(crate) fn rtt(&self) -> (ModuleAddr, wasm::HeapType) {
        (self.module_addr, wasm::HeapType::TypeIdx(self.ty_idx.0))
    }
}
