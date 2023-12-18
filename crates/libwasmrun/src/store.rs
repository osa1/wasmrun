mod table;

use crate::exec::Runtime;
use crate::fun::{Fun, HostFun};
use crate::mem::Mem;
use crate::module::{Module, TypeIdx};
use crate::value::Value;
use crate::Result;
pub use table::Table;

use libwasmrun_syntax::{self as wasm, FunctionType, IndexMap};

use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleAddr(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunAddr(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExnAddr(pub u32);

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
pub(crate) struct TagAddr(u32);

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
        self.funs.push(Fun::Host(HostFun {
            module_addr,
            ty_idx,
            fun_addr,
            fun,
        }));
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

    pub(crate) fn allocate_tag(&mut self, tag_ty: wasm::FunctionType) -> TagAddr {
        let ret = self.tags.len() as u32;
        self.tags.push(Tag {
            id: ret,
            ty: tag_ty,
        });
        TagAddr(ret)
    }

    pub(crate) fn get_tag(&self, tag_addr: TagAddr) -> &Tag {
        &self.tags[tag_addr.0 as usize]
    }
}

#[derive(Debug)]
pub(crate) struct Global {
    pub(crate) value: Value,
    #[allow(unused)]
    pub(crate) mutable: bool, // Only needed for validation
}

// NB. Currently there is one type of tag which is for exceptions
#[derive(Debug)]
pub(crate) struct Tag {
    /// Unique id of the tag
    #[allow(unused)]
    pub(crate) id: u32,

    /// Type of the exception tag
    #[allow(unused)]
    pub(crate) ty: FunctionType,
}
