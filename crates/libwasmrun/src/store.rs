use crate::exec::Runtime;
use crate::fun::{Fun, HostFun};
use crate::mem::Mem;
use crate::module::{Module, TypeIdx};
use crate::value::Value;
use crate::Result;

use libwasmrun_syntax::elements::{self as wasm, IndexMap};

use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleAddr(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunAddr(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct TableAddr(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemAddr(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct GlobalAddr(u32);

#[derive(Default)]
pub struct Store {
    modules: Vec<Module>,
    funs: Vec<Fun>,
    tables: Vec<Vec<Option<FunAddr>>>,
    mems: Vec<Mem>,
    globals: Vec<Global>,
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

    pub(crate) fn allocate_table(&mut self, table: Vec<Option<FunAddr>>) -> TableAddr {
        let ret = self.tables.len() as u32;
        self.tables.push(table);
        TableAddr(ret)
    }

    pub(crate) fn get_table(&self, table_addr: TableAddr) -> &[Option<FunAddr>] {
        &self.tables[table_addr.0 as usize]
    }

    pub(crate) fn get_table_mut(&mut self, table_addr: TableAddr) -> &mut Vec<Option<FunAddr>> {
        &mut self.tables[table_addr.0 as usize]
    }

    pub(crate) fn allocate_mem(&mut self, mem: Mem) -> MemAddr {
        let ret = self.mems.len() as u32;
        self.mems.push(mem);
        MemAddr(ret)
    }

    pub fn get_mem(&self, mem_addr: MemAddr) -> &Mem {
        &self.mems[mem_addr.0 as usize]
    }

    pub fn get_mem_mut(&mut self, mem_addr: MemAddr) -> &mut Mem {
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
}

#[derive(Debug)]
pub(crate) struct Global {
    pub(crate) value: Value,
    pub(crate) mutable: bool, // Only needed for validation
}
