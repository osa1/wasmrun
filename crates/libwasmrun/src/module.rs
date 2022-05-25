use crate::export::{Export, ExportKind};
use crate::store::{FunAddr, GlobalAddr, MemAddr, TableAddr};

use fxhash::FxHashMap;
use libwasmrun_syntax::elements as wasm;

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct TableIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct GlobalIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct MemIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct TypeIdx(pub u32);

#[derive(Debug, Default)]
pub(crate) struct Module {
    types: Vec<wasm::FunctionType>,
    func_addrs: Vec<FunAddr>,
    table_addrs: Vec<TableAddr>,
    mem_addrs: Vec<MemAddr>,
    global_addrs: Vec<GlobalAddr>,
    exports: Vec<Export>,
    start: Option<FunIdx>,
    name_to_fun: FxHashMap<String, FunIdx>,
}

impl Module {
    pub(crate) fn add_type(&mut self, ty: wasm::FunctionType) -> TypeIdx {
        let ret = self.types.len();
        self.types.push(ty);
        TypeIdx(ret as u32)
    }

    pub(crate) fn get_type(&self, ty_idx: TypeIdx) -> &wasm::FunctionType {
        &self.types[ty_idx.0 as usize]
    }

    pub(crate) fn add_fun(&mut self, fun_addr: FunAddr) -> FunIdx {
        let ret = self.func_addrs.len();
        self.func_addrs.push(fun_addr);
        FunIdx(ret as u32)
    }

    pub(crate) fn get_fun(&self, fun_idx: FunIdx) -> FunAddr {
        self.func_addrs[fun_idx.0 as usize]
    }

    pub(crate) fn add_table(&mut self, table_addr: TableAddr) -> TableIdx {
        let ret = self.table_addrs.len();
        self.table_addrs.push(table_addr);
        TableIdx(ret as u32)
    }

    pub(crate) fn get_table(&self, table_idx: TableIdx) -> TableAddr {
        self.table_addrs[table_idx.0 as usize]
    }

    pub(crate) fn add_mem(&mut self, mem_addr: MemAddr) -> MemIdx {
        let ret = self.mem_addrs.len();
        self.mem_addrs.push(mem_addr);
        MemIdx(ret as u32)
    }

    pub(crate) fn get_mem(&self, mem_idx: MemIdx) -> MemAddr {
        self.mem_addrs[mem_idx.0 as usize]
    }

    pub(crate) fn get_mem_opt(&self, mem_idx: MemIdx) -> Option<MemAddr> {
        self.mem_addrs.get(mem_idx.0 as usize).copied()
    }

    pub(crate) fn add_global(&mut self, global_addr: GlobalAddr) -> GlobalIdx {
        let ret = self.global_addrs.len();
        self.global_addrs.push(global_addr);
        GlobalIdx(ret as u32)
    }

    pub(crate) fn get_global(&self, global_idx: GlobalIdx) -> GlobalAddr {
        self.global_addrs[global_idx.0 as usize]
    }

    pub(crate) fn add_export(&mut self, export: Export) {
        self.exports.push(export);
    }

    pub(crate) fn get_exported_fun(&self, fun: &str) -> Option<FunAddr> {
        for export in &self.exports {
            if let ExportKind::Fun(fun_idx) = export.kind() {
                if export.field() == fun {
                    return Some(self.get_fun(fun_idx));
                }
            }
        }
        None
    }

    pub(crate) fn get_exported_table(&self, table: &str) -> Option<TableAddr> {
        for export in &self.exports {
            if let ExportKind::Table(table_idx) = export.kind() {
                if export.field() == table {
                    return Some(self.get_table(table_idx));
                }
            }
        }
        None
    }

    pub(crate) fn get_exported_mem(&self, mem: &str) -> Option<MemAddr> {
        for export in &self.exports {
            if let ExportKind::Mem(mem_idx) = export.kind() {
                if export.field() == mem {
                    return Some(self.get_mem(mem_idx));
                }
            }
        }
        None
    }

    pub(crate) fn get_exported_global(&self, global: &str) -> Option<GlobalAddr> {
        for export in &self.exports {
            if let ExportKind::Global(global_idx) = export.kind() {
                if export.field() == global {
                    return Some(self.get_global(global_idx));
                }
            }
        }
        None
    }

    pub(crate) fn set_start(&mut self, start: FunIdx) {
        self.start = Some(start);
    }

    pub(crate) fn get_start(&self) -> Option<FunIdx> {
        self.start
    }

    pub(crate) fn add_fun_name(&mut self, name: String, idx: FunIdx) {
        self.name_to_fun.insert(name, idx);
    }

    pub(crate) fn get_fun_name(&self, name: &str) -> Option<FunIdx> {
        self.name_to_fun.get(name).copied()
    }
}
