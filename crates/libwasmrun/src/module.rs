use crate::collections::Map;
use crate::export::{Export, ExportKind};
use crate::store::{DataAddr, ElemAddr, FunAddr, GlobalAddr, MemAddr, TableAddr, TagAddr};

use libwasmrun_syntax as wasm;

#[derive(Debug, Clone, Copy)]
pub struct FunIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct TableIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct GlobalIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct MemIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct TypeIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct DataIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct ElemIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct TagIdx(pub u32);

#[derive(Debug, Default)]
pub struct Module {
    types: Vec<wasm::SubType>,
    func_addrs: Vec<FunAddr>,
    table_addrs: Vec<TableAddr>,
    mem_addrs: Vec<MemAddr>,
    global_addrs: Vec<GlobalAddr>,
    exports: Vec<Export>,
    start: Option<FunIdx>,
    name_to_fun: Map<String, FunIdx>,
    datas: Vec<DataAddr>,
    elems: Vec<ElemAddr>,
    tag_addrs: Vec<TagAddr>,

    /// Maps type indices of the module to their global canonical indices.
    pub(crate) canonical_type_ids: Vec<u32>,
}

impl Module {
    pub(crate) fn add_type(&mut self, ty: wasm::SubType) -> TypeIdx {
        let ret = self.types.len();
        self.types.push(ty);
        TypeIdx(ret as u32)
    }

    pub(crate) fn get_type(&self, ty_idx: TypeIdx) -> &wasm::SubType {
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

    pub(crate) fn add_tag(&mut self, tag_addr: TagAddr) -> TagIdx {
        let ret = self.tag_addrs.len();
        self.tag_addrs.push(tag_addr);
        TagIdx(ret as u32)
    }

    pub(crate) fn get_tag(&self, tag_idx: TagIdx) -> TagAddr {
        self.tag_addrs[tag_idx.0 as usize]
    }

    pub(crate) fn add_export(&mut self, export: Export) {
        self.exports.push(export);
    }

    pub(crate) fn get_exported_fun_addr(&self, fun: &str) -> Option<FunAddr> {
        self.get_exported_fun_idx(fun).map(|idx| self.get_fun(idx))
    }

    pub fn get_exported_fun_idx(&self, fun: &str) -> Option<FunIdx> {
        for export in &self.exports {
            if let ExportKind::Fun(fun_idx) = export.kind() {
                if export.field() == fun {
                    return Some(fun_idx);
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

    pub(crate) fn get_exported_tag(&self, tag: &str) -> Option<TagAddr> {
        for export in &self.exports {
            if let ExportKind::Tag(tag_idx) = export.kind() {
                if export.field() == tag {
                    return Some(self.get_tag(tag_idx));
                }
            }
        }
        None
    }

    pub(crate) fn set_start(&mut self, start: FunIdx) {
        self.start = Some(start);
    }

    pub fn get_start(&self) -> Option<FunIdx> {
        self.start
    }

    pub(crate) fn add_fun_name(&mut self, name: String, idx: FunIdx) {
        self.name_to_fun.insert(name, idx);
    }

    pub(crate) fn get_fun_name(&self, name: &str) -> Option<FunIdx> {
        self.name_to_fun.get(name).copied()
    }

    pub(crate) fn add_data(&mut self, data: DataAddr) -> DataIdx {
        let ret = self.datas.len();
        self.datas.push(data);
        DataIdx(ret as u32)
    }

    pub(crate) fn get_data(&self, data_idx: DataIdx) -> DataAddr {
        self.datas[data_idx.0 as usize]
    }

    pub(crate) fn add_elem(&mut self, elem: ElemAddr) -> ElemIdx {
        let ret = self.elems.len();
        self.elems.push(elem);
        ElemIdx(ret as u32)
    }

    pub(crate) fn get_elem(&self, elem_idx: ElemIdx) -> ElemAddr {
        self.elems[elem_idx.0 as usize]
    }
}
