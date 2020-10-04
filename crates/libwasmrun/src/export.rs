use crate::module::{FunIdx, GlobalIdx, MemIdx, TableIdx};

#[derive(Debug)]
pub(crate) struct Export {
    kind: ExportKind,
    field: String,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ExportKind {
    Fun(FunIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}

impl Export {
    pub(crate) fn new_fun(field: String, fun_idx: FunIdx) -> Self {
        Export {
            kind: ExportKind::Fun(fun_idx),
            field,
        }
    }

    pub(crate) fn new_table(field: String, table_idx: TableIdx) -> Self {
        Export {
            kind: ExportKind::Table(table_idx),
            field,
        }
    }

    pub(crate) fn new_mem(field: String, mem_idx: MemIdx) -> Self {
        Export {
            kind: ExportKind::Mem(mem_idx),
            field,
        }
    }

    pub(crate) fn new_global(field: String, global_idx: GlobalIdx) -> Self {
        Export {
            kind: ExportKind::Global(global_idx),
            field,
        }
    }

    pub(crate) fn kind(&self) -> ExportKind {
        self.kind
    }

    pub(crate) fn field(&self) -> &str {
        &self.field
    }
}
