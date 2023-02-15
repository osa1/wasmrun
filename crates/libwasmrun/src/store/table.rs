use crate::exec::Trap;
use crate::value::Ref;
use crate::{ExecError, Result};

use libwasmrun_syntax::TableType;

#[derive(Debug)]
pub struct Table {
    ty: TableType,
    // Invariant: Type of `Ref`s is the same as `type.ref_type`
    elems: Vec<Ref>,
}

impl Table {
    pub fn new(elem: Ref, ty: TableType) -> Table {
        Table {
            ty,
            elems: vec![elem; ty.limits().initial() as usize],
        }
    }

    pub fn resize(&mut self, new_len: usize, elem: Ref) {
        self.elems.resize(new_len, elem);
    }

    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn get(&self, idx: usize) -> Option<&Ref> {
        self.elems.get(idx)
    }

    pub fn set(&mut self, idx: usize, elem: Ref) -> Result<()> {
        debug_assert_eq!(self.ty.elem_type().heap_ty, elem.heap_ty());
        match self.elems.get_mut(idx) {
            Some(elem_ref) => {
                *elem_ref = elem;
                Ok(())
            }
            None => Err(ExecError::Trap(Trap::OOBTableAccess)),
        }
    }

    /// Returns old size of the table
    pub fn grow(&mut self, amt: usize, elem: Ref) -> Option<usize> {
        debug_assert_eq!(self.ty.elem_type().heap_ty, elem.heap_ty());
        let old_size = self.elems.len();
        let new_size = match old_size.checked_add(amt) {
            Some(new_size) => new_size,
            None => return None,
        };
        if let Some(max_size) = self.ty.limits().maximum() {
            if new_size > max_size as usize {
                return None;
            }
        }

        self.elems.resize(new_size, elem);
        Some(old_size)
    }

    /// Returns whether `idx + amt` is in range and fill is successful
    pub fn fill(&mut self, idx: usize, amt: usize, elem: Ref) -> bool {
        debug_assert_eq!(self.ty.elem_type().heap_ty, elem.heap_ty());

        if amt + idx > self.len() {
            return false;
        }

        for i in idx..idx + amt {
            self.elems[i] = elem;
        }

        true
    }
}
