use crate::value::Ref;

use libwasmrun_syntax::elements::TableType;

#[derive(Debug)]
pub struct Table {
    ty: TableType,
    // Invariant: Type of `Ref`s is the same as `type.ref_type`
    elems: Vec<Ref>,
}

impl Table {
    pub fn new(elem: Ref, size: usize, ty: TableType) -> Table {
        Table {
            ty,
            elems: vec![elem; size],
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

    /// Returns whether `idx` is in range and set is successful
    pub fn set(&mut self, idx: usize, elem: Ref) -> bool {
        debug_assert_eq!(self.ty.elem_type(), elem.ty());
        self.elems
            .get_mut(idx)
            .map(|elem_ref| *elem_ref = elem)
            .is_some()
    }

    /// Returns old size of the table
    pub fn grow(&mut self, amt: usize, elem: Ref) -> usize {
        debug_assert_eq!(self.ty.elem_type(), elem.ty());
        let size = self.elems.len();
        self.elems.resize(size + amt, elem);
        size
    }

    /// Returns whether `idx + amt` is in range and fill is successful
    pub fn fill(&mut self, idx: usize, amt: usize, elem: Ref) -> bool {
        debug_assert_eq!(self.ty.elem_type(), elem.ty());

        if (amt + idx) as usize > self.len() {
            return false;
        }

        for i in idx..idx + amt {
            self.elems[i] = elem;
        }

        true
    }
}
