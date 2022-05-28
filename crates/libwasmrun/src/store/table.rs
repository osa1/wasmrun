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

    pub fn set(&mut self, idx: usize, elem: Ref) {
        debug_assert_eq!(self.ty.elem_type(), elem.ty());
        self.elems[idx] = elem;
    }
}
