use crate::store::TagAddr;
use crate::value::Value;

#[derive(Debug, Default)]
pub struct Heap {
    exceptions: Vec<Exception>,
}

#[derive(Debug, Clone)]
pub struct Exception {
    /// Address of the exception in the store.
    pub addr: TagAddr,

    /// Arguments of the exception. Used by `throw` blocks.
    pub args: Vec<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExnAddr(pub u32); // an index into `Heap.exceptions`

impl Heap {
    pub fn allocate_exn(&mut self, exn: Exception) -> ExnAddr {
        let idx = self.exceptions.len() as u32;
        self.exceptions.push(exn);
        ExnAddr(idx)
    }

    pub fn get_exn(&self, exn_addr: ExnAddr) -> &Exception {
        &self.exceptions[exn_addr.0 as usize]
    }
}
