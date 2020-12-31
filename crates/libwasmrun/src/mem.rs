use crate::exec::{Trap, PAGE_SIZE};
use crate::{ExecError, Result};

use std::ops::{Index, IndexMut};

use wiggle::{BorrowHandle, GuestError, GuestMemory, Region};
use wiggle_borrow::BorrowChecker;

pub struct Mem {
    mem: Vec<u8>,
    limit: Option<u32>,
    bc: BorrowChecker,
}

impl Index<u32> for Mem {
    type Output = u8;

    fn index(&self, index: u32) -> &Self::Output {
        &self.mem[index as usize]
    }
}

impl IndexMut<u32> for Mem {
    fn index_mut(&mut self, index: u32) -> &mut Self::Output {
        &mut self.mem[index as usize]
    }
}

unsafe impl GuestMemory for Mem {
    fn base(&self) -> (*mut u8, u32) {
        (self.mem.as_ptr() as *mut _, self.mem.len() as u32)
    }

    fn has_outstanding_borrows(&self) -> bool {
        self.bc.has_outstanding_borrows()
    }

    fn is_shared_borrowed(&self, r: Region) -> bool {
        self.bc.is_shared_borrowed(r)
    }

    fn is_mut_borrowed(&self, r: Region) -> bool {
        self.bc.is_mut_borrowed(r)
    }

    fn shared_borrow(&self, r: Region) -> std::result::Result<BorrowHandle, GuestError> {
        self.bc.shared_borrow(r)
    }

    fn mut_borrow(&self, r: Region) -> std::result::Result<BorrowHandle, GuestError> {
        self.bc.mut_borrow(r)
    }

    fn shared_unborrow(&self, h: BorrowHandle) {
        self.bc.shared_unborrow(h)
    }

    fn mut_unborrow(&self, h: BorrowHandle) {
        self.bc.mut_unborrow(h)
    }
}

impl Mem {
    /// `initial`: Initial number of pages
    /// `limit`: Max num. of pages
    pub(crate) fn new(initial: u32, limit: Option<u32>) -> Self {
        Mem {
            mem: vec![0; initial as usize * PAGE_SIZE],
            limit,
            bc: BorrowChecker::new(),
        }
    }

    pub fn get_len(&self, offset: u32, len: u32) -> Result<&[u8]> {
        self.get_range_exclusive(offset, offset + len)
    }

    pub fn get_len_mut(&mut self, offset: u32, len: u32) -> Result<&mut [u8]> {
        self.get_range_exclusive_mut(offset, offset + len)
    }

    pub fn get_range_exclusive(&self, range_begin: u32, range_end_exclusive: u32) -> Result<&[u8]> {
        self.check_range(range_begin, range_end_exclusive - range_begin)?;
        Ok(&self.mem[range_begin as usize..range_end_exclusive as usize])
    }

    pub fn get_range_exclusive_mut(
        &mut self,
        range_begin: u32,
        range_end_exclusive: u32,
    ) -> Result<&mut [u8]> {
        self.check_range(range_begin, range_end_exclusive - range_begin)?;
        Ok(&mut self.mem[range_begin as usize..range_end_exclusive as usize])
    }

    pub fn set_range(&mut self, offset: u32, value: &[u8]) -> Result<()> {
        if value.is_empty() {
            return Ok(());
        }
        self.check_range(offset, value.len() as u32)?;
        let offset = offset as usize;
        (&mut self.mem[offset..offset + value.len()]).copy_from_slice(value);
        Ok(())
    }

    pub(crate) fn max_pages(&self) -> Option<u32> {
        self.limit
    }

    pub(crate) fn size_pages(&self) -> u32 {
        debug_assert_eq!(self.mem.len() % PAGE_SIZE, 0);
        (self.mem.len() / PAGE_SIZE) as u32
    }

    pub(crate) fn add_pages(&mut self, n: u32) {
        self.mem
            .resize((self.size_pages() + n) as usize * PAGE_SIZE, 0);
    }

    pub(crate) fn check_range(&self, addr: u32, len: u32) -> Result<()> {
        if addr
            .checked_add(len)
            .ok_or(ExecError::Trap(Trap::OOBMemoryAccess))? as usize
            > self.mem.len()
        {
            Err(ExecError::Trap(Trap::OOBMemoryAccess))
        } else {
            Ok(())
        }
    }

    /*
        pub(crate) fn store_8(&mut self, addr: u32, value: u8) -> Result<()> {
            self.check_range(addr, 1)?;
            self[addr] = value;
            Ok(())
        }
    */

    pub fn store_32(&mut self, addr: u32, value: u32) -> Result<()> {
        self.check_range(addr, 4)?;

        let [b1, b2, b3, b4] = value.to_le_bytes();
        self[addr] = b1;
        self[addr + 1] = b2;
        self[addr + 2] = b3;
        self[addr + 3] = b4;

        Ok(())
    }

    pub fn load_32(&self, addr: u32) -> Result<u32> {
        self.check_range(addr, 4)?;

        let b1 = self[addr];
        let b2 = self[addr + 1];
        let b3 = self[addr + 2];
        let b4 = self[addr + 3];

        Ok(u32::from_le_bytes([b1, b2, b3, b4]))
    }

    pub fn store_64(&mut self, addr: u32, value: u64) -> Result<()> {
        self.check_range(addr, 8)?;

        let [b1, b2, b3, b4, b5, b6, b7, b8] = value.to_le_bytes();
        self[addr] = b1;
        self[addr + 1] = b2;
        self[addr + 2] = b3;
        self[addr + 3] = b4;
        self[addr + 4] = b5;
        self[addr + 5] = b6;
        self[addr + 6] = b7;
        self[addr + 7] = b8;

        Ok(())
    }
}
