use crate::exec::{Trap, PAGE_SIZE};
use crate::{ExecError, Result};

use std::ops::{Index, IndexMut};

use wiggle::{BorrowHandle, GuestError, GuestMemory, Region};
use wiggle_borrow::BorrowChecker;

pub(crate) struct Mem {
    pub mem: Vec<u8>,
    limit: Option<u32>,
    bc: BorrowChecker,
}

impl std::fmt::Debug for Mem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Mem")
            .field("mem", &self.mem)
            .field("limit", &self.limit)
            .finish()
    }
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

    pub(crate) fn len(&self) -> usize {
        self.mem.len()
    }

    pub(crate) fn set_range(&mut self, offset: u32, value: &[u8]) -> Result<()> {
        if value.is_empty() {
            return Ok(());
        }
        self.check_range(offset, value.len() as u32)?;
        let offset = offset as usize;
        self.mem[offset..offset + value.len()].copy_from_slice(value);
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

    fn check_range(&self, addr: u32, len: u32) -> Result<()> {
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

    pub(crate) fn load_8(&self, addr: u32) -> Result<u8> {
        self.check_range(addr, 1)?;
        Ok(self[addr])
    }

    pub(crate) fn store_8(&mut self, addr: u32, value: u8) -> Result<()> {
        self.check_range(addr, 1)?;
        self[addr] = value;
        Ok(())
    }

    pub(crate) fn load_16_le(&self, addr: u32) -> Result<u16> {
        self.check_range(addr, 2)?;
        Ok(load_16_le_unchecked(&self.mem, addr as usize))
    }

    pub(crate) fn store_16_le(&mut self, addr: u32, value: u16) -> Result<()> {
        self.check_range(addr, 2)?;
        store_16_le_unchecked(value, &mut self.mem, addr as usize);
        Ok(())
    }

    pub(crate) fn load_32_le(&self, addr: u32) -> Result<u32> {
        self.check_range(addr, 4)?;
        Ok(load_32_le_unchecked(&self.mem, addr as usize))
    }

    pub(crate) fn store_32_le(&mut self, addr: u32, value: u32) -> Result<()> {
        self.check_range(addr, 4)?;
        store_32_le_unchecked(value, &mut self.mem, addr as usize);
        Ok(())
    }

    pub(crate) fn load_64_le(&self, addr: u32) -> Result<u64> {
        self.check_range(addr, 8)?;
        Ok(load_64_le_unchecked(&self.mem, addr as usize))
    }

    pub(crate) fn store_64_le(&mut self, addr: u32, value: u64) -> Result<()> {
        self.check_range(addr, 8)?;
        store_64_le_unchecked(value, &mut self.mem, addr as usize);
        Ok(())
    }

    pub(crate) fn load_128_le(&self, addr: u32) -> Result<u128> {
        self.check_range(addr, 16)?;
        Ok(load_128_le_unchecked(&self.mem, addr as usize))
    }

    pub(crate) fn store_128_le(&mut self, addr: u32, value: u128) -> Result<()> {
        self.check_range(addr, 16)?;
        store_128_le_unchecked(value, &mut self.mem, addr as usize);
        Ok(())
    }
}

pub(crate) fn store_16_le_unchecked(value: u16, mem: &mut [u8], addr: usize) {
    let [b1, b2] = value.to_le_bytes();
    mem[addr] = b1;
    mem[addr + 1] = b2;
}

pub(crate) fn load_16_le_unchecked(mem: &[u8], addr: usize) -> u16 {
    let b1 = mem[addr];
    let b2 = mem[addr + 1];

    u16::from_le_bytes([b1, b2])
}

pub(crate) fn store_32_le_unchecked(value: u32, mem: &mut [u8], addr: usize) {
    let [b1, b2, b3, b4] = value.to_le_bytes();
    mem[addr] = b1;
    mem[addr + 1] = b2;
    mem[addr + 2] = b3;
    mem[addr + 3] = b4;
}

pub(crate) fn load_32_le_unchecked(mem: &[u8], addr: usize) -> u32 {
    let b1 = mem[addr];
    let b2 = mem[addr + 1];
    let b3 = mem[addr + 2];
    let b4 = mem[addr + 3];

    u32::from_le_bytes([b1, b2, b3, b4])
}

pub(crate) fn store_64_le_unchecked(value: u64, mem: &mut [u8], addr: usize) {
    let [b1, b2, b3, b4, b5, b6, b7, b8] = value.to_le_bytes();
    mem[addr] = b1;
    mem[addr + 1] = b2;
    mem[addr + 2] = b3;
    mem[addr + 3] = b4;
    mem[addr + 4] = b5;
    mem[addr + 5] = b6;
    mem[addr + 6] = b7;
    mem[addr + 7] = b8;
}

pub(crate) fn load_64_le_unchecked(mem: &[u8], addr: usize) -> u64 {
    let b1 = mem[addr];
    let b2 = mem[addr + 1];
    let b3 = mem[addr + 2];
    let b4 = mem[addr + 3];
    let b5 = mem[addr + 4];
    let b6 = mem[addr + 5];
    let b7 = mem[addr + 6];
    let b8 = mem[addr + 7];

    u64::from_le_bytes([b1, b2, b3, b4, b5, b6, b7, b8])
}

pub(crate) fn store_128_le_unchecked(value: u128, mem: &mut [u8], addr: usize) {
    let [b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16] =
        value.to_le_bytes();
    mem[addr] = b1;
    mem[addr + 1] = b2;
    mem[addr + 2] = b3;
    mem[addr + 3] = b4;
    mem[addr + 4] = b5;
    mem[addr + 5] = b6;
    mem[addr + 6] = b7;
    mem[addr + 7] = b8;
    mem[addr + 8] = b9;
    mem[addr + 9] = b10;
    mem[addr + 10] = b11;
    mem[addr + 11] = b12;
    mem[addr + 12] = b13;
    mem[addr + 13] = b14;
    mem[addr + 14] = b15;
    mem[addr + 15] = b16;
}

pub(crate) fn load_128_le_unchecked(mem: &[u8], addr: usize) -> u128 {
    let b1 = mem[addr];
    let b2 = mem[addr + 1];
    let b3 = mem[addr + 2];
    let b4 = mem[addr + 3];
    let b5 = mem[addr + 4];
    let b6 = mem[addr + 5];
    let b7 = mem[addr + 6];
    let b8 = mem[addr + 7];
    let b9 = mem[addr + 8];
    let b10 = mem[addr + 9];
    let b11 = mem[addr + 10];
    let b12 = mem[addr + 11];
    let b13 = mem[addr + 12];
    let b14 = mem[addr + 13];
    let b15 = mem[addr + 14];
    let b16 = mem[addr + 15];

    u128::from_le_bytes([
        b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16,
    ])
}
