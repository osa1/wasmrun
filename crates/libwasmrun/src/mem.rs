use crate::exec::PAGE_SIZE;
use crate::{ExecError, Result};

use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct Mem {
    pub mem: Vec<u8>,
    pub limit: Option<u32>,
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

impl Mem {
    /// `initial`: Initial number of pages
    /// `limit`: Max num. of pages
    pub fn new(initial: u32, limit: Option<u32>) -> Self {
        Mem {
            mem: vec![0; initial as usize * PAGE_SIZE],
            limit,
        }
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

    pub fn max_pages(&self) -> Option<u32> {
        self.limit
    }

    pub fn size_pages(&self) -> u32 {
        debug_assert_eq!(self.mem.len() % PAGE_SIZE, 0);
        (self.mem.len() / PAGE_SIZE) as u32
    }

    pub fn add_pages(&mut self, n: u32) {
        self.mem
            .resize((self.size_pages() + n) as usize * PAGE_SIZE, 0);
    }

    pub fn check_range(&self, addr: u32, len: u32) -> Result<()> {
        if addr.checked_add(len).ok_or(ExecError::Trap)? as usize > self.mem.len() {
            Err(ExecError::Trap)
        } else {
            Ok(())
        }
    }
}
