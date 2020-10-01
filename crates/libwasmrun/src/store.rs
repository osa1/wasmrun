use crate::exec::Runtime;
use crate::mem::Mem;
use crate::value::Value;
use crate::{ExecError, Result};

use fxhash::FxHashMap;
use parity_wasm::elements as wasm;
use wasm::Instruction;

use std::fmt;
use std::rc::Rc;

pub type ModuleIdx = usize;

#[derive(Default, Debug)]
pub struct Store {
    pub funcs: Vec<Func>,
    pub tables: Vec<Vec<Option<u32>>>, // indexed by table address (table_addrs), returns function address (index into Store.funcs)
    pub mems: Vec<Mem>,                // indexed by module idx
    pub globals: Vec<Global>,
}

impl Store {
    pub fn allocate_fun(
        &mut self,
        module_idx: ModuleIdx,
        ty_idx: u32,
        fun_addr: u32,
        fun: wasm::FuncBody,
    ) -> Result<u32> {
        let ret = self.funcs.len() as u32;
        self.funcs
            .push(Func::new(module_idx, ty_idx, fun_addr, fun)?);
        Ok(ret)
    }

    pub fn allocate_host_fun(
        &mut self,
        module_idx: ModuleIdx,
        ty_idx: u32,
        fun: Rc<dyn Fn(&mut Runtime)>,
    ) -> u32 {
        let ret = self.funcs.len() as u32;
        self.funcs.push(Func::Host(HostFunc {
            module_idx,
            ty_idx,
            fun,
        }));
        ret
    }

    pub fn allocate_table(&mut self, table: Vec<Option<u32>>) -> u32 {
        let ret = self.tables.len() as u32;
        self.tables.push(table);
        ret
    }

    pub fn allocate_mem(&mut self, mem: Mem) -> u32 {
        let ret = self.mems.len() as u32;
        self.mems.push(mem);
        ret
    }

    pub fn allocate_global(&mut self, global: Global) -> u32 {
        let ret = self.globals.len() as u32;
        self.globals.push(global);
        ret
    }
}

#[derive(Debug)]
pub struct Global {
    pub value: Value,
    pub mutable: bool, // Only needed for validation
}

pub enum Func {
    Wasm(WasmFunc),
    Host(HostFunc),
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

pub struct HostFunc {
    /// Index of the function's module
    pub module_idx: ModuleIdx,
    /// Index of the function's type in its module
    pub ty_idx: u32,
    /// Function code
    pub fun: Rc<dyn Fn(&mut Runtime)>,
}

#[derive(Debug)]
pub struct WasmFunc {
    /// Index of the function's module
    pub module_idx: ModuleIdx,
    /// Type index of the function in its module
    pub ty_idx: u32,
    /// Address of the function in the heap
    pub fun_addr: u32,
    /// Function code
    pub fun: wasm::FuncBody,
    /// Maps `block` and `if instructions to their `end` instructions
    pub block_to_end: FxHashMap<u32, u32>,
    /// Maps if instructions to their else instructions
    pub if_to_else: FxHashMap<u32, u32>,
}

impl Func {
    pub fn new(
        module_idx: ModuleIdx,
        ty_idx: u32,
        fun_addr: u32,
        fun: wasm::FuncBody,
    ) -> Result<Func> {
        let (block_to_end, if_to_else) = gen_block_bounds(fun.code().elements())?;
        Ok(Func::Wasm(WasmFunc {
            module_idx,
            ty_idx,
            fun_addr,
            fun,
            block_to_end,
            if_to_else,
        }))
    }

    pub fn ty_idx(&self) -> u32 {
        match self {
            Func::Wasm(fun) => fun.ty_idx,
            Func::Host(fun) => fun.ty_idx,
        }
    }

    pub fn module_idx(&self) -> ModuleIdx {
        match self {
            Func::Wasm(fun) => fun.module_idx,
            Func::Host(fun) => fun.module_idx,
        }
    }
}

fn gen_block_bounds(
    instrs: &[wasm::Instruction],
) -> Result<(FxHashMap<u32, u32>, FxHashMap<u32, u32>)> {
    let mut block_to_end: FxHashMap<u32, u32> = Default::default();
    let mut if_to_else: FxHashMap<u32, u32> = Default::default();
    let mut blocks: Vec<u32> = vec![];

    for (instr_idx, instr) in instrs.iter().enumerate() {
        // println!("gen_block_bounds instr={:?}, blocks={:?}", instr, blocks);
        match instr {
            Instruction::Block(_) => {
                blocks.push(instr_idx as u32);
            }

            Instruction::If(_) => {
                blocks.push(instr_idx as u32);
            }

            Instruction::Loop(_) => {
                blocks.push(instr_idx as u32);
            }

            Instruction::Else => match blocks.last_mut() {
                Some(if_loc) => {
                    if_to_else.insert(*if_loc, instr_idx as u32);
                }
                None => {
                    return Err(ExecError::Panic("Found else block without if".to_string()));
                }
            },

            Instruction::End => {
                match blocks.pop() {
                    None => {
                        // Must be the end of the function or a `loop`
                    }
                    Some(start_idx) => {
                        block_to_end.insert(start_idx, instr_idx as u32);
                    }
                }
            }

            _ => {}
        }
    }

    Ok((block_to_end, if_to_else))
}
