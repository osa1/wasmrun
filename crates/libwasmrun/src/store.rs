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
    pub fn allocate_fun(&mut self, fun: WasmFunc) -> u32 {
        let ret = self.funcs.len() as u32;
        self.funcs.push(Func::Wasm(fun));
        ret
    }

    pub fn allocate_host_fun(&mut self, fun: Rc<dyn Fn(&mut Runtime)>) -> u32 {
        let ret = self.funcs.len() as u32;
        self.funcs.push(Func::Host(fun));
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
    Host(Rc<dyn Fn(&mut Runtime)>),
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

#[derive(Debug)]
pub struct WasmFunc {
    pub module_idx: ModuleIdx,
    pub fun_idx: usize,
    pub fun: wasm::FuncBody,
    pub fun_ty_idx: u32,

    /// Maps `block` and `if instructions to their `end` instructions
    pub block_to_end: FxHashMap<u32, u32>,

    /// Maps if instructions to their else instructions
    pub if_to_else: FxHashMap<u32, u32>,
}

impl Func {
    pub fn new(
        module_idx: ModuleIdx,
        fun_idx: usize,
        fun: wasm::FuncBody,
        fun_ty_idx: u32,
    ) -> Result<Func> {
        let (block_to_end, if_to_else) = gen_block_bounds(fun.code().elements())?;
        Ok(Func::Wasm(WasmFunc {
            module_idx,
            fun_idx,
            fun,
            fun_ty_idx,
            block_to_end,
            if_to_else,
        }))
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
