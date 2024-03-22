use crate::collections::Map;
use crate::exec::Runtime;
use crate::module::TypeIdx;
use crate::store::{FunAddr, MemAddr, ModuleAddr};
use crate::value::Value;
use crate::Result;

use std::fmt;
use std::rc::Rc;

use libwasmrun_syntax::Instruction;
use libwasmrun_syntax::{self as wasm, IndexMap};

pub struct Fun {
    /// Address of the function's module.
    pub(crate) module_addr: ModuleAddr,

    /// Index of the function's type in its module.
    pub(crate) ty_idx: TypeIdx,

    /// Address of the function in the heap.
    pub(crate) fun_addr: FunAddr,

    /// Wasm or host function.
    pub(crate) kind: FunKind,
}

pub(crate) enum FunKind {
    Wasm(WasmFun),
    Host(HostFun),
}

impl fmt::Debug for Fun {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

type HostFun = Rc<dyn Fn(&mut Runtime, Option<MemAddr>) -> Result<Vec<Value>>>;

#[derive(Debug)]
pub struct WasmFun {
    /// Function code.
    pub(crate) fun: wasm::FuncBody,

    /// Function name as specified in the name section.
    pub(crate) name: Option<String>,

    /// Names of locals as specified in the name section.
    #[allow(unused)]
    pub(crate) local_names: Option<IndexMap<String>>,

    /// Maps `block` and `if` instructions to their `end` instructions.
    pub(crate) block_to_end: Map<u32, u32>,

    /// Maps `if` instructions to their else instructions.
    pub(crate) if_to_else: Map<u32, u32>,
}

impl Fun {
    pub(crate) fn new(
        module_addr: ModuleAddr,
        ty_idx: TypeIdx,
        fun_addr: FunAddr,
        fun: wasm::FuncBody,
        name: Option<String>,
        local_names: Option<IndexMap<String>>,
    ) -> Result<Fun> {
        let (block_to_end, if_to_else) = gen_block_bounds(fun.code().elements())?;
        Ok(Fun {
            module_addr,
            ty_idx,
            fun_addr,
            kind: FunKind::Wasm(WasmFun {
                fun,
                name,
                local_names,
                block_to_end,
                if_to_else,
            }),
        })
    }

    pub(crate) fn ty_idx(&self) -> TypeIdx {
        self.ty_idx
    }

    pub(crate) fn module_addr(&self) -> ModuleAddr {
        self.module_addr
    }

    pub(crate) fn fun_addr(&self) -> FunAddr {
        self.fun_addr
    }

    pub(crate) fn locals(&self) -> &[wasm::Local] {
        match &self.kind {
            FunKind::Wasm(fun) => fun.fun.locals(),
            FunKind::Host(_) => &[],
        }
    }

    pub fn name(&self) -> Option<&String> {
        match &self.kind {
            FunKind::Wasm(fun) => fun.name.as_ref(),
            FunKind::Host(_) => None,
        }
    }

    pub(crate) fn rtt(&self) -> (ModuleAddr, wasm::HeapType) {
        (self.module_addr, wasm::HeapType::TypeIdx(self.ty_idx.0))
    }
}

fn gen_block_bounds(instrs: &[wasm::Instruction]) -> Result<(Map<u32, u32>, Map<u32, u32>)> {
    enum BlockType {
        /// Starts with `block`, `loop`, or `try_table`. Ends with `end`.
        Block,

        /// Starts with `if`, may contain an `else`, ends with `end`.
        If,
    }

    let mut block_to_end: Map<u32, u32> = Default::default();
    let mut if_to_else: Map<u32, u32> = Default::default();

    let mut blocks: Vec<(BlockType, u32)> = vec![];

    for (instr_idx, instr) in instrs.iter().enumerate() {
        let instr_idx = instr_idx as u32;

        // println!("gen_block_bounds instr={:?}, blocks={:?}", instr, blocks);
        match instr {
            Instruction::Block(_) | Instruction::Loop(_) | Instruction::TryTable(_, _) => {
                blocks.push((BlockType::Block, instr_idx));
            }

            Instruction::If(_) => {
                blocks.push((BlockType::If, instr_idx));
            }

            Instruction::Else => match blocks.last_mut() {
                Some((BlockType::If, if_loc)) => {
                    if_to_else.insert(*if_loc, instr_idx);
                }
                _ => exec_panic!("Found `else` block without `if`"),
            },

            Instruction::End => {
                match blocks.pop() {
                    None => {
                        // Must be the end of the function
                    }
                    Some((BlockType::Block | BlockType::If, start_idx)) => {
                        block_to_end.insert(start_idx, instr_idx);
                    }
                }
            }

            _ => {}
        }
    }

    Ok((block_to_end, if_to_else))
}
