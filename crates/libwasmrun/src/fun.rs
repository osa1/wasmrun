use crate::collections::Map;
use crate::exec::Runtime;
use crate::module::TypeIdx;
use crate::store::{FunAddr, MemAddr, ModuleAddr};
use crate::value::Value;
use crate::{ExecError, Result};

use std::fmt;
use std::rc::Rc;

use libwasmrun_syntax::Instruction;
use libwasmrun_syntax::{self as wasm, IndexMap};

pub enum Fun {
    Wasm(WasmFun),
    Host(HostFun),
}

impl fmt::Debug for Fun {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

pub struct HostFun {
    /// Address of the function's module
    pub(crate) module_addr: ModuleAddr,

    /// Index of the function's type in its module
    pub(crate) ty_idx: TypeIdx,

    /// Address of the function in the heap
    pub(crate) fun_addr: FunAddr,

    /// Function code
    pub(crate) fun: Rc<dyn Fn(&mut Runtime, Option<MemAddr>) -> Result<Vec<Value>>>,
}

#[derive(Debug)]
pub struct WasmFun {
    /// Address of the function's module
    pub(crate) module_addr: ModuleAddr,

    /// Type index of the function in its module
    pub(crate) ty_idx: TypeIdx,

    /// Address of the function in the heap
    pub(crate) fun_addr: FunAddr,

    /// Function code
    pub(crate) fun: wasm::FuncBody,

    /// Function name as specified in the name section
    pub(crate) name: Option<String>,

    /// Names of locals as specified in the name section
    #[allow(unused)]
    pub(crate) local_names: Option<IndexMap<String>>,

    /// Maps `block` and `if` instructions to their `end` instructions
    pub(crate) block_to_end: Map<u32, u32>,

    /// Maps `if` instructions to their else instructions
    pub(crate) if_to_else: Map<u32, u32>,

    /// Maps `try` instructions to their `catch`, `catch_all`, `delegate`, and `end`.
    ///
    /// The syntax will be validated: last one in the list will be `delegate` or `end`. `catch_all`
    /// can appear at most once, and after `catch` blocks.
    #[allow(unused)]
    pub(crate) try_to_catch: Map<u32, Vec<u32>>,
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
        let (block_to_end, if_to_else, try_to_catch) = gen_block_bounds(fun.code().elements())?;
        Ok(Fun::Wasm(WasmFun {
            module_addr,
            ty_idx,
            fun_addr,
            fun,
            name,
            local_names,
            block_to_end,
            if_to_else,
            try_to_catch,
        }))
    }

    pub(crate) fn ty_idx(&self) -> TypeIdx {
        match self {
            Fun::Wasm(fun) => fun.ty_idx,
            Fun::Host(fun) => fun.ty_idx,
        }
    }

    pub(crate) fn module_addr(&self) -> ModuleAddr {
        match self {
            Fun::Wasm(fun) => fun.module_addr,
            Fun::Host(fun) => fun.module_addr,
        }
    }

    pub(crate) fn fun_addr(&self) -> FunAddr {
        match self {
            Fun::Wasm(fun) => fun.fun_addr,
            Fun::Host(fun) => fun.fun_addr,
        }
    }

    pub(crate) fn locals(&self) -> &[wasm::Local] {
        match self {
            Fun::Wasm(fun) => fun.fun.locals(),
            Fun::Host(_) => &[],
        }
    }

    pub fn name(&self) -> Option<&String> {
        match self {
            Fun::Wasm(fun) => fun.name.as_ref(),
            Fun::Host(_) => None,
        }
    }
}

fn gen_block_bounds(
    instrs: &[wasm::Instruction],
) -> Result<(Map<u32, u32>, Map<u32, u32>, Map<u32, Vec<u32>>)> {
    enum BlockType {
        /// Starts with `block` or `loop`, ends with `end`
        BlockOrLoop,

        /// Starts with `if`, may contain an `else`, ends with `end`
        If,

        /// Starts with `try`, if contains `catch` or `catch_all` must end with `end`. Otherwise
        /// ends with `delegate`
        Try,
    }

    let mut block_to_end: Map<u32, u32> = Default::default();
    let mut if_to_else: Map<u32, u32> = Default::default();
    let mut try_to_catch: Map<u32, Vec<u32>> = Default::default();

    let mut blocks: Vec<(BlockType, u32)> = vec![];

    for (instr_idx, instr) in instrs.iter().enumerate() {
        let instr_idx = instr_idx as u32;

        // println!("gen_block_bounds instr={:?}, blocks={:?}", instr, blocks);
        match instr {
            Instruction::Block(_) | Instruction::Loop(_) => {
                blocks.push((BlockType::BlockOrLoop, instr_idx));
            }

            Instruction::If(_) => {
                blocks.push((BlockType::If, instr_idx));
            }

            Instruction::Try(_) => {
                blocks.push((BlockType::Try, instr_idx));
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
                    Some((BlockType::Try, start_idx)) => {
                        try_to_catch.entry(start_idx).or_default().push(instr_idx);
                    }
                    Some((BlockType::BlockOrLoop | BlockType::If, start_idx)) => {
                        block_to_end.insert(start_idx, instr_idx);
                    }
                }
            }

            Instruction::Catch(_) | Instruction::CatchAll => match blocks.last_mut() {
                Some((BlockType::Try, try_loc)) => {
                    try_to_catch.entry(*try_loc).or_default().push(instr_idx)
                }
                _ => exec_panic!("Found `catch`, `catch_all` outside `try`"),
            },

            Instruction::Delegate(_) => match blocks.pop() {
                Some((BlockType::Try, try_loc)) => {
                    try_to_catch.entry(try_loc).or_default().push(instr_idx)
                }
                _ => exec_panic!("Found `delegate` outside `try`"),
            },

            _ => {}
        }
    }

    // Validate try-catch-catch-all and try-delegate syntax
    for try_conts in try_to_catch.values() {
        let n_conts = try_conts.len();
        let mut catch_all_seen = false;
        for (cont_idx, cont) in try_conts.iter().enumerate() {
            match &instrs[(*cont) as usize] {
                Instruction::End => {
                    // This is an assertion and `end` should terminate the current `try` block when
                    // generating the map in the previous pass
                    assert_eq!(cont_idx, n_conts - 1);
                }

                Instruction::Catch(_) => {
                    if catch_all_seen {
                        exec_panic!("Invalid `try`: `catch` after `catch_all`");
                    }
                }

                Instruction::CatchAll => {
                    if catch_all_seen {
                        exec_panic!("Invalid `try`: multiple `catch_all` blocks");
                    }

                    catch_all_seen = true;
                }

                Instruction::Delegate(_) => {
                    if cont_idx != n_conts - 1 {
                        exec_panic!("Invalid `try`: more blocks after `delegate`");
                    }
                }

                other => {
                    exec_panic!("Unexpected instruction in try-to-catch map: {:?}", other);
                }
            }
        }
    }

    Ok((block_to_end, if_to_else, try_to_catch))
}
