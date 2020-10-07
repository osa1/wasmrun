use crate::exec::Runtime;
use crate::module::TypeIdx;
use crate::store::{FunAddr, MemAddr, ModuleAddr};
use crate::value::Value;
use crate::{ExecError, Result};

use std::fmt;
use std::rc::Rc;

use fxhash::FxHashMap;
use parity_wasm::elements as wasm;
use parity_wasm::elements::Instruction;

pub(crate) enum Fun {
    Wasm(WasmFun),
    Host(HostFun),
    WASI(WASIFun),
}

impl fmt::Debug for Fun {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

pub(crate) struct HostFun {
    /// Address of the function's module
    pub(crate) module_addr: ModuleAddr,
    /// Index of the function's type in its module
    pub(crate) ty_idx: TypeIdx,
    /// Address of the function in the heap
    pub(crate) fun_addr: FunAddr,
    /// Function code
    pub(crate) fun: Rc<dyn Fn(&mut Runtime) -> Result<Vec<Value>>>,
}

#[derive(Debug)]
pub(crate) struct WasmFun {
    /// Addrss of the function's module
    pub(crate) module_addr: ModuleAddr,
    /// Type index of the function in its module
    pub(crate) ty_idx: TypeIdx,
    /// Address of the function in the heap
    pub(crate) fun_addr: FunAddr,
    /// Function code
    pub(crate) fun: wasm::FuncBody,
    /// Maps `block` and `if instructions to their `end` instructions
    pub(crate) block_to_end: FxHashMap<u32, u32>,
    /// Maps if instructions to their else instructions
    pub(crate) if_to_else: FxHashMap<u32, u32>,
}

pub(crate) struct WASIFun {
    /// Address of the WASI module. NB. I think This is not used. (TODO: maybe remove and panic
    /// when this is needed?)
    pub(crate) module_addr: ModuleAddr,
    /// Type index of the function in its module
    pub(crate) ty_idx: TypeIdx,
    /// Address of the function in the heap
    pub(crate) fun_addr: FunAddr,
    /// The function. WASI functions don't use multi-value returns yet so the return value is just
    /// a single `Value`.
    pub(crate) fun: &'static dyn Fn(&mut Runtime, MemAddr) -> Result<Value>,
}

impl Fun {
    pub(crate) fn new(
        module_addr: ModuleAddr,
        ty_idx: TypeIdx,
        fun_addr: FunAddr,
        fun: wasm::FuncBody,
    ) -> Result<Fun> {
        let (block_to_end, if_to_else) = gen_block_bounds(fun.code().elements())?;
        Ok(Fun::Wasm(WasmFun {
            module_addr,
            ty_idx,
            fun_addr,
            fun,
            block_to_end,
            if_to_else,
        }))
    }

    pub(crate) fn ty_idx(&self) -> TypeIdx {
        match self {
            Fun::Wasm(fun) => fun.ty_idx,
            Fun::Host(fun) => fun.ty_idx,
            Fun::WASI(fun) => fun.ty_idx,
        }
    }

    pub(crate) fn module_addr(&self) -> ModuleAddr {
        match self {
            Fun::Wasm(fun) => fun.module_addr,
            Fun::Host(fun) => fun.module_addr,
            Fun::WASI(fun) => fun.module_addr,
        }
    }

    pub(crate) fn fun_addr(&self) -> FunAddr {
        match self {
            Fun::Wasm(fun) => fun.fun_addr,
            Fun::Host(fun) => fun.fun_addr,
            Fun::WASI(fun) => fun.fun_addr,
        }
    }

    pub(crate) fn locals(&self) -> &[wasm::Local] {
        match self {
            Fun::Wasm(fun) => fun.fun.locals(),
            Fun::Host(_) | Fun::WASI(_) => &[],
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
