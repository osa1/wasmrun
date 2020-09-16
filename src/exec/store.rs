use super::value::Value;

use fxhash::FxHashMap;
use parity_wasm::elements as wasm;
use wasm::Instruction;

pub type ModuleIdx = usize;

#[derive(Default, Debug)]
pub struct Store {
    pub funcs: Vec<Func>,
    pub tables: Vec<Vec<Option<u32>>>, // indexed by table address (table_addrs), returns function address (index into Store.funcs)
    pub mems: Vec<Vec<u8>>,            // indexed by module idx
    pub globals: Vec<Global>,
}

#[derive(Debug)]
pub struct Func {
    pub module_idx: ModuleIdx,
    pub fun_idx: usize,
    pub fun: wasm::FuncBody,
    pub fun_ty_idx: u32,
    /// Maps block instruction indices to their `break` targets. For a `block` or `if` the target
    /// is the instruction after the block. For `loop` it's the first instruction of the block.
    pub block_bounds: FxHashMap<u32, u32>,
}

impl Func {
    pub fn new(
        module_idx: ModuleIdx,
        fun_idx: usize,
        fun: wasm::FuncBody,
        fun_ty_idx: u32,
    ) -> Func {
        let block_bounds = gen_block_bounds(fun.code().elements());
        Func {
            module_idx,
            fun_idx,
            fun,
            fun_ty_idx,
            block_bounds,
        }
    }
}

enum BlockKind {
    BlockOrIf,
    Loop,
}

fn gen_block_bounds(instrs: &[wasm::Instruction]) -> FxHashMap<u32, u32> {
    let mut ret: FxHashMap<u32, u32> = Default::default();
    let mut blocks: Vec<(BlockKind, u32)> = vec![];

    for (instr_idx, instr) in instrs.iter().enumerate() {
        match instr {
            Instruction::Block(_) | Instruction::If(_) => {
                blocks.push((BlockKind::BlockOrIf, instr_idx as u32));
            }

            Instruction::Loop(_) => {
                blocks.push((BlockKind::Loop, instr_idx as u32));
            }

            Instruction::End => {
                match blocks.pop() {
                    None => {
                        // Must be the end of the function
                        assert_eq!(instr_idx + 1, instrs.len());
                    }
                    Some((block_kind, start_idx)) => match block_kind {
                        BlockKind::BlockOrIf => {
                            ret.insert(start_idx, instr_idx as u32 + 1);
                        }
                        BlockKind::Loop => {
                            ret.insert(start_idx, start_idx + 1);
                        }
                    },
                }
            }

            _ => {}
        }
    }

    ret
}

#[derive(Debug)]
pub struct Global {
    pub value: Value,
    pub mutable: bool, // Only needed for validation
}
