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
    /// Maps if instructions to their else instructions
    pub else_instrs: FxHashMap<u32, u32>,
}

impl Func {
    pub fn new(
        module_idx: ModuleIdx,
        fun_idx: usize,
        fun: wasm::FuncBody,
        fun_ty_idx: u32,
    ) -> Func {
        let (block_bounds, else_instrs) = gen_block_bounds(fun.code().elements());
        Func {
            module_idx,
            fun_idx,
            fun,
            fun_ty_idx,
            block_bounds,
            else_instrs,
        }
    }
}

enum BlockKind {
    Block,
    If { else_loc: Option<u32> },
    Loop,
}

fn gen_block_bounds(instrs: &[wasm::Instruction]) -> (FxHashMap<u32, u32>, FxHashMap<u32, u32>) {
    let mut block_bounds: FxHashMap<u32, u32> = Default::default();
    let mut else_instrs: FxHashMap<u32, u32> = Default::default();
    let mut blocks: Vec<(BlockKind, u32)> = vec![];

    for (instr_idx, instr) in instrs.iter().enumerate() {
        match instr {
            Instruction::Block(_) => {
                blocks.push((BlockKind::Block, instr_idx as u32));
            }

            Instruction::If(_) => {
                blocks.push((BlockKind::If { else_loc: None }, instr_idx as u32));
            }

            Instruction::Loop(_) => {
                blocks.push((BlockKind::Loop, instr_idx as u32));
            }

            Instruction::Else => match blocks.last_mut() {
                Some((BlockKind::If { else_loc }, if_loc)) => {
                    assert!(else_loc.is_none());
                    *else_loc = Some(instr_idx as u32);
                    else_instrs.insert(*if_loc, instr_idx as u32);
                }
                None | Some((_, _)) => {
                    panic!("Found else block without if");
                }
            },

            Instruction::End => {
                match blocks.pop() {
                    None => {
                        // Must be the end of the function
                        assert_eq!(instr_idx + 1, instrs.len());
                    }
                    Some((block_kind, start_idx)) => match block_kind {
                        BlockKind::Block => {
                            block_bounds.insert(start_idx, instr_idx as u32 + 1);
                        }
                        BlockKind::If { else_loc } => {
                            block_bounds.insert(start_idx, instr_idx as u32 + 1);
                            if let Some(else_loc) = else_loc {
                                block_bounds.insert(else_loc, instr_idx as u32 + 1);
                            }
                        }
                        BlockKind::Loop => {
                            block_bounds.insert(start_idx, start_idx + 1);
                        }
                    },
                }
            }

            _ => {}
        }
    }

    (block_bounds, else_instrs)
}

#[derive(Debug)]
pub struct Global {
    pub value: Value,
    pub mutable: bool, // Only needed for validation
}
