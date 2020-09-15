use super::value::Value;

use fxhash::FxHashMap;
use parity_wasm::elements as wasm;

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

pub fn gen_block_bounds(instrs: &[wasm::Instruction]) -> FxHashMap<u32, u32> {
    todo!()
}

#[derive(Debug)]
pub struct Global {
    pub value: Value,
    pub mutable: bool, // Only needed for validation
}
