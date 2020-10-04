use crate::exec::Runtime;
use crate::mem::Mem;
use crate::module::{Module, TypeIdx};
use crate::value::Value;
use crate::{ExecError, Result};

use fxhash::FxHashMap;
use parity_wasm::elements as wasm;
use wasm::Instruction;

use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Copy)]
pub struct ModuleAddr(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct FunAddr(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct TableAddr(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct MemAddr(u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct GlobalAddr(u32);

#[derive(Default, Debug)]
pub(crate) struct Store {
    modules: Vec<Module>,
    funcs: Vec<Func>,
    tables: Vec<Vec<Option<FunAddr>>>, // indexed by table address (table_addrs), returns function address (index into Store.funcs)
    mems: Vec<Mem>,                    // indexed by module idx
    globals: Vec<Global>,
}

impl Store {
    pub(crate) fn allocate_module(&mut self, module: Module) -> ModuleAddr {
        let ret = self.next_module_addr();
        self.modules.push(module);
        ret
    }

    pub(crate) fn next_module_addr(&self) -> ModuleAddr {
        ModuleAddr(self.modules.len() as u32)
    }

    pub(crate) fn get_module(&self, module_addr: ModuleAddr) -> &Module {
        &self.modules[module_addr.0 as usize]
    }

    pub(crate) fn next_fun_addr(&self) -> FunAddr {
        FunAddr(self.funcs.len() as u32)
    }

    pub(crate) fn allocate_fun(
        &mut self,
        module_addr: ModuleAddr,
        ty_idx: TypeIdx,
        fun_addr: FunAddr,
        fun: wasm::FuncBody,
    ) -> Result<()> {
        self.funcs
            .push(Func::new(module_addr, ty_idx, fun_addr, fun)?);
        Ok(())
    }

    pub(crate) fn allocate_host_fun(
        &mut self,
        module_addr: ModuleAddr,
        ty_idx: TypeIdx,
        fun: Rc<dyn Fn(&mut Runtime)>,
    ) -> FunAddr {
        let ret = self.funcs.len();
        self.funcs.push(Func::Host(HostFunc {
            module_addr,
            ty_idx,
            fun,
        }));
        FunAddr(ret as u32)
    }

    pub(crate) fn get_fun(&self, fun_addr: FunAddr) -> &Func {
        &self.funcs[fun_addr.0 as usize]
    }

    pub(crate) fn allocate_table(&mut self, table: Vec<Option<FunAddr>>) -> TableAddr {
        let ret = self.tables.len() as u32;
        self.tables.push(table);
        TableAddr(ret)
    }

    pub(crate) fn get_table(&self, table_addr: TableAddr) -> &[Option<FunAddr>] {
        &self.tables[table_addr.0 as usize]
    }

    pub(crate) fn get_table_mut(&mut self, table_addr: TableAddr) -> &mut Vec<Option<FunAddr>> {
        &mut self.tables[table_addr.0 as usize]
    }

    pub(crate) fn allocate_mem(&mut self, mem: Mem) -> MemAddr {
        let ret = self.mems.len() as u32;
        self.mems.push(mem);
        MemAddr(ret)
    }

    pub(crate) fn get_mem(&self, mem_addr: MemAddr) -> &Mem {
        &self.mems[mem_addr.0 as usize]
    }

    pub(crate) fn get_mem_mut(&mut self, mem_addr: MemAddr) -> &mut Mem {
        &mut self.mems[mem_addr.0 as usize]
    }

    pub(crate) fn allocate_global(&mut self, global: Global) -> GlobalAddr {
        let ret = self.globals.len() as u32;
        self.globals.push(global);
        GlobalAddr(ret)
    }

    pub(crate) fn get_global(&self, global_addr: GlobalAddr) -> &Global {
        &self.globals[global_addr.0 as usize]
    }

    pub(crate) fn get_global_mut(&mut self, global_addr: GlobalAddr) -> &mut Global {
        &mut self.globals[global_addr.0 as usize]
    }
}

#[derive(Debug)]
pub(crate) struct Global {
    pub(crate) value: Value,
    pub(crate) mutable: bool, // Only needed for validation
}

pub(crate) enum Func {
    Wasm(WasmFunc),
    Host(HostFunc),
}

impl fmt::Debug for Func {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

pub(crate) struct HostFunc {
    /// Address of the function's module
    pub(crate) module_addr: ModuleAddr,
    /// Index of the function's type in its module
    pub(crate) ty_idx: TypeIdx,
    /// Function code
    pub(crate) fun: Rc<dyn Fn(&mut Runtime)>,
}

#[derive(Debug)]
pub(crate) struct WasmFunc {
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

impl Func {
    pub(crate) fn new(
        module_addr: ModuleAddr,
        ty_idx: TypeIdx,
        fun_addr: FunAddr,
        fun: wasm::FuncBody,
    ) -> Result<Func> {
        let (block_to_end, if_to_else) = gen_block_bounds(fun.code().elements())?;
        Ok(Func::Wasm(WasmFunc {
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
            Func::Wasm(fun) => fun.ty_idx,
            Func::Host(fun) => fun.ty_idx,
        }
    }

    pub(crate) fn module_addr(&self) -> ModuleAddr {
        match self {
            Func::Wasm(fun) => fun.module_addr,
            Func::Host(fun) => fun.module_addr,
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
