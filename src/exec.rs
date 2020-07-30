#![allow(dead_code)]

mod const_expr;
mod frame;
mod stack;
mod store;
mod value;

use const_expr::ConstExpr;
use frame::FrameStack;
use stack::Stack;
use store::{Global, ModuleIdx, Store};
use value::Value;

use crate::parser;
use crate::parser::{Export, FuncIdx, FuncType, ImportDesc, Instruction, MemArg};

use std::mem::replace;
use std::rc::Rc;

type Addr = u32;

#[derive(Default)]
pub struct Module {
    pub types: Vec<FuncType>,
    pub func_addrs: Vec<Addr>,
    pub table_addrs: Vec<Addr>,
    pub mem_addrs: Vec<Addr>,
    pub global_addrs: Vec<Addr>,
    pub exports: Vec<Export>,
    pub start: Option<FuncIdx>,
}

#[derive(Debug, Clone, Copy)]
enum BlockType {
    // A block in a function
    Block,
    // A loop in a function
    Loop,
    // Main block of a function
    Function,
}

#[derive(Default)]
pub struct Runtime {
    store: Store,
    stack: Stack,
    frames: FrameStack,
    modules: Vec<Module>,

    // Instruction pointer. Currently we don't need to make this a part of `Runtime`, but at some
    // point we'll have debugging commands and we want to be able to stop at any point in execution
    // and then continue. For that we need to store the current point in program permanently, and I
    // think this is a good place for that.
    ip: Vec<(BlockType, Rc<[Instruction]>, u32)>,
}

impl Runtime {
    pub fn get_module(&self, idx: ModuleIdx) -> &Module {
        &self.modules[idx]
    }

    pub fn get_module_start(&self, idx: ModuleIdx) -> Option<FuncIdx> {
        self.modules[idx].start
    }

    // Move on to the next instruction in the current function. Depending on the current block type
    // this may jump forwards or backwards.
    fn next_instr(&mut self) {
        let mut ip = replace(&mut self.ip, vec![]);

        if let Some((block_ty, current_block, block_ip)) = ip.pop() {
            if (block_ip + 1) as usize >= current_block.len() {
                match block_ty {
                    BlockType::Function => {
                        // End of the function, the function frame will be popped by `call`
                        ip.push((block_ty, current_block, block_ip));
                        self.ip = ip;
                    }
                    BlockType::Block => {
                        // End of the block, which is already popped. Restore ip.
                        self.ip = ip;
                    }
                    BlockType::Loop => {
                        // End of loop, jump to beginning.
                        ip.push((block_ty, current_block, 0));
                        self.ip = ip;
                    }
                }
            } else {
                ip.push((block_ty, current_block, block_ip + 1));
                self.ip = ip;
            }
        }
    }
}

pub fn allocate_module(rt: &mut Runtime, mut parsed_module: parser::Module) -> ModuleIdx {
    // https://webassembly.github.io/spec/core/exec/modules.html

    let module_idx = rt.modules.len();

    let mut inst = Module::default();
    inst.exports = parsed_module.exports;

    // Allocate imported functions
    // TODO: allocate other imported stuff (tables, memories, globals)
    // TODO: not sure how to resolve imports yet
    for import in parsed_module.imports.drain(..) {
        match import.desc {
            ImportDesc::Func(_) => {
                // FIXME
                inst.func_addrs.push(u32::MAX);
            }
            ImportDesc::Table(_) | ImportDesc::MemType(_) | ImportDesc::Global(_) => {}
        }
    }

    // Allocate functions
    for fun in parsed_module.funs.drain(..) {
        let fun_idx = rt.store.funcs.len();
        rt.store.funcs.push(store::Func { module_idx, fun });
        inst.func_addrs.push(fun_idx as u32);
    }

    // Allocate tables
    for table in parsed_module.tables.drain(..) {
        let table_idx = rt.store.tables.len();
        rt.store.tables.push(vec![None; table.limits.min as usize]);
        inst.table_addrs.push(table_idx as u32);
    }

    // Allocate memories
    assert!(parsed_module.mem_addrs.len() <= 1); // No more than 1
    for mem in parsed_module.mem_addrs.drain(..) {
        let mem_idx = rt.store.mems.len();
        rt.store.mems.push(vec![0; mem.min as usize]);
        inst.mem_addrs.push(mem_idx as u32);
    }

    // Allocate globals
    for global in parsed_module.globals.drain(..) {
        let global_idx = rt.store.globals.len();
        let value = match ConstExpr::from_expr(&global.expr) {
            None => panic!(
                "Global value is not a constant expression: {:?}",
                global.expr
            ),
            Some(ConstExpr::Const(value)) => value,
            Some(ConstExpr::GlobalGet(_idx)) =>
            // See the comments in `ConstExpr` type. This can only be an import.
            {
                todo!()
            }
        };
        rt.store.globals.push(Global {
            value,
            mutable: global.ty.mut_ == parser::types::Mutability::Var,
        });
        inst.global_addrs.push(global_idx as u32);
    }

    // TODO: Initialize the table with 'elems'
    // TODO: Initialize the memory with 'data'

    // Set start
    inst.start = parsed_module.start;

    // Done
    rt.modules.push(inst);

    module_idx
}

pub fn call(rt: &mut Runtime, module_idx: ModuleIdx, fun_idx: u32) {
    let fun_addr = rt.modules[module_idx].func_addrs[fun_idx as usize];
    let func = &rt.store.funcs[fun_addr as usize];

    // println!("func: {:#?}", func);

    // Normally we'd pop arguments, push return address, push arguments again, but this is
    // the entry so we don't have a return address.

    rt.frames.push(func);

    // Initialize instruction pointer
    rt.ip
        .push((BlockType::Function, func.fun.expr.instrs.clone(), 0));

    // Run until the end of the function.
    exec(rt);

    rt.frames.pop();

    // Pop blocks of the function
    while let Some((BlockType::Block | BlockType::Loop, _, _)) = rt.ip.last() {
        let _ = rt.ip.pop().unwrap();
    }
    // Pop the function
    let _ = rt.ip.pop().unwrap();
}

pub fn exec(runtime: &mut Runtime) {
    while let Some((_, block, ip)) = runtime.ip.last().cloned() {
        use Instruction::*;

        if ip as usize == block.len() {
            runtime.next_instr(); // pop the block
            return;
        }

        let instr = &block[ip as usize];

        println!("{}: {:?}", ip, instr);
        // println!("frames: {:?}", runtime.frames);
        // println!("block: {:?}", runtime.ip);

        match instr {
            I32Store(MemArg { align: _, offset }) => {
                let value = runtime.stack.pop_i32();
                let addr = runtime.stack.pop_i32() as u32;
                let addr = (addr + offset) as usize;
                let end_addr = addr + 4;

                let current_module = runtime.frames.current().module();
                let mem = &mut runtime.store.mems[current_module];
                if end_addr as usize > mem.len() {
                    panic!("OOB I32Store (mem size={}, addr={})", mem.len(), addr);
                }

                let [b1, b2, b3, b4] = value.to_le_bytes();
                mem[addr] = b1;
                mem[addr + 1] = b2;
                mem[addr + 2] = b3;
                mem[addr + 4] = b4;

                runtime.next_instr();
            }

            I32Load(MemArg { align: _, offset }) => {
                let addr = runtime.stack.pop_i32() as u32;
                let addr = (addr + offset) as usize;
                let end_addr = addr + 4;

                let current_module = runtime.frames.current().module();
                let mem = &runtime.store.mems[current_module];
                if end_addr as usize > mem.len() {
                    panic!("OOB I32Load (mem size={}, addr={})", mem.len(), addr);
                }

                let b1 = mem[addr];
                let b2 = mem[addr + 1];
                let b3 = mem[addr + 2];
                let b4 = mem[addr + 3];
                runtime.stack.push_i32(i32::from_le_bytes([b1, b2, b3, b4]));

                runtime.next_instr();
            }

            LocalGet(idx) => {
                let val = runtime.frames.current().get_local(*idx);
                runtime.stack.push_value(val);
                runtime.next_instr();
            }

            LocalSet(idx) => {
                let val = runtime.stack.pop_value();
                runtime.frames.current_mut().set_local(*idx, val);
                runtime.next_instr();
            }

            LocalTee(idx) => {
                let val = runtime.stack.pop_value();
                runtime.frames.current_mut().set_local(*idx, val);
                runtime.stack.push_value(val);
                runtime.next_instr();
            }

            GlobalGet(idx) => {
                let current_module = runtime.frames.current().module();
                let global_idx = runtime.modules[current_module].global_addrs[*idx as usize];
                let value = runtime.store.globals[global_idx as usize].value;
                runtime.stack.push_value(value);
                runtime.next_instr();
            }

            GlobalSet(idx) => {
                let current_module = runtime.frames.current().module();
                let global_idx = runtime.modules[current_module].global_addrs[*idx as usize];
                let value = runtime.stack.pop_value();
                runtime.store.globals[global_idx as usize].value = value;
                runtime.next_instr();
            }

            I32Const(i) => {
                runtime.stack.push_i32(*i);
                runtime.next_instr();
            }

            I64Const(i) => {
                runtime.stack.push_i64(*i);
                runtime.next_instr();
            }

            F32Const(f) => {
                runtime.stack.push_f32(*f);
                runtime.next_instr();
            }

            F64Const(f) => {
                runtime.stack.push_f64(*f);
                runtime.next_instr();
            }

            I32Le_u => {
                let val2 = runtime.stack.pop_i32();
                let val1 = runtime.stack.pop_i32();
                runtime.stack.push_bool(val1 <= val2);
                runtime.next_instr();
            }

            Call(func_idx) => {
                let module_idx = runtime.frames.current().module();
                call(runtime, module_idx, *func_idx);
                runtime.next_instr();
            }

            CallIndirect(type_idx) => {
                todo!()
                /*
                let module_idx = runtime.frames.current().module();
                let table_idx = runtime.modules[module_idx].table_addrs[0];
                let table = &runtime.store.tables[table_idx as usize];
                let fun_idx = runtime.stack.pop_i32();
                match table.get(fun_idx as usize) {
                    None => {
                        panic!("call_indirect: OOB function index (function idx={}, table idx={}, table size={})",
                               fun_idx, table_idx, table.len());
                    }
                    Some(None) => {
                        panic!("call_indirect: function index not initialized (function idx={}, table idx={})",
                               fun_idx, table_idx);
                    }
                    Some(Some(fun_addr)) => {
                        let fun = &runtime.store.funcs[*fun_addr as usize];

                        let fun_ty = fun.fun.ty;
                        if fun_ty != *type_idx {
                            panic!("call_indirect: function type doesn't match expected type (fun ty={}, expected={})",
                                   fun_ty, type_idx);
                        }

                        runtime.frames.push(fun);
                        let instrs = fun.fun.expr.instrs.clone();
                        exec(runtime, &*instrs, 0);
                        runtime.frames.pop();
                        ip += 1;
                    }
                }
                */
            }

            Return => {
                break;
            }

            Block(parser::types::Block { ty: _, instrs }) => {
                todo!()
                /*
                // TODO: I think type is not useful for execution?
                let instrs = instrs.clone();
                exec(runtime, &*instrs, 0);
                */
            }

            Loop(parser::types::Block { ty: _, instrs }) => todo!(),

            _ => todo!("unhandled instruction: {:?}", instr),
        }
    }
}
