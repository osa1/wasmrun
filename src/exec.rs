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

use crate::parser;
use crate::parser::{Export, FuncIdx, FuncType, Instruction, MemArg};

use std::rc::Rc;

type Addr = u32;

#[derive(Default)]
pub struct Module {
    types: Vec<FuncType>,
    func_addrs: Vec<Addr>,
    table_addrs: Vec<Addr>,
    mem_addrs: Vec<Addr>,
    global_addrs: Vec<Addr>,
    exports: Vec<Export>,
    start: Option<FuncIdx>,
}

#[derive(Default)]
pub struct Runtime {
    store: Store,
    stack: Stack,
    frames: FrameStack,
    modules: Vec<Module>,
}

impl Runtime {
    pub fn allocate_module(&mut self, mut parsed_module: parser::Module) -> ModuleIdx {
        // https://webassembly.github.io/spec/core/exec/modules.html

        let module_idx = self.modules.len();

        let mut inst = Module::default();

        // Allocate functions
        for fun in parsed_module.funs.drain(..) {
            let fun_idx = self.store.funcs.len();
            self.store.funcs.push(store::Func { module_idx, fun });
            inst.func_addrs.push(fun_idx as u32);
        }

        // Allocate tables
        for table in parsed_module.tables.drain(..) {
            let table_idx = self.store.tables.len();
            self.store
                .tables
                .push(vec![None; table.limits.min as usize]);
            inst.table_addrs.push(table_idx as u32);
        }

        // Allocate memories
        assert!(parsed_module.mem_addrs.len() <= 1); // No more than 1
        for mem in parsed_module.mem_addrs.drain(..) {
            let mem_idx = self.store.mems.len();
            self.store.mems.push(vec![0; mem.min as usize]);
            inst.mem_addrs.push(mem_idx as u32);
        }

        // Allocate globals
        for global in parsed_module.globals.drain(..) {
            let global_idx = self.store.globals.len();
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
            self.store.globals.push(Global {
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
        self.modules.push(inst);

        module_idx
    }

    pub fn get_module_start(&self, idx: ModuleIdx) -> Option<FuncIdx> {
        self.modules[idx].start
    }

    pub fn call(&mut self, module_idx: ModuleIdx, fun_idx: u32) {
        let fun_addr = self.modules[module_idx].func_addrs[fun_idx as usize];
        let fun = &self.store.funcs[fun_addr as usize].fun;
        // TODO: push dummy values for locals
    }
}

pub fn exec(runtime: &mut Runtime, block: Rc<[Instruction]>) {
    let ip_stack: Vec<(Rc<[Instruction]>, u32)> = vec![(block, 0)];

    while let Some((block, ip)) = ip_stack.last().cloned() {
        use Instruction::*;
        let instr = &block[ip as usize];

        println!("{}: {:?}", ip, instr);

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

                // ip += 1;
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

                // ip += 1;
            }

            LocalGet(idx) => {
                let val = runtime.frames.current().get_local(*idx);
                runtime.stack.push(val);
                // ip += 1;
            }

            LocalSet(idx) => {
                let val = runtime.stack.pop();
                runtime.frames.current_mut().set_local(*idx, val);
                // ip += 1;
            }

            LocalTee(idx) => {
                let val = runtime.stack.pop();
                runtime.frames.current_mut().set_local(*idx, val);
                runtime.stack.push(val);
                // ip += 1;
            }

            GlobalGet(idx) => {
                let current_module = runtime.frames.current().module();
                let global_idx = runtime.modules[current_module].global_addrs[*idx as usize];
                let value = runtime.store.globals[global_idx as usize].value;
                runtime.stack.push(value);
                // ip += 1;
            }

            GlobalSet(idx) => {
                let current_module = runtime.frames.current().module();
                let global_idx = runtime.modules[current_module].global_addrs[*idx as usize];
                let value = runtime.stack.pop();
                runtime.store.globals[global_idx as usize].value = value;
                // ip += 1;
            }

            I32Const(i) => {
                runtime.stack.push_i32(*i);
                // ip += 1;
            }

            I64Const(i) => {
                runtime.stack.push_i64(*i);
                // ip += 1;
            }

            F32Const(f) => {
                runtime.stack.push_f32(*f);
                // ip += 1;
            }

            F64Const(f) => {
                runtime.stack.push_f64(*f);
                // ip += 1;
            }

            I32Le_u => {
                let val2 = runtime.stack.pop_i32();
                let val1 = runtime.stack.pop_i32();
                runtime.stack.push_bool(val1 <= val2);
                // ip += 1;
            }

            Call(x) => {
                todo!()
                /*
                let module_idx = runtime.frames.current().module();
                let module = &runtime.modules[module_idx];
                let func_addr = module.func_addrs[*x as usize];
                let fun = &runtime.store.funcs[func_addr as usize];
                runtime.frames.push(fun);
                let instrs = fun.fun.expr.instrs.clone();
                exec(runtime, &*instrs, 0);
                runtime.frames.pop();
                ip += 1;
                */
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
                return;
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
