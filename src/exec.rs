mod frame;
mod stack;
mod store;
mod value;

use frame::{Frame, FrameStack};
use stack::Stack;
use store::{Global, ModuleIdx, Store};
use value::Value;

use parity_wasm::elements as wasm;

use std::mem::{replace, transmute};

type Addr = u32;

type FuncIdx = u32;

const PAGE_SIZE: usize = 65536;

#[derive(Default)]
pub struct Module {
    pub types: Vec<wasm::FunctionType>,
    pub func_addrs: Vec<Addr>,
    pub table_addrs: Vec<Addr>,
    pub mem_addrs: Vec<Addr>,
    pub global_addrs: Vec<Addr>,
    pub exports: Vec<wasm::ExportEntry>,
    pub start: Option<FuncIdx>,
}

#[derive(Default)]
pub struct Runtime {
    store: Store,
    stack: Stack,
    frames: FrameStack,
    modules: Vec<Module>,
    // Stack of block continuations. TODO: should this be a part of Frame?
    blocks: Vec<Vec<u32>>,
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
        /*
                let mut ip = replace(&mut self.ip, vec![]);

                if let Some((block_ty, current_block, block_ip)) = ip.pop() {
                    if (block_ip + 1) as usize >= current_block.len() {
                        match block_ty {
                            BlockType::Function => {
                                // End of the function, the function frame will be popped by `call`.
                                ip.push((block_ty, current_block, block_ip + 1));
                            }
                            BlockType::Block => {
                                // End of the block, which is already popped.
                            }
                            BlockType::Loop => {
                                // End of loop, jump to beginning.
                                ip.push((block_ty, current_block, 0));
                            }
                        }
                    } else {
                        ip.push((block_ty, current_block, block_ip + 1));
                    }
                }

                self.ip = ip;
        */
    }
}

pub fn allocate_module(rt: &mut Runtime, mut parsed_module: wasm::Module) -> ModuleIdx {
    // https://webassembly.github.io/spec/core/exec/modules.html

    let module_idx = rt.modules.len();

    let mut inst = Module::default();
    inst.types = parsed_module
        .type_section_mut()
        .map(|section| {
            section
                .types_mut()
                .drain(..)
                .map(|ty| match ty {
                    wasm::Type::Function(fun_ty) => fun_ty,
                })
                .collect()
        })
        .unwrap_or(vec![]);

    inst.exports = parsed_module
        .export_section_mut()
        .map(|section| replace(section.entries_mut(), vec![]))
        .unwrap_or(vec![]);

    // Allocate imported functions
    // TODO: allocate other imported stuff (tables, memories, globals)
    // TODO: not sure how to resolve imports yet
    if let Some(import_section) = parsed_module.import_section_mut() {
        for import in import_section.entries_mut().drain(..) {
            match import.external() {
                wasm::External::Function(_) => {
                    // FIXME
                    inst.func_addrs.push(u32::MAX);
                }
                wasm::External::Table(_)
                | wasm::External::Memory(_)
                | wasm::External::Global(_) => todo!(),
            }
        }
    }

    // Allocate functions
    if let Some(code_section) = parsed_module.code_section_mut() {
        for fun in replace(code_section.bodies_mut(), vec![]).into_iter() {
            let fun_idx = rt.store.funcs.len();
            let block_bounds = store::gen_block_bounds(fun.code().elements());
            rt.store.funcs.push(store::Func {
                module_idx,
                fun_idx,
                fun,
                fun_ty_idx: parsed_module.function_section().unwrap().entries()[fun_idx].type_ref(),
                block_bounds,
            });
            inst.func_addrs.push(fun_idx as u32);
        }
    }

    // Allocate tables
    if let Some(table_section) = parsed_module.table_section_mut() {
        for table in table_section.entries_mut().drain(..) {
            let table_idx = rt.store.tables.len();
            rt.store
                .tables
                .push(vec![None; table.limits().initial() as usize]);
            inst.table_addrs.push(table_idx as u32);
        }
    }

    // Allocate memories
    if let Some(memory_section) = parsed_module.memory_section_mut() {
        assert!(memory_section.entries().len() <= 1); // No more than 1 currently
        for mem in memory_section.entries_mut().drain(..) {
            let mem_idx = rt.store.mems.len();
            rt.store
                .mems
                .push(vec![0; mem.limits().initial() as usize * PAGE_SIZE]);
            inst.mem_addrs.push(mem_idx as u32);
        }
    }

    // Allcoate globals
    if let Some(global_section) = parsed_module.global_section_mut() {
        for global in global_section.entries_mut().drain(..) {
            let global_idx = rt.store.globals.len();

            let value = match global.init_expr().code() {
                [wasm::Instruction::I32Const(value)] => Value::I32(*value),
                [wasm::Instruction::I64Const(value)] => Value::I64(*value),
                [wasm::Instruction::F32Const(value)] => Value::F32(unsafe { transmute(*value) }),
                [wasm::Instruction::F64Const(value)] => Value::F64(unsafe { transmute(*value) }),
                [wasm::Instruction::GetGlobal(_idx)] => {
                    // See the comments in `ConstExpr` type. This can only be an import.
                    todo!()
                }
                _ => todo!(),
            };

            rt.store.globals.push(Global {
                value,
                mutable: global.global_type().is_mutable(),
            });

            inst.global_addrs.push(global_idx as u32);
        }
    }

    // TODO: Initialize the table with 'elems'
    // TODO: Initialize the memory with 'data'

    // Set start
    inst.start = parsed_module.start_section();

    // Done
    rt.modules.push(inst);

    module_idx
}

pub fn call(rt: &mut Runtime, module_idx: ModuleIdx, fun_idx: u32) {
    let fun_addr = rt.modules[module_idx].func_addrs[fun_idx as usize];
    let func = &rt.store.funcs[fun_addr as usize];

    // println!("func: {:#?}", func);

    rt.frames.push(func);

    // Set locals for arguments
    let fun_arity = rt.get_module(module_idx).types[func.fun_ty_idx as usize]
        .params()
        .len();

    for local_idx in (0..fun_arity).rev() {
        let arg_val = rt.stack.pop_value();
        rt.frames.current_mut().set_local(local_idx as u32, arg_val);
    }

    rt.blocks.push(vec![]);

    // Run until the end of the function.
    exec(rt);

    // Pop function frame
    rt.frames.pop();

    // Pop blocks of the function
    rt.blocks.pop();
}

pub fn exec(rt: &mut Runtime) {
    while let Some(Frame {
        fun_idx,
        locals: _,
        ip,
    }) = rt.frames.current_opt()
    {
        let store::Func {
            module_idx,
            fun_idx: _,
            fun,
            block_bounds,
            fun_ty_idx: _,
        } = &rt.store.funcs[*fun_idx as usize];

        let instr = &fun.code().elements()[*ip as usize];

        println!("{}: {:?}", ip, instr);
        // println!("frames: {:?}", runtime.frames);
        // println!("block: {:?}", runtime.ip);

        use wasm::Instruction::*;
        match instr {
            I32Store(_, offset) => {
                let value = rt.stack.pop_i32();
                let addr = rt.stack.pop_i32() as u32;
                let addr = (addr + offset) as usize;
                let end_addr = addr + 4;

                let mem = &mut rt.store.mems[*module_idx];
                if end_addr as usize > mem.len() {
                    panic!("OOB I32Store (mem size={}, addr={})", mem.len(), addr);
                }

                let [b1, b2, b3, b4] = value.to_le_bytes();
                mem[addr] = b1;
                mem[addr + 1] = b2;
                mem[addr + 2] = b3;
                mem[addr + 4] = b4;

                rt.next_instr();
            }

            I32Load(_, offset) => {
                let addr = rt.stack.pop_i32() as u32;
                let addr = (addr + offset) as usize;
                let end_addr = addr + 4;

                let mem = &rt.store.mems[*module_idx];
                if end_addr as usize > mem.len() {
                    panic!("OOB I32Load (mem size={}, addr={})", mem.len(), addr);
                }

                let b1 = mem[addr];
                let b2 = mem[addr + 1];
                let b3 = mem[addr + 2];
                let b4 = mem[addr + 3];
                rt.stack.push_i32(i32::from_le_bytes([b1, b2, b3, b4]));

                rt.next_instr();
            }

            GetLocal(idx) => {
                let val = rt.frames.current().get_local(*idx);
                rt.stack.push_value(val);
                rt.next_instr();
            }

            SetLocal(idx) => {
                let val = rt.stack.pop_value();
                rt.frames.current_mut().set_local(*idx, val);
                rt.next_instr();
            }

            TeeLocal(idx) => {
                let val = rt.stack.pop_value();
                rt.frames.current_mut().set_local(*idx, val);
                rt.stack.push_value(val);
                rt.next_instr();
            }

            GetGlobal(idx) => {
                let global_idx = rt.modules[*module_idx].global_addrs[*idx as usize];
                let value = rt.store.globals[global_idx as usize].value;
                rt.stack.push_value(value);
                rt.next_instr();
            }

            SetGlobal(idx) => {
                let global_idx = rt.modules[*module_idx].global_addrs[*idx as usize];
                let value = rt.stack.pop_value();
                rt.store.globals[global_idx as usize].value = value;
                rt.next_instr();
            }

            I32Const(i) => {
                rt.stack.push_i32(*i);
                rt.next_instr();
            }

            I64Const(i) => {
                rt.stack.push_i64(*i);
                rt.next_instr();
            }

            F32Const(f) => {
                rt.stack.push_f32(unsafe { transmute(*f) });
                rt.next_instr();
            }

            F64Const(f) => {
                rt.stack.push_f64(unsafe { transmute(*f) });
                rt.next_instr();
            }

            I32Eqz => {
                let val = rt.stack.pop_i32();
                rt.stack.push_bool(val == 0);
                rt.next_instr();
            }

            I32LeU => {
                let val2 = rt.stack.pop_i32();
                let val1 = rt.stack.pop_i32();
                rt.stack.push_bool(val1 <= val2);
                rt.next_instr();
            }

            I32Sub => {
                let val2 = rt.stack.pop_i32();
                let val1 = rt.stack.pop_i32();
                rt.stack.push_i32(val1 - val2);
                rt.next_instr();
            }

            //////////////////////////
            // Control instructions //
            //////////////////////////
            Call(func_idx) => {
                let module_idx = *module_idx;
                let func_idx = *func_idx;
                call(rt, module_idx, func_idx);
                rt.next_instr();
            }

            CallIndirect(_type_idx, _table_idx) => {
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

            /*
                        Block(parser::types::Block { ty: _, instrs }) => {
                            // Bump instruction pointer for the current block
                            rt.next_instr();
                            // Execute the new block
                            rt.ip.push((BlockType::Block, instrs.clone(), 0));
                        }

                        Loop(parser::types::Block { ty: _, instrs: _ }) => todo!(),

                        BrIf(lbl_idx) => {
                            let val = rt.stack.pop_i32();
                            if val != 0 {
                                for _ in 0..=*lbl_idx {
                                    rt.ip.pop();
                                }
                            // Parent block's instruction pointer was already bumped by 'Block' case above,
                            // so no need to update it
                            } else {
                                rt.next_instr();
                            }
                        }
            */
            _ => todo!("unhandled instruction: {:?}", instr),
        }
    }
}
