pub mod frame;
pub mod stack;
pub mod store;
pub mod value;

use frame::FrameStack;
use stack::Stack;
use store::{Global, ModuleIdx, Store};
use value::Value;

use parity_wasm::elements as wasm;
use wasm::Instruction;

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

pub struct Runtime {
    /// The heap
    store: Store,
    /// Value stack
    stack: Stack,
    /// Call stack
    frames: FrameStack,
    ///
    modules: Vec<Module>,
    /// Continuation stack. `conts.last_mut().pop()` gives the continuation for the current block.
    /// On return we pop the outer vector to drop the blocks of the current function.
    conts: Vec<Vec<u32>>,
    /// Instruction pointer. Instruction index in the function at the top of the call stack
    /// (`frames`).
    ip: u32,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            store: Default::default(),
            stack: Default::default(),
            frames: Default::default(),
            modules: Default::default(),
            conts: vec![vec![]],
            ip: 0,
        }
    }

    pub fn get_module(&self, idx: ModuleIdx) -> &Module {
        &self.modules[idx]
    }

    pub fn get_module_start(&self, idx: ModuleIdx) -> Option<FuncIdx> {
        self.modules[idx].start
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
            rt.store.funcs.push(store::Func::new(
                module_idx,
                fun_idx,
                fun,
                parsed_module.function_section().unwrap().entries()[fun_idx].type_ref(),
            ));
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

            use Instruction::*;
            let value = match global.init_expr().code() {
                [I32Const(value), End] => Value::I32(*value),
                [I64Const(value), End] => Value::I64(*value),
                [F32Const(value), End] => Value::F32(unsafe { transmute(*value) }),
                [F64Const(value), End] => Value::F64(unsafe { transmute(*value) }),
                [GetGlobal(_idx), End] => {
                    // See the comments in `ConstExpr` type. This can only be an import.
                    todo!()
                }
                other => todo!("Global initializer: {:?}", other),
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

pub fn invoke(rt: &mut Runtime, module_idx: ModuleIdx, fun_idx: u32) {
    let fun_addr = rt.modules[module_idx].func_addrs[fun_idx as usize];
    let func = &rt.store.funcs[fun_addr as usize];

    rt.frames.push(func);

    // Set locals for arguments
    let fun_arity = rt.get_module(module_idx).types[func.fun_ty_idx as usize]
        .params()
        .len();

    for local_idx in (0..fun_arity).rev() {
        let arg_val = rt.stack.pop_value();
        rt.frames.current_mut().set_local(local_idx as u32, arg_val);
    }

    rt.conts.last_mut().unwrap().push(rt.ip + 1);
    rt.conts.push(vec![]);
    rt.ip = 0;
}

pub fn finish(rt: &mut Runtime) {
    while !rt.frames.is_empty() {
        single_step(rt);
    }
}

pub fn single_step(rt: &mut Runtime) {
    let current_fun_idx = rt.frames.current().fun_idx;
    let current_fun = &rt.store.funcs[current_fun_idx as usize];

    if rt.ip as usize >= current_fun.fun.code().elements().len() {
        // End of the function, pop the frame, update ip.
        // TODO: Should this really take one step?

        rt.frames.pop(); // Pop call frame
        rt.conts.pop(); // Pop blocks of current frame
        rt.ip = rt
            .conts
            .last_mut()
            .and_then(|conts| conts.pop())
            .unwrap_or(0); // TODO: Setting ip 0 here going to be a problem?

        return;
    }

    let instr = &current_fun.fun.code().elements()[rt.ip as usize];
    let module_idx = current_fun.module_idx;

    println!(
        "instruction={:?}, stack={:?}, call stack={:?}",
        instr, rt.stack, rt.frames
    );

    match instr {
        Instruction::I32Store(_, offset) => {
            let value = rt.stack.pop_i32();
            let addr = rt.stack.pop_i32() as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 4;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                panic!("OOB I32Store (mem size={}, addr={})", mem.len(), addr);
            }

            let [b1, b2, b3, b4] = value.to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 4] = b4;
            rt.ip += 1;
        }

        Instruction::I32Load(_, offset) => {
            let addr = rt.stack.pop_i32() as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 4;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                panic!("OOB I32Load (mem size={}, addr={})", mem.len(), addr);
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            rt.stack.push_i32(i32::from_le_bytes([b1, b2, b3, b4]));
            rt.ip += 1;
        }

        Instruction::GetLocal(idx) => {
            let val = rt.frames.current().get_local(*idx);
            rt.stack.push_value(val);
            rt.ip += 1;
        }

        Instruction::SetLocal(idx) => {
            let val = rt.stack.pop_value();
            rt.frames.current_mut().set_local(*idx, val);
            rt.ip += 1;
        }

        Instruction::TeeLocal(idx) => {
            let val = rt.stack.pop_value();
            rt.frames.current_mut().set_local(*idx, val);
            rt.stack.push_value(val);
            rt.ip += 1;
        }

        Instruction::GetGlobal(idx) => {
            let global_idx = rt.modules[module_idx].global_addrs[*idx as usize];
            let value = rt.store.globals[global_idx as usize].value;
            rt.stack.push_value(value);
            rt.ip += 1;
        }

        Instruction::SetGlobal(idx) => {
            let global_idx = rt.modules[module_idx].global_addrs[*idx as usize];
            let value = rt.stack.pop_value();
            rt.store.globals[global_idx as usize].value = value;
            rt.ip += 1;
        }

        Instruction::I32Const(i) => {
            rt.stack.push_i32(*i);
            rt.ip += 1;
        }

        Instruction::I64Const(i) => {
            rt.stack.push_i64(*i);
            rt.ip += 1;
        }

        Instruction::F32Const(f) => {
            rt.stack.push_f32(unsafe { transmute(*f) });
            rt.ip += 1;
        }

        Instruction::F64Const(f) => {
            rt.stack.push_f64(unsafe { transmute(*f) });
            rt.ip += 1;
        }

        Instruction::I32Eqz => {
            let val = rt.stack.pop_i32();
            rt.stack.push_bool(val == 0);
            rt.ip += 1;
        }

        Instruction::I32LeU => {
            let val2 = rt.stack.pop_i32();
            let val1 = rt.stack.pop_i32();
            rt.stack.push_bool(val1 <= val2);
            rt.ip += 1;
        }

        Instruction::I32Sub => {
            let val2 = rt.stack.pop_i32();
            let val1 = rt.stack.pop_i32();
            rt.stack.push_i32(val1 - val2);
            rt.ip += 1;
        }

        Instruction::Call(func_idx) => {
            let func_idx = *func_idx;
            // NB. invoke updates the ip
            invoke(rt, module_idx, func_idx);
        }

        Instruction::End => {
            match rt.conts.last_mut().unwrap().pop() {
                Some(_) => {
                    // End of the block
                    rt.ip += 1;
                }
                None => {
                    // End of the function. Code is the same as `return` case.
                    rt.frames.pop();
                    rt.conts.pop();
                    rt.ip = rt.conts.last_mut().unwrap().pop().unwrap();
                }
            }
        }

        Instruction::Return => {
            rt.frames.pop();
            rt.conts.pop();
            rt.ip = rt.conts.last_mut().unwrap().pop().unwrap();
        }

        Instruction::Block(_) => {
            let cont = match current_fun.block_bounds.get(&rt.ip) {
                None => {
                    panic!("Couldn't find continuation of block");
                }
                Some(cont) => *cont,
            };
            rt.conts.last_mut().unwrap().push(cont);
            rt.ip += 1;
        }

        Instruction::BrIf(n_blocks) => {
            if rt.stack.pop_i32() == 0 {
                rt.ip += 1;
            } else {
                for _ in 0..*n_blocks {
                    rt.conts.last_mut().unwrap().pop().unwrap();
                }
                rt.ip = *rt.conts.last_mut().unwrap().last().unwrap();
            }
        }

        other => todo!("Instruction not implemented: {:?}", other),
    }
}
