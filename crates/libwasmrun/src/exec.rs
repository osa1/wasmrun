pub mod frame;
pub mod stack;
pub mod store;
pub mod value;

use frame::FrameStack;
use stack::{Stack, StackValue};
use store::{Global, ModuleIdx, Store};
pub use value::Value;

use fxhash::FxHashMap;
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
    /// Maps function names to their indices in `func_addrs`
    pub name_to_func: FxHashMap<String, u32>,
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

    pub fn pop_value(&mut self) -> Option<Value> {
        self.stack.pop_value_opt()
    }

    pub fn push_value(&mut self, value: Value) {
        self.stack.push_value(value)
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

    // Add exported function names to name_to_func
    for export in &inst.exports {
        match export.internal() {
            wasm::Internal::Function(fun_idx) => {
                inst.name_to_func
                    .insert(export.field().to_owned(), *fun_idx);
            }
            wasm::Internal::Table(_) | wasm::Internal::Memory(_) | wasm::Internal::Global(_) => {}
        }
    }

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

    // Allocate table elements
    if let Some(element_section) = parsed_module.elements_section() {
        for elements in element_section.entries() {
            let table_idx = elements.index();
            let offset = elements
                .offset()
                .as_ref()
                .map(|init_expr| get_const_expr_val(init_expr.code()));

            let offset = match offset {
                Some(val) => match val {
                    Value::I32(offset) => offset as usize,
                    Value::I64(offset) => offset as usize,
                    Value::F32(_) | Value::F64(_) => {
                        panic!("Weird table offset: {:?}", val);
                    }
                },
                None => 0,
            };

            let table = &mut rt.store.tables[inst.table_addrs[table_idx as usize] as usize];
            for (elem_idx, elem) in elements.members().iter().copied().enumerate() {
                let elem_idx = offset + elem_idx;
                if elem_idx <= table.len() {
                    table.resize(elem_idx + 1, None);
                }
                table[elem_idx] = Some(elem);
            }
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
            let value = get_const_expr_val(global.init_expr().code());
            rt.store.globals.push(Global {
                value,
                mutable: global.global_type().is_mutable(),
            });
            inst.global_addrs.push(global_idx as u32);
        }
    }

    // Get names of functions, for now just to be able to call functions by name
    if let Some(names_section) = parsed_module.names_section_mut() {
        if let Some(function_names) = names_section.functions_mut() {
            for (fun_idx, fun_name) in
                replace(function_names.names_mut(), Default::default()).into_iter()
            {
                inst.name_to_func.insert(fun_name, fun_idx);
            }
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

fn get_const_expr_val(instrs: &[Instruction]) -> Value {
    use Instruction::*;
    match instrs {
        [I32Const(value), End] => Value::I32(*value),
        [I64Const(value), End] => Value::I64(*value),
        [F32Const(value), End] => Value::F32(unsafe { transmute(*value) }),
        [F64Const(value), End] => Value::F64(unsafe { transmute(*value) }),
        [GetGlobal(_idx), End] => {
            // See the comments in `ConstExpr` type. This can only be an import.
            todo!("get.global constant expression")
        }
        other => todo!("Global initializer: {:?}", other),
    }
}

pub fn invoke_by_name(rt: &mut Runtime, module_idx: ModuleIdx, fun_name: &str) {
    match rt.modules[module_idx].name_to_func.get(fun_name) {
        None => {
            panic!("Unknown function: {}", fun_name);
        }
        Some(fun_idx) => {
            let fun_idx = *fun_idx;
            invoke(rt, module_idx, fun_idx);
        }
    }
}

pub fn invoke(rt: &mut Runtime, module_idx: ModuleIdx, fun_idx: u32) {
    let fun_addr = rt.modules[module_idx].func_addrs[fun_idx as usize];
    let func = &rt.store.funcs[fun_addr as usize];

    let arg_tys = rt.modules[module_idx].types[func.fun_ty_idx as usize].params();
    rt.frames.push(func, arg_tys);

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

    // Instruction is just 3 words so clonning here should be fine, and avoid borrowchk issues
    // later on
    let instr = current_fun.fun.code().elements()[rt.ip as usize].clone();
    let module_idx = current_fun.module_idx;

    /*
        println!(
            "instruction={:?}, stack={:?}, call stack={:?}, conts={:?}",
            instr, rt.stack, rt.frames, rt.conts
        );
    */

    match instr {
        Instruction::GrowMemory(mem_ref) => {
            assert_eq!(mem_ref, 0);
            let mem = &mut rt.store.mems[module_idx];
            let sz = mem.len();
            debug_assert!(sz % PAGE_SIZE == 0);
            let sz_pages = sz / PAGE_SIZE;

            // NB. This operation currently does not fail
            let n_pages = rt.stack.pop_i32() as usize;
            mem.resize(mem.len() + PAGE_SIZE * n_pages, 0);

            rt.stack.push_i32(sz_pages as i32);

            rt.ip += 1;
        }

        Instruction::Nop => {
            rt.ip += 1;
        }

        Instruction::Select => {
            let select = rt.stack.pop_i32();
            let val2 = rt.stack.pop_value();
            let val1 = rt.stack.pop_value();
            if select == 0 {
                rt.stack.push_value(val2);
            } else {
                rt.stack.push_value(val1);
            }
            rt.ip += 1;
        }

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
            let val = rt.frames.current().get_local(idx);
            rt.stack.push_value(val);
            rt.ip += 1;
        }

        Instruction::SetLocal(idx) => {
            let val = rt.stack.pop_value();
            rt.frames.current_mut().set_local(idx, val);
            rt.ip += 1;
        }

        Instruction::TeeLocal(idx) => {
            let val = rt.stack.pop_value();
            rt.frames.current_mut().set_local(idx, val);
            rt.stack.push_value(val);
            rt.ip += 1;
        }

        Instruction::GetGlobal(idx) => {
            let global_idx = rt.modules[module_idx].global_addrs[idx as usize];
            let value = rt.store.globals[global_idx as usize].value;
            rt.stack.push_value(value);
            rt.ip += 1;
        }

        Instruction::SetGlobal(idx) => {
            let global_idx = rt.modules[module_idx].global_addrs[idx as usize];
            let value = rt.stack.pop_value();
            rt.store.globals[global_idx as usize].value = value;
            rt.ip += 1;
        }

        Instruction::I32Const(i) => {
            rt.stack.push_i32(i);
            rt.ip += 1;
        }

        Instruction::I64Const(i) => {
            rt.stack.push_i64(i);
            rt.ip += 1;
        }

        Instruction::F32Const(f) => {
            rt.stack.push_f32(unsafe { transmute(f) });
            rt.ip += 1;
        }

        Instruction::F64Const(f) => {
            rt.stack.push_f64(unsafe { transmute(f) });
            rt.ip += 1;
        }

        Instruction::I32Eq => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a == b);
            rt.ip += 1;
        }

        Instruction::I32Ne => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a != b);
            rt.ip += 1;
        }

        Instruction::I32Eqz => {
            let val = rt.stack.pop_i32();
            rt.stack.push_bool(val == 0);
            rt.ip += 1;
        }

        Instruction::I64Eqz => {
            let val = rt.stack.pop_i64();
            rt.stack.push_bool(val == 0);
            rt.ip += 1;
        }

        Instruction::I32LeU => {
            op2::<u32, bool, _>(&mut rt.stack, |a, b| a <= b);
            rt.ip += 1;
        }

        Instruction::I32LeS => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a <= b);
            rt.ip += 1;
        }

        Instruction::I32LtU => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a < b);
            rt.ip += 1;
        }

        Instruction::I32Add => {
            op2::<i32, i32, _>(&mut rt.stack, ::std::ops::Add::add);
            rt.ip += 1;
        }

        Instruction::I64Add => {
            op2::<i64, i64, _>(&mut rt.stack, ::std::ops::Add::add);
            rt.ip += 1;
        }

        Instruction::I32Sub => {
            op2::<i32, i32, _>(&mut rt.stack, ::std::ops::Sub::sub);
            rt.ip += 1;
        }

        Instruction::I64Sub => {
            op2::<i64, i64, _>(&mut rt.stack, ::std::ops::Sub::sub);
            rt.ip += 1;
        }

        Instruction::I32Mul => {
            op2::<i32, i32, _>(&mut rt.stack, ::std::ops::Mul::mul);
            rt.ip += 1;
        }

        Instruction::I64Mul => {
            op2::<i64, i64, _>(&mut rt.stack, ::std::ops::Mul::mul);
            rt.ip += 1;
        }

        Instruction::I32Ctz => {
            let val = rt.stack.pop_i32();
            rt.stack.push_i32(val.trailing_zeros() as i32);
            rt.ip += 1;
        }

        Instruction::I32GtU => {
            op2::<u32, bool, _>(&mut rt.stack, |a, b| a > b);
            rt.ip += 1;
        }

        Instruction::I64GtU => {
            op2::<u64, bool, _>(&mut rt.stack, |a, b| a > b);
            rt.ip += 1;
        }

        Instruction::I32Or => {
            op2::<i32, i32, _>(&mut rt.stack, ::std::ops::BitOr::bitor);
            rt.ip += 1;
        }

        Instruction::Call(func_idx) => {
            // NB. invoke updates the ip
            invoke(rt, module_idx, func_idx);
        }

        Instruction::CallIndirect(_sig, table_ref) => {
            let elem_idx = rt.stack.pop_i32();

            let table_addr = rt.modules[module_idx].table_addrs[table_ref as usize];
            let fun = rt.store.tables[table_addr as usize][elem_idx as usize];

            let fun_idx = match fun {
                Some(fun_idx) => fun_idx,
                None => {
                    panic!(
                        "Table index not initialized. module={}, table={}, elem_idx={}",
                        module_idx, table_ref, elem_idx
                    );
                }
            };

            invoke(rt, module_idx, fun_idx);
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

        Instruction::Block(_) | Instruction::Loop(_) => {
            let current_fun = &rt.store.funcs[current_fun_idx as usize];
            let cont = match current_fun.block_bounds.get(&rt.ip) {
                None => {
                    panic!("Couldn't find continuation of block");
                }
                Some(cont) => *cont,
            };
            rt.conts.last_mut().unwrap().push(cont);
            rt.ip += 1;
        }

        Instruction::If(_) => {
            let current_fun = &rt.store.funcs[current_fun_idx as usize];
            let cont = match current_fun.block_bounds.get(&rt.ip) {
                None => {
                    panic!("Couldn't find continuation of if");
                }
                Some(cont) => *cont,
            };
            rt.conts.last_mut().unwrap().push(cont);

            let cond = rt.stack.pop_i32();
            if cond == 0 {
                let current_fun = &rt.store.funcs[current_fun_idx as usize];
                match current_fun.else_instrs.get(&rt.ip) {
                    None => match current_fun.block_bounds.get(&rt.ip) {
                        None => {
                            panic!("Couldn't find else block or continuation of if");
                        }
                        Some(cont) => {
                            rt.ip = *cont;
                        }
                    },
                    Some(else_) => {
                        rt.ip = else_ + 1;
                    }
                }
            } else {
                rt.ip += 1;
            }
        }

        Instruction::Else => {
            let current_fun = &rt.store.funcs[current_fun_idx as usize];
            let cont = match current_fun.block_bounds.get(&rt.ip) {
                None => {
                    panic!("Couldn't find continuation of else");
                }
                Some(cont) => *cont,
            };
            rt.ip = cont;
        }

        Instruction::Br(n_blocks) => {
            br(rt, n_blocks);
        }

        Instruction::BrIf(n_blocks) => {
            if rt.stack.pop_i32() == 0 {
                rt.ip += 1;
            } else {
                br(rt, n_blocks);
            }
        }

        Instruction::BrTable(table) => {
            let idx = rt.stack.pop_i32();
            let n_blocks = match table.table.get(idx as usize) {
                None => table.default,
                Some(n_blocks) => *n_blocks,
            };
            br(rt, n_blocks);
        }

        Instruction::Drop => {
            let _ = rt.stack.pop_value();
            rt.ip += 1;
        }

        other => todo!("Instruction not implemented: {:?}", other),
    }
}

fn op2<A: StackValue, B: StackValue, F: Fn(A, A) -> B>(stack: &mut Stack, op: F) {
    let val2 = A::pop(stack);
    let val1 = A::pop(stack);
    let ret = op(val1, val2);
    ret.push(stack);
}

fn br(rt: &mut Runtime, n_blocks: u32) {
    for _ in 0..n_blocks + 1 {
        let cont_frame = rt.conts.last_mut().unwrap();
        match cont_frame.pop() {
            None => {
                // Function return
                let current_fun_idx = rt.frames.current().fun_idx;
                let current_fun = &rt.store.funcs[current_fun_idx as usize];
                rt.ip = current_fun.fun.code().elements().len() as u32;
            }
            Some(ip) => {
                rt.ip = ip;
            }
        }
    }
}
