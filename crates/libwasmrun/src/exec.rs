pub mod frame;
pub mod stack;
pub mod store;
pub mod value;

use crate::{ExecError, Result};
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
const MAX_PAGES: usize = 65536; // (2**32 - 1 / PAGE_SIZE), or 0x10000

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

    pub fn clear_stack(&mut self) {
        self.stack.clear()
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

pub fn allocate_module(rt: &mut Runtime, mut parsed_module: wasm::Module) -> Result<ModuleIdx> {
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
                | wasm::External::Global(_) => {
                    return Err(ExecError::Panic(format!(
                        "Importing tables, memories, and globals are not implemented yet: {:?}",
                        import.external()
                    )));
                }
            }
        }
    }

    // Allocate functions
    if let Some(code_section) = parsed_module.code_section_mut() {
        for fun in replace(code_section.bodies_mut(), vec![]).into_iter() {
            let fun_idx = rt.store.funcs.len();

            let function_section = parsed_module.function_section().ok_or_else(|| {
                ExecError::Panic("Module has a code section but no function section".to_string())
            })?;

            rt.store.funcs.push(store::Func::new(
                module_idx,
                fun_idx,
                fun,
                function_section.entries()[fun_idx].type_ref(),
            )?);
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
                        return Err(ExecError::Panic(format!("Weird table offset: {:?}", val)));
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
                *table.get_mut(elem_idx).ok_or_else(|| {
                    ExecError::Panic(format!("Elem index out of bounds: {}", elem_idx))
                })? = Some(elem);
            }
        }
    }

    // Allocate memories
    if let Some(memory_section) = parsed_module.memory_section_mut() {
        assert!(memory_section.entries().len() <= 1); // No more than 1 currently
        for mem in memory_section.entries_mut().drain(..) {
            let mem_idx = rt.store.mems.len();
            // TODO: We need to record the upper bounds to be able to fail in memory.grow
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

    Ok(module_idx)
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

pub fn invoke_by_name(rt: &mut Runtime, module_idx: ModuleIdx, fun_name: &str) -> Result<()> {
    match rt.modules[module_idx].name_to_func.get(fun_name) {
        None => Err(ExecError::Panic(format!("Unknown function: {}", fun_name))),
        Some(fun_idx) => {
            let fun_idx = *fun_idx;
            invoke(rt, module_idx, fun_idx)
        }
    }
}

pub fn invoke(rt: &mut Runtime, module_idx: ModuleIdx, fun_idx: u32) -> Result<()> {
    let fun_addr = rt.modules[module_idx].func_addrs[fun_idx as usize];
    let func =
        rt.store.funcs.get(fun_addr as usize).ok_or_else(|| {
            ExecError::Panic(format!("Function address out of bounds: {}", fun_addr))
        })?;

    let arg_tys = rt.modules[module_idx].types[func.fun_ty_idx as usize].params();
    rt.frames.push(func, arg_tys);

    // Set locals for arguments
    let fun_arity = rt.get_module(module_idx).types[func.fun_ty_idx as usize]
        .params()
        .len();

    for local_idx in (0..fun_arity).rev() {
        let arg_val = rt.stack.pop_value()?;
        rt.frames
            .current_mut()?
            .set_local(local_idx as u32, arg_val)?;
    }

    rt.conts
        .last_mut()
        .ok_or_else(|| {
            ExecError::Panic("invoke: Empty continuation stack after function call".to_string())
        })?
        .push(rt.ip + 1);
    rt.conts.push(vec![]);
    rt.ip = 0;

    Ok(())
}

pub fn finish(rt: &mut Runtime) -> Result<()> {
    while !rt.frames.is_empty() {
        single_step(rt)?;
    }
    Ok(())
}

pub fn single_step(rt: &mut Runtime) -> Result<()> {
    let current_fun_idx = rt.frames.current()?.fun_idx;
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

        return Ok(());
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

            let n_pages = rt.stack.pop_i32()? as usize;
            let new_pages = (sz / PAGE_SIZE) + n_pages;

            if new_pages > MAX_PAGES {
                rt.stack.push_i32(-1);
            } else {
                mem.resize(sz + PAGE_SIZE * n_pages, 0);
                rt.stack.push_i32(sz_pages as i32);
            }

            rt.ip += 1;
        }

        Instruction::CurrentMemory(mem_ref) => {
            // memory.size
            assert_eq!(mem_ref, 0);
            let mem = &mut rt.store.mems[module_idx];
            let sz = mem.len();
            debug_assert!(sz % PAGE_SIZE == 0);
            rt.stack.push_i32((sz / PAGE_SIZE) as i32);
            rt.ip += 1;
        }

        Instruction::Nop => {
            rt.ip += 1;
        }

        Instruction::Select => {
            let select = rt.stack.pop_i32()?;
            let val2 = rt.stack.pop_value()?;
            let val1 = rt.stack.pop_value()?;
            if select == 0 {
                rt.stack.push_value(val2);
            } else {
                rt.stack.push_value(val1);
            }
            rt.ip += 1;
        }

        Instruction::I32Store(_, offset) => {
            let value = rt.stack.pop_i32()?;
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 4;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Store (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let [b1, b2, b3, b4] = value.to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 3] = b4;
            rt.ip += 1;
        }

        Instruction::F32Store(_, offset) => {
            let value = rt.stack.pop_f32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 4;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB F32Store (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let value: i32 = unsafe { ::std::mem::transmute(value) };
            let [b1, b2, b3, b4] = value.to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 3] = b4;

            rt.ip += 1;
        }

        Instruction::I64Store(_, offset) => {
            let value = rt.stack.pop_i64()?;
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 8;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I64Store (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let [b1, b2, b3, b4, b5, b6, b7, b8] = value.to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 3] = b4;
            mem[addr + 4] = b5;
            mem[addr + 5] = b6;
            mem[addr + 6] = b7;
            mem[addr + 7] = b8;
            rt.ip += 1;
        }

        Instruction::F64Store(_, offset) => {
            let value = rt.stack.pop_f64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 8;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB F64Store (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let value: i64 = unsafe { ::std::mem::transmute(value) };
            let [b1, b2, b3, b4, b5, b6, b7, b8] = value.to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 3] = b4;
            mem[addr + 4] = b5;
            mem[addr + 5] = b6;
            mem[addr + 6] = b7;
            mem[addr + 7] = b8;
            rt.ip += 1;
        }

        Instruction::I64Store8(_, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 1;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I64Store8 (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let val = (c & 0b1111_1111) as u8;
            mem[addr] = val;

            rt.ip += 1;
        }

        Instruction::I64Store16(_, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 2;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I64Store16 (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let val = (c & 0b1111_1111) as u8;
            mem[addr] = val;

            let val = ((c >> 8) & 0b1111_1111) as u8;
            mem[addr + 1] = val;

            rt.ip += 1;
        }

        Instruction::I64Store32(_, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 4;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I64Store32 (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let [b1, b2, b3, b4] = (c as u32).to_le_bytes();

            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 4] = b4;

            rt.ip += 1;
        }

        Instruction::I64Load8S(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 1;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I64Load8S (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let val = mem[addr];
            rt.stack.push_i64(val as i64);

            rt.ip += 1;
        }

        Instruction::I32Load(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 4;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Load (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            rt.stack.push_i32(i32::from_le_bytes([b1, b2, b3, b4]));
            rt.ip += 1;
        }

        Instruction::F32Load(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 4;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB F32Load (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            rt.stack
                .push_f32(unsafe { ::std::mem::transmute(i32::from_le_bytes([b1, b2, b3, b4])) });
            rt.ip += 1;
        }

        Instruction::I64Load(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 8;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I64Load (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            let b5 = mem[addr + 4];
            let b6 = mem[addr + 5];
            let b7 = mem[addr + 6];
            let b8 = mem[addr + 7];
            rt.stack
                .push_i64(i64::from_le_bytes([b1, b2, b3, b4, b5, b6, b7, b8]));
            rt.ip += 1;
        }

        Instruction::F64Load(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 8;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I64Load (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            let b5 = mem[addr + 4];
            let b6 = mem[addr + 5];
            let b7 = mem[addr + 6];
            let b8 = mem[addr + 7];
            rt.stack.push_f64(unsafe {
                ::std::mem::transmute(i64::from_le_bytes([b1, b2, b3, b4, b5, b6, b7, b8]))
            });
            rt.ip += 1;
        }

        Instruction::I32Load8U(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 1;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Load8U (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b = mem[addr];
            rt.stack.push_i32(b as i32);
            rt.ip += 1;
        }

        Instruction::I32Load8S(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 1;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Load8S (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b = mem[addr] as i8;
            let b_abs = b.abs() as i32;

            let val = if b < 0 { -1 * b_abs } else { b_abs };
            rt.stack.push_i32(val);
            rt.ip += 1;
        }

        Instruction::I64Load8U(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 1;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Load8U (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b = mem[addr];
            rt.stack.push_i64(b as i64);
            rt.ip += 1;
        }

        Instruction::I32Load16U(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 2;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Load16U (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            rt.stack.push_i32(i32::from_le_bytes([b1, b2, 0, 0]));
            rt.ip += 1;
        }

        Instruction::I32Load16S(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 2;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Load16S (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let i_16 = i16::from_le_bytes([b1, b2]);
            let i_16_abs = i_16.abs();

            let val = if i_16 < 0 {
                -1 * (i_16_abs as i32)
            } else {
                i_16_abs as i32
            };

            rt.stack.push_i32(val);
            rt.ip += 1;
        }

        Instruction::I64Load16U(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 2;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Load16U (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            rt.stack
                .push_i64(i64::from_le_bytes([b1, b2, 0, 0, 0, 0, 0, 0]));
            rt.ip += 1;
        }

        Instruction::I64Load16S(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 2;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I64Load16S (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let i_16 = i16::from_le_bytes([b1, b2]);
            let i_16_abs = i_16.abs();
            let val = if i_16 < 0 {
                -1 * (i_16_abs as i64)
            } else {
                i_16_abs as i64
            };

            rt.stack.push_i64(val);
            rt.ip += 1;
        }

        Instruction::I64Load32U(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 4;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Load32U (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            rt.stack
                .push_i64(i64::from_le_bytes([b1, b2, b3, b4, 0, 0, 0, 0]));
            rt.ip += 1;
        }

        Instruction::I64Load32S(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 4;

            let mem = &rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Load32S (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            let i_32 = i32::from_le_bytes([b1, b2, b3, b4]);
            let i_32_abs = i_32.abs();

            let val = if i_32 < 0 {
                -1 * (i_32_abs as i64)
            } else {
                i_32_abs as i64
            };

            rt.stack.push_i64(val);
            rt.ip += 1;
        }

        Instruction::I32Store8(_, offset) => {
            let c = rt.stack.pop_i32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 1;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Store8 (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            mem[addr] = c as u8;
            rt.ip += 1;
        }

        Instruction::I32Store16(_, offset) => {
            let c = rt.stack.pop_i32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = (addr + offset) as usize;
            let end_addr = addr + 2;

            let mem = &mut rt.store.mems[module_idx];
            if end_addr as usize > mem.len() {
                return Err(ExecError::Panic(format!(
                    "OOB I32Store8 (mem size={}, addr={})",
                    mem.len(),
                    addr
                )));
            }

            mem[addr] = c as u8;
            mem[addr + 1] = (c >> 8) as u8;
            rt.ip += 1;
        }

        Instruction::GetLocal(idx) => {
            let val = rt.frames.current()?.get_local(idx)?;
            rt.stack.push_value(val);
            rt.ip += 1;
        }

        Instruction::SetLocal(idx) => {
            let val = rt.stack.pop_value()?;
            rt.frames.current_mut()?.set_local(idx, val)?;
            rt.ip += 1;
        }

        Instruction::TeeLocal(idx) => {
            let val = rt.stack.pop_value()?;
            rt.frames.current_mut()?.set_local(idx, val)?;
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
            let value = rt.stack.pop_value()?;
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
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a == b)?;
            rt.ip += 1;
        }

        Instruction::F32Eq => {
            op2::<f32, bool, _>(&mut rt.stack, |a, b| a == b)?;
            rt.ip += 1;
        }

        Instruction::I64Eq => {
            op2::<i64, bool, _>(&mut rt.stack, |a, b| a == b)?;
            rt.ip += 1;
        }

        Instruction::F64Eq => {
            op2::<f64, bool, _>(&mut rt.stack, |a, b| a == b)?;
            rt.ip += 1;
        }

        Instruction::I32Ne => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a != b)?;
            rt.ip += 1;
        }
        Instruction::F32Ne => {
            op2::<f32, bool, _>(&mut rt.stack, |a, b| a != b)?;
            rt.ip += 1;
        }

        Instruction::I64Ne => {
            op2::<i64, bool, _>(&mut rt.stack, |a, b| a != b)?;
            rt.ip += 1;
        }

        Instruction::F64Ne => {
            op2::<f64, bool, _>(&mut rt.stack, |a, b| a != b)?;
            rt.ip += 1;
        }

        Instruction::I32Eqz => {
            let val = rt.stack.pop_i32()?;
            rt.stack.push_bool(val == 0);
            rt.ip += 1;
        }

        Instruction::I64Eqz => {
            let val = rt.stack.pop_i64()?;
            rt.stack.push_bool(val == 0);
            rt.ip += 1;
        }

        Instruction::I32LeU => {
            op2::<u32, bool, _>(&mut rt.stack, |a, b| a <= b)?;
            rt.ip += 1;
        }

        Instruction::I64LeU => {
            op2::<u64, bool, _>(&mut rt.stack, |a, b| a <= b)?;
            rt.ip += 1;
        }

        Instruction::I32LeS => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a <= b)?;
            rt.ip += 1;
        }

        Instruction::F32Le => {
            op2::<f32, bool, _>(&mut rt.stack, |a, b| a <= b)?;
            rt.ip += 1;
        }

        Instruction::I64LeS => {
            op2::<i64, bool, _>(&mut rt.stack, |a, b| a <= b)?;
            rt.ip += 1;
        }

        Instruction::F64Le => {
            op2::<f64, bool, _>(&mut rt.stack, |a, b| a <= b)?;
            rt.ip += 1;
        }

        Instruction::I32LtU => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a < b)?;
            rt.ip += 1;
        }

        Instruction::I64LtU => {
            op2::<i64, bool, _>(&mut rt.stack, |a, b| a < b)?;
            rt.ip += 1;
        }

        Instruction::I32Add => {
            op2::<i32, i32, _>(&mut rt.stack, i32::wrapping_add)?;
            rt.ip += 1;
        }

        Instruction::F32Add => {
            op2::<f32, f32, _>(&mut rt.stack, ::std::ops::Add::add)?;
            rt.ip += 1;
        }

        Instruction::I64Add => {
            op2::<i64, i64, _>(&mut rt.stack, i64::wrapping_add)?;
            rt.ip += 1;
        }

        Instruction::F64Add => {
            op2::<f64, f64, _>(&mut rt.stack, ::std::ops::Add::add)?;
            rt.ip += 1;
        }

        Instruction::I32Sub => {
            op2::<i32, i32, _>(&mut rt.stack, i32::wrapping_sub)?;
            rt.ip += 1;
        }

        Instruction::F32Sub => {
            op2::<f32, f32, _>(&mut rt.stack, ::std::ops::Sub::sub)?;
            rt.ip += 1;
        }

        Instruction::I64Sub => {
            op2::<i64, i64, _>(&mut rt.stack, i64::wrapping_sub)?;
            rt.ip += 1;
        }

        Instruction::F64Sub => {
            op2::<f64, f64, _>(&mut rt.stack, ::std::ops::Sub::sub)?;
            rt.ip += 1;
        }

        Instruction::I32Mul => {
            op2::<i32, i32, _>(&mut rt.stack, i32::wrapping_mul)?;
            rt.ip += 1;
        }

        Instruction::F32Mul => {
            op2::<f32, f32, _>(&mut rt.stack, ::std::ops::Mul::mul)?;
            rt.ip += 1;
        }

        Instruction::I64Mul => {
            op2::<i64, i64, _>(&mut rt.stack, i64::wrapping_mul)?;
            rt.ip += 1;
        }

        Instruction::F64Mul => {
            op2::<f64, f64, _>(&mut rt.stack, ::std::ops::Mul::mul)?;
            rt.ip += 1;
        }

        Instruction::I32DivS => {
            op2::<i32, i32, _>(&mut rt.stack, i32::wrapping_div)?;
            rt.ip += 1;
        }

        Instruction::F32Div => {
            op2::<f32, f32, _>(&mut rt.stack, ::std::ops::Div::div)?;
            rt.ip += 1;
        }

        Instruction::I64DivS => {
            op2::<i64, i64, _>(&mut rt.stack, i64::wrapping_div)?;
            rt.ip += 1;
        }

        Instruction::F64Div => {
            op2::<f64, f64, _>(&mut rt.stack, ::std::ops::Div::div)?;
            rt.ip += 1;
        }

        Instruction::I32DivU => {
            op2::<u32, u32, _>(&mut rt.stack, u32::wrapping_div)?;
            rt.ip += 1;
        }

        Instruction::I64DivU => {
            op2::<u64, u64, _>(&mut rt.stack, u64::wrapping_div)?;
            rt.ip += 1;
        }

        Instruction::I32Ctz => {
            let val = rt.stack.pop_i32()?;
            rt.stack.push_i32(val.trailing_zeros() as i32);
            rt.ip += 1;
        }

        Instruction::I64Ctz => {
            let val = rt.stack.pop_i64()?;
            rt.stack.push_i64(val.trailing_zeros() as i64);
            rt.ip += 1;
        }

        Instruction::I32GtU => {
            op2::<u32, bool, _>(&mut rt.stack, |a, b| a > b)?;
            rt.ip += 1;
        }

        Instruction::I64GtU => {
            op2::<u64, bool, _>(&mut rt.stack, |a, b| a > b)?;
            rt.ip += 1;
        }

        Instruction::I32LtS => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a < b)?;
            rt.ip += 1;
        }

        Instruction::F32Lt => {
            op2::<f32, bool, _>(&mut rt.stack, |a, b| a < b)?;
            rt.ip += 1;
        }

        Instruction::I64LtS => {
            op2::<i64, bool, _>(&mut rt.stack, |a, b| a < b)?;
            rt.ip += 1;
        }

        Instruction::F64Lt => {
            op2::<f64, bool, _>(&mut rt.stack, |a, b| a < b)?;
            rt.ip += 1;
        }

        Instruction::I32GtS => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a > b)?;
            rt.ip += 1;
        }

        Instruction::F32Gt => {
            op2::<f32, bool, _>(&mut rt.stack, |a, b| a > b)?;
            rt.ip += 1;
        }

        Instruction::I64GtS => {
            op2::<i64, bool, _>(&mut rt.stack, |a, b| a > b)?;
            rt.ip += 1;
        }

        Instruction::F64Gt => {
            op2::<f64, bool, _>(&mut rt.stack, |a, b| a > b)?;
            rt.ip += 1;
        }

        Instruction::I32GeU => {
            op2::<u32, bool, _>(&mut rt.stack, |a, b| a >= b)?;
            rt.ip += 1;
        }

        Instruction::I64GeU => {
            op2::<u64, bool, _>(&mut rt.stack, |a, b| a >= b)?;
            rt.ip += 1;
        }

        Instruction::I32GeS => {
            op2::<i32, bool, _>(&mut rt.stack, |a, b| a >= b)?;
            rt.ip += 1;
        }

        Instruction::F32Ge => {
            op2::<f32, bool, _>(&mut rt.stack, |a, b| a >= b)?;
            rt.ip += 1;
        }

        Instruction::I64GeS => {
            op2::<i64, bool, _>(&mut rt.stack, |a, b| a >= b)?;
            rt.ip += 1;
        }

        Instruction::F64Ge => {
            op2::<f64, bool, _>(&mut rt.stack, |a, b| a >= b)?;
            rt.ip += 1;
        }

        Instruction::I32Popcnt => {
            let i = rt.stack.pop_i32()?;
            let ret = unsafe { ::core::arch::x86_64::_popcnt32(i) };
            rt.stack.push_i32(ret);
            rt.ip += 1;
        }

        Instruction::I64Popcnt => {
            let i = rt.stack.pop_i64()?;
            let ret = unsafe { ::core::arch::x86_64::_popcnt64(i) };
            rt.stack.push_i64(ret as i64);
            rt.ip += 1;
        }

        Instruction::I32Or => {
            op2::<i32, i32, _>(&mut rt.stack, ::std::ops::BitOr::bitor)?;
            rt.ip += 1;
        }

        Instruction::I64Or => {
            op2::<i64, i64, _>(&mut rt.stack, ::std::ops::BitOr::bitor)?;
            rt.ip += 1;
        }

        Instruction::I32And => {
            op2::<i32, i32, _>(&mut rt.stack, ::std::ops::BitAnd::bitand)?;
            rt.ip += 1;
        }

        Instruction::I64And => {
            op2::<i64, i64, _>(&mut rt.stack, ::std::ops::BitAnd::bitand)?;
            rt.ip += 1;
        }

        Instruction::I32WrapI64 => {
            let i = rt.stack.pop_i64()?;
            rt.stack.push_i32((i % 2i64.pow(32)) as i32);
            rt.ip += 1;
        }

        Instruction::I32RemS => {
            op2::<i32, i32, _>(&mut rt.stack, |a, b| a.wrapping_rem(b))?;
            rt.ip += 1;
        }

        Instruction::I64RemS => {
            op2::<i64, i64, _>(&mut rt.stack, |a, b| a.wrapping_rem(b))?;
            rt.ip += 1;
        }

        Instruction::I32RemU => {
            op2::<u32, u32, _>(&mut rt.stack, |a, b| a.wrapping_rem(b))?;
            rt.ip += 1;
        }

        Instruction::I64RemU => {
            op2::<u64, u64, _>(&mut rt.stack, |a, b| a.wrapping_rem(b))?;
            rt.ip += 1;
        }

        Instruction::I32ShrU => {
            op2::<u32, u32, _>(&mut rt.stack, |a, b| a.wrapping_shr(b))?;
            rt.ip += 1;
        }

        Instruction::I64ShrU => {
            op2::<u64, u64, _>(&mut rt.stack, |a, b| a.wrapping_shr(b as u32))?; // FIXME shift amount
            rt.ip += 1;
        }

        Instruction::I32ShrS => {
            op2::<i32, i32, _>(&mut rt.stack, |a, b| a.wrapping_shr(b as u32))?;
            rt.ip += 1;
        }

        Instruction::I64ShrS => {
            op2::<i64, i64, _>(&mut rt.stack, |a, b| a.wrapping_shr(b as u32))?; // FIXME shift amount
            rt.ip += 1;
        }

        Instruction::I32Shl => {
            op2::<u32, u32, _>(&mut rt.stack, |a, b| a.wrapping_shl(b))?;
            rt.ip += 1;
        }

        Instruction::I64Shl => {
            op2::<u32, u32, _>(&mut rt.stack, |a, b| a.wrapping_shl(b))?;
            rt.ip += 1;
        }

        Instruction::I32Rotl => {
            op2::<u32, u32, _>(&mut rt.stack, |a, b| a.rotate_left(b))?;
            rt.ip += 1;
        }

        Instruction::I64Rotl => {
            op2::<u64, u64, _>(&mut rt.stack, |a, b| a.rotate_left(b as u32))?; // FIXME shift amount
            rt.ip += 1;
        }

        Instruction::I32Rotr => {
            op2::<u32, u32, _>(&mut rt.stack, |a, b| a.rotate_right(b))?;
            rt.ip += 1;
        }

        Instruction::I64Rotr => {
            op2::<u64, u64, _>(&mut rt.stack, |a, b| a.rotate_right(b as u32))?; // FIXME shift amount
            rt.ip += 1;
        }

        Instruction::I32Xor => {
            op2::<i32, i32, _>(&mut rt.stack, ::std::ops::BitXor::bitxor)?;
            rt.ip += 1;
        }

        Instruction::I64Xor => {
            op2::<i64, i64, _>(&mut rt.stack, ::std::ops::BitXor::bitxor)?;
            rt.ip += 1;
        }

        Instruction::I32Clz => {
            let i = rt.stack.pop_i32()?;
            rt.stack.push_i32(i.leading_zeros() as i32);
            rt.ip += 1;
        }

        Instruction::I64Clz => {
            let i = rt.stack.pop_i64()?;
            rt.stack.push_i64(i.leading_zeros() as i64);
            rt.ip += 1;
        }

        Instruction::F32Max => {
            op2::<f32, f32, _>(&mut rt.stack, f32::max)?;
            rt.ip += 1;
        }

        Instruction::F64Max => {
            op2::<f64, f64, _>(&mut rt.stack, f64::max)?;
            rt.ip += 1;
        }

        Instruction::F32Min => {
            op2::<f32, f32, _>(&mut rt.stack, f32::min)?;
            rt.ip += 1;
        }

        Instruction::F64Min => {
            op2::<f64, f64, _>(&mut rt.stack, f64::min)?;
            rt.ip += 1;
        }

        Instruction::F32Neg => {
            let f = rt.stack.pop_f32()?;
            rt.stack.push_f32(-1f32 * f);
            rt.ip += 1;
        }

        Instruction::F64Neg => {
            let f = rt.stack.pop_f64()?;
            rt.stack.push_f64(-1f64 * f);
            rt.ip += 1;
        }

        Instruction::F32Sqrt => {
            let f = rt.stack.pop_f32()?;
            rt.stack.push_f32(f.sqrt());
            rt.ip += 1;
        }

        Instruction::F64Sqrt => {
            let f = rt.stack.pop_f64()?;
            rt.stack.push_f64(f.sqrt());
            rt.ip += 1;
        }

        Instruction::F32Abs => {
            let f = rt.stack.pop_f32()?;
            rt.stack.push_f32(f.abs());
            rt.ip += 1;
        }

        Instruction::F64Abs => {
            let f = rt.stack.pop_f64()?;
            rt.stack.push_f64(f.abs());
            rt.ip += 1;
        }

        Instruction::F32Ceil => {
            let f = rt.stack.pop_f32()?;
            rt.stack.push_f32(f.ceil());
            rt.ip += 1;
        }

        Instruction::F64Ceil => {
            let f = rt.stack.pop_f64()?;
            rt.stack.push_f64(f.ceil());
            rt.ip += 1;
        }

        Instruction::F32Floor => {
            let f = rt.stack.pop_f32()?;
            rt.stack.push_f32(f.floor());
            rt.ip += 1;
        }

        Instruction::F64Floor => {
            let f = rt.stack.pop_f64()?;
            rt.stack.push_f64(f.floor());
            rt.ip += 1;
        }

        Instruction::F32Trunc => {
            let f = rt.stack.pop_f32()?;
            rt.stack.push_f32(f.trunc());
            rt.ip += 1;
        }

        Instruction::F64Trunc => {
            let f = rt.stack.pop_f64()?;
            rt.stack.push_f64(f.trunc());
            rt.ip += 1;
        }

        Instruction::I32ReinterpretF32 => {
            let f = rt.stack.pop_f32()?;
            rt.stack.push_i32(unsafe { ::std::mem::transmute(f) });
            rt.ip += 1;
        }

        Instruction::I64ReinterpretF64 => {
            let f = rt.stack.pop_f64()?;
            rt.stack.push_i64(unsafe { ::std::mem::transmute(f) });
            rt.ip += 1;
        }

        Instruction::F32ReinterpretI32 => {
            let f = rt.stack.pop_i32()?;
            rt.stack.push_f32(unsafe { ::std::mem::transmute(f) });
            rt.ip += 1;
        }

        Instruction::F64ReinterpretI64 => {
            let f = rt.stack.pop_i64()?;
            rt.stack.push_f64(unsafe { ::std::mem::transmute(f) });
            rt.ip += 1;
        }

        /* FIXME: These implementations are not correct

                Instruction::F32Copysign => {
                    op2::<f32, f32, _>(&mut rt.stack, |a, b| {
                        if b.is_sign_negative() {
                            -1f32 * a
                        } else {
                            a
                        }
                    })?;
                    rt.ip += 1;
                }

                Instruction::F64Copysign => {
                    op2::<f64, f64, _>(&mut rt.stack, |a, b| {
                        if b.is_sign_negative() {
                            -1f64 * a
                        } else {
                            a
                        }
                    })?;
                    rt.ip += 1;
                }
        */
        Instruction::Call(func_idx) => {
            // NB. invoke updates the ip
            invoke(rt, module_idx, func_idx)?;
        }

        Instruction::CallIndirect(_sig, table_ref) => {
            let elem_idx = rt.stack.pop_i32()?;

            let table_addr = rt.modules[module_idx].table_addrs[table_ref as usize];
            let fun = rt.store.tables[table_addr as usize][elem_idx as usize];

            let fun_idx = match fun {
                Some(fun_idx) => fun_idx,
                None => {
                    return Err(ExecError::Panic(format!(
                        "Table index not initialized. module={}, table={}, elem_idx={}",
                        module_idx, table_ref, elem_idx
                    )));
                }
            };

            invoke(rt, module_idx, fun_idx)?;
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
                    return Err(ExecError::Panic(
                        "Couldn't find continuation of block".to_string(),
                    ));
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
                    return Err(ExecError::Panic(
                        "Couldn't find continuation of block".to_string(),
                    ));
                }
                Some(cont) => *cont,
            };
            rt.conts.last_mut().unwrap().push(cont);

            let cond = rt.stack.pop_i32()?;
            if cond == 0 {
                let current_fun = &rt.store.funcs[current_fun_idx as usize];
                match current_fun.else_instrs.get(&rt.ip) {
                    None => match current_fun.block_bounds.get(&rt.ip) {
                        None => {
                            return Err(ExecError::Panic(
                                "Couldn't find else block or continuation of if".to_string(),
                            ));
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
                    return Err(ExecError::Panic(
                        "Couldn't find continuation of else".to_string(),
                    ));
                }
                Some(cont) => *cont,
            };
            rt.ip = cont;
        }

        Instruction::Br(n_blocks) => {
            br(rt, n_blocks)?;
        }

        Instruction::BrIf(n_blocks) => {
            if rt.stack.pop_i32()? == 0 {
                rt.ip += 1;
            } else {
                br(rt, n_blocks)?;
            }
        }

        Instruction::BrTable(table) => {
            let idx = rt.stack.pop_i32()?;
            let n_blocks = match table.table.get(idx as usize) {
                None => table.default,
                Some(n_blocks) => *n_blocks,
            };
            br(rt, n_blocks)?;
        }

        Instruction::Drop => {
            let _ = rt.stack.pop_value();
            rt.ip += 1;
        }

        Instruction::Unreachable => {
            return Err(ExecError::Trap);
        }

        other => {
            return Err(ExecError::Panic(format!(
                "Instruction not implemented: {:?}",
                other
            )));
        } // Instruction::F32Nearest => {}
          // Instruction::F64Nearest => {}
          // Instruction::I32TruncSF32 => {}
          // Instruction::I32TruncUF32 => {}
          // Instruction::I32TruncSF64 => {}
          // Instruction::I32TruncUF64 => {}
          // Instruction::I64ExtendSI32 => {}
          // Instruction::I64ExtendUI32 => {}
          // Instruction::I64TruncSF32 => {}
          // Instruction::I64TruncUF32 => {}
          // Instruction::I64TruncSF64 => {}
          // Instruction::I64TruncUF64 => {}
          // Instruction::F32ConvertSI32 => {}
          // Instruction::F32ConvertUI32 => {}
          // Instruction::F32ConvertSI64 => {}
          // Instruction::F32ConvertUI64 => {}
          // Instruction::F32DemoteF64 => {}
          // Instruction::F64ConvertSI32 => {}
          // Instruction::F64ConvertUI32 => {}
          // Instruction::F64ConvertSI64 => {}
          // Instruction::F64ConvertUI64 => {}
          // Instruction::F64PromoteF32 => {}
          // Instruction::Atomics(_) => {}
          // Instruction::Simd(_) => {}
          // Instruction::SignExt(_) => {}
          // Instruction::Bulk(_) => {}
    }

    Ok(())
}

fn op2<A: StackValue, B: StackValue, F: Fn(A, A) -> B>(stack: &mut Stack, op: F) -> Result<()> {
    let val2 = A::pop(stack)?;
    let val1 = A::pop(stack)?;
    let ret = op(val1, val2);
    ret.push(stack);
    Ok(())
}

fn br(rt: &mut Runtime, n_blocks: u32) -> Result<()> {
    for _ in 0..n_blocks + 1 {
        let cont_frame = rt.conts.last_mut().unwrap();
        match cont_frame.pop() {
            None => {
                // Function return
                let current_fun_idx = rt.frames.current()?.fun_idx;
                let current_fun = &rt.store.funcs[current_fun_idx as usize];
                rt.ip = current_fun.fun.code().elements().len() as u32;
            }
            Some(ip) => {
                rt.ip = ip;
            }
        }
    }
    Ok(())
}
