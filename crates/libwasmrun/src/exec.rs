use crate::frame::FrameStack;
use crate::mem::Mem;
use crate::stack::{Stack, StackValue};
use crate::store::{Func, Global, ModuleIdx, Store};
pub use crate::value::Value;
use crate::{ExecError, Result};

use fxhash::FxHashMap;
use ieee754::Ieee754;
use parity_wasm::elements as wasm;
use wasm::Instruction;

use std::mem::{replace, transmute};

type Addr = u32;

type FuncIdx = u32;

pub const PAGE_SIZE: usize = 65536;
pub const MAX_PAGES: u32 = 65536; // (2**32 - 1 / PAGE_SIZE), or 0x10000

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

            rt.store.funcs.push(Func::new(
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
                Some(val) => match val? {
                    Value::I32(offset) => offset as usize,
                    Value::I64(offset) => offset as usize,
                    val => {
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
            rt.store
                .mems
                .push(Mem::new(mem.limits().initial(), mem.limits().maximum()));
            inst.mem_addrs.push(mem_idx as u32);
        }
    }

    // Allcoate globals
    if let Some(global_section) = parsed_module.global_section_mut() {
        for global in global_section.entries_mut().drain(..) {
            let global_idx = rt.store.globals.len();
            let value = get_const_expr_val(global.init_expr().code())?;
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

    // Initialize memories with 'data' section
    if let Some(data_section) = parsed_module.data_section() {
        for data in data_section.entries() {
            let index: u32 = data.index();
            let offset = match data.offset() {
                None => 0,
                Some(offset_expr) => match get_const_expr_val(offset_expr.code())? {
                    Value::I32(offset) => offset as u32,
                    other => {
                        return Err(ExecError::Panic(format!(
                            "Weird data section offset: {:?}",
                            other
                        )));
                    }
                },
            };
            let values = data.value();

            let mem_addr = inst.mem_addrs[index as usize];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.set_range(offset, values)?;
        }
    }

    // TODO: Initialize the table with 'elems'

    // Set start
    inst.start = parsed_module.start_section();

    // Done
    rt.modules.push(inst);

    Ok(module_idx)
}

fn get_const_expr_val(instrs: &[Instruction]) -> Result<Value> {
    use Instruction::*;
    match instrs {
        [I32Const(value), End] => Ok(Value::I32(*value)),
        [I64Const(value), End] => Ok(Value::I64(*value)),
        [F32Const(value), End] => Ok(Value::F32(unsafe { transmute(*value) })),
        [F64Const(value), End] => Ok(Value::F64(unsafe { transmute(*value) })),
        [GetGlobal(_idx), End] => {
            // See the comments in `ConstExpr` type. This can only be an import.
            Err(ExecError::Panic(
                "get.global constant expression not supported yet".to_string(),
            ))
        }
        other => Err(ExecError::Panic(format!("Global initializer: {:?}", other))),
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

            let current_pages = mem.size_pages();
            let new_pages = rt.stack.pop_i32()? as u32;
            let total_pages = current_pages + new_pages;

            if total_pages > mem.max_pages().unwrap_or(MAX_PAGES) {
                rt.stack.push_i32(-1);
            } else {
                mem.add_pages(new_pages);
                rt.stack.push_i32(total_pages as i32);
            }

            rt.ip += 1;
        }

        Instruction::CurrentMemory(mem_ref) => {
            // memory.size
            assert_eq!(mem_ref, 0);
            let mem = &mut rt.store.mems[module_idx];
            rt.stack.push_i32(mem.size_pages() as i32);
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
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr + 3)?;

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
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr + 3)?;

            let value: i32 = unsafe { transmute(value) };
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
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr + 7)?;

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
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr + 7)?;

            let value: i64 = unsafe { transmute(value) };
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
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr)?;

            let val = (c & 0b1111_1111) as u8;
            mem[addr] = val;

            rt.ip += 1;
        }

        Instruction::I64Store16(_, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr + 1)?;

            let val = (c & 0b1111_1111) as u8;
            mem[addr] = val;

            let val = ((c >> 8) & 0b1111_1111) as u8;
            mem[addr + 1] = val;

            rt.ip += 1;
        }

        Instruction::I64Store32(_, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr + 3)?;

            let [b1, b2, b3, b4] = (c as u32).to_le_bytes();

            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 3] = b4;

            rt.ip += 1;
        }

        Instruction::I64Load8S(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr)?;

            let val = mem[addr];
            rt.stack.push_i64(val as i64);

            rt.ip += 1;
        }

        Instruction::I32Load(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 3)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            rt.stack.push_i32(i32::from_le_bytes([b1, b2, b3, b4]));
            rt.ip += 1;
        }

        Instruction::F32Load(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 3)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            rt.stack
                .push_f32(unsafe { transmute(i32::from_le_bytes([b1, b2, b3, b4])) });
            rt.ip += 1;
        }

        Instruction::I64Load(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 7)?;

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
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 7)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            let b5 = mem[addr + 4];
            let b6 = mem[addr + 5];
            let b7 = mem[addr + 6];
            let b8 = mem[addr + 7];
            rt.stack.push_f64(unsafe {
                transmute(i64::from_le_bytes([b1, b2, b3, b4, b5, b6, b7, b8]))
            });
            rt.ip += 1;
        }

        Instruction::I32Load8U(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr)?;

            let b = mem[addr];
            rt.stack.push_i32(b as i32);
            rt.ip += 1;
        }

        Instruction::I32Load8S(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr)?;

            let b = mem[addr] as i8;
            let b_abs = b.abs() as i32;

            let val = if b < 0 { -1 * b_abs } else { b_abs };
            rt.stack.push_i32(val);
            rt.ip += 1;
        }

        Instruction::I64Load8U(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr)?;

            let b = mem[addr];
            rt.stack.push_i64(b as i64);
            rt.ip += 1;
        }

        Instruction::I32Load16U(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 1)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            rt.stack.push_i32(i32::from_le_bytes([b1, b2, 0, 0]));
            rt.ip += 1;
        }

        Instruction::I32Load16S(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 1)?;

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
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 1)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            rt.stack
                .push_i64(i64::from_le_bytes([b1, b2, 0, 0, 0, 0, 0, 0]));
            rt.ip += 1;
        }

        Instruction::I64Load16S(_, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 1)?;

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
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 3)?;

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
            let addr = addr + offset;

            let mem = &rt.store.mems[module_idx];
            mem.check_range(addr + 3)?;

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
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr)?;

            mem[addr] = c as u8;
            rt.ip += 1;
        }

        Instruction::I32Store16(_, offset) => {
            let c = rt.stack.pop_i32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = addr + offset;

            let mem = &mut rt.store.mems[module_idx];
            mem.check_range(addr + 1)?;

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
            op2::<i32, bool, _>(rt, |a, b| a == b)?;
        }

        Instruction::F32Eq => {
            op2::<f32, bool, _>(rt, |a, b| a == b)?;
        }

        Instruction::I64Eq => {
            op2::<i64, bool, _>(rt, |a, b| a == b)?;
        }

        Instruction::F64Eq => {
            op2::<f64, bool, _>(rt, |a, b| a == b)?;
        }

        Instruction::I32Ne => {
            op2::<i32, bool, _>(rt, |a, b| a != b)?;
        }

        Instruction::F32Ne => {
            op2::<f32, bool, _>(rt, |a, b| a != b)?;
        }

        Instruction::I64Ne => {
            op2::<i64, bool, _>(rt, |a, b| a != b)?;
        }

        Instruction::F64Ne => {
            op2::<f64, bool, _>(rt, |a, b| a != b)?;
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
            op2::<u32, bool, _>(rt, |a, b| a <= b)?;
        }

        Instruction::I64LeU => {
            op2::<u64, bool, _>(rt, |a, b| a <= b)?;
        }

        Instruction::I32LeS => {
            op2::<i32, bool, _>(rt, |a, b| a <= b)?;
        }

        Instruction::F32Le => {
            op2::<f32, bool, _>(rt, |a, b| a <= b)?;
        }

        Instruction::I64LeS => {
            op2::<i64, bool, _>(rt, |a, b| a <= b)?;
        }

        Instruction::F64Le => {
            op2::<f64, bool, _>(rt, |a, b| a <= b)?;
        }

        Instruction::I32LtU => {
            op2::<i32, bool, _>(rt, |a, b| a < b)?;
        }

        Instruction::I64LtU => {
            op2::<i64, bool, _>(rt, |a, b| a < b)?;
        }

        Instruction::I32Add => {
            op2::<i32, i32, _>(rt, i32::wrapping_add)?;
        }

        Instruction::F32Add => {
            op2::<f32, f32, _>(rt, ::std::ops::Add::add)?;
        }

        Instruction::I64Add => {
            op2::<i64, i64, _>(rt, i64::wrapping_add)?;
        }

        Instruction::F64Add => {
            op2::<f64, f64, _>(rt, ::std::ops::Add::add)?;
        }

        Instruction::I32Sub => {
            op2::<i32, i32, _>(rt, i32::wrapping_sub)?;
        }

        Instruction::F32Sub => {
            op2::<f32, f32, _>(rt, ::std::ops::Sub::sub)?;
        }

        Instruction::I64Sub => {
            op2::<i64, i64, _>(rt, i64::wrapping_sub)?;
        }

        Instruction::F64Sub => {
            op2::<f64, f64, _>(rt, ::std::ops::Sub::sub)?;
        }

        Instruction::I32Mul => {
            op2::<i32, i32, _>(rt, i32::wrapping_mul)?;
        }

        Instruction::F32Mul => {
            op2::<f32, f32, _>(rt, ::std::ops::Mul::mul)?;
        }

        Instruction::I64Mul => {
            op2::<i64, i64, _>(rt, i64::wrapping_mul)?;
        }

        Instruction::F64Mul => {
            op2::<f64, f64, _>(rt, ::std::ops::Mul::mul)?;
        }

        Instruction::I32DivS => {
            op2::<i32, i32, _>(rt, i32::wrapping_div)?;
        }

        Instruction::F32Div => {
            op2::<f32, f32, _>(rt, ::std::ops::Div::div)?;
        }

        Instruction::I64DivS => {
            op2::<i64, i64, _>(rt, i64::wrapping_div)?;
        }

        Instruction::F64Div => {
            op2::<f64, f64, _>(rt, ::std::ops::Div::div)?;
        }

        Instruction::I32DivU => {
            op2::<u32, u32, _>(rt, u32::wrapping_div)?;
        }

        Instruction::I64DivU => {
            op2::<u64, u64, _>(rt, u64::wrapping_div)?;
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
            op2::<u32, bool, _>(rt, |a, b| a > b)?;
        }

        Instruction::I64GtU => {
            op2::<u64, bool, _>(rt, |a, b| a > b)?;
        }

        Instruction::I32LtS => {
            op2::<i32, bool, _>(rt, |a, b| a < b)?;
        }

        Instruction::F32Lt => {
            op2::<f32, bool, _>(rt, |a, b| a < b)?;
        }

        Instruction::I64LtS => {
            op2::<i64, bool, _>(rt, |a, b| a < b)?;
        }

        Instruction::F64Lt => {
            op2::<f64, bool, _>(rt, |a, b| a < b)?;
        }

        Instruction::I32GtS => {
            op2::<i32, bool, _>(rt, |a, b| a > b)?;
        }

        Instruction::F32Gt => {
            op2::<f32, bool, _>(rt, |a, b| a > b)?;
        }

        Instruction::I64GtS => {
            op2::<i64, bool, _>(rt, |a, b| a > b)?;
        }

        Instruction::F64Gt => {
            op2::<f64, bool, _>(rt, |a, b| a > b)?;
        }

        Instruction::I32GeU => {
            op2::<u32, bool, _>(rt, |a, b| a >= b)?;
        }

        Instruction::I64GeU => {
            op2::<u64, bool, _>(rt, |a, b| a >= b)?;
        }

        Instruction::I32GeS => {
            op2::<i32, bool, _>(rt, |a, b| a >= b)?;
        }

        Instruction::F32Ge => {
            op2::<f32, bool, _>(rt, |a, b| a >= b)?;
        }

        Instruction::I64GeS => {
            op2::<i64, bool, _>(rt, |a, b| a >= b)?;
        }

        Instruction::F64Ge => {
            op2::<f64, bool, _>(rt, |a, b| a >= b)?;
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
            op2::<i32, i32, _>(rt, ::std::ops::BitOr::bitor)?;
        }

        Instruction::I64Or => {
            op2::<i64, i64, _>(rt, ::std::ops::BitOr::bitor)?;
        }

        Instruction::I32And => {
            op2::<i32, i32, _>(rt, ::std::ops::BitAnd::bitand)?;
        }

        Instruction::I64And => {
            op2::<i64, i64, _>(rt, ::std::ops::BitAnd::bitand)?;
        }

        Instruction::I32WrapI64 => {
            let i = rt.stack.pop_i64()?;
            rt.stack.push_i32((i % 2i64.pow(32)) as i32);
            rt.ip += 1;
        }

        Instruction::I32RemS => {
            op2::<i32, i32, _>(rt, |a, b| a.wrapping_rem(b))?;
        }

        Instruction::I64RemS => {
            op2::<i64, i64, _>(rt, |a, b| a.wrapping_rem(b))?;
        }

        Instruction::I32RemU => {
            op2::<u32, u32, _>(rt, |a, b| a.wrapping_rem(b))?;
        }

        Instruction::I64RemU => {
            op2::<u64, u64, _>(rt, |a, b| a.wrapping_rem(b))?;
        }

        Instruction::I32ShrU => {
            op2::<u32, u32, _>(rt, |a, b| a.wrapping_shr(b))?;
        }

        Instruction::I64ShrU => {
            op2::<u64, u64, _>(rt, |a, b| a.wrapping_shr(b as u32))?; // FIXME shift amount
        }

        Instruction::I32ShrS => {
            op2::<i32, i32, _>(rt, |a, b| a.wrapping_shr(b as u32))?;
        }

        Instruction::I64ShrS => {
            op2::<i64, i64, _>(rt, |a, b| a.wrapping_shr(b as u32))?; // FIXME shift amount
        }

        Instruction::I32Shl => {
            op2::<u32, u32, _>(rt, |a, b| a.wrapping_shl(b))?;
        }

        Instruction::I64Shl => {
            op2::<u32, u32, _>(rt, |a, b| a.wrapping_shl(b))?;
        }

        Instruction::I32Rotl => {
            op2::<u32, u32, _>(rt, |a, b| a.rotate_left(b))?;
        }

        Instruction::I64Rotl => {
            op2::<u64, u64, _>(rt, |a, b| a.rotate_left(b as u32))?; // FIXME shift amount
        }

        Instruction::I32Rotr => {
            op2::<u32, u32, _>(rt, |a, b| a.rotate_right(b))?;
        }

        Instruction::I64Rotr => {
            op2::<u64, u64, _>(rt, |a, b| a.rotate_right(b as u32))?; // FIXME shift amount
        }

        Instruction::I32Xor => {
            op2::<i32, i32, _>(rt, ::std::ops::BitXor::bitxor)?;
        }

        Instruction::I64Xor => {
            op2::<i64, i64, _>(rt, ::std::ops::BitXor::bitxor)?;
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
            op2::<f32, f32, _>(rt, f32::max)?;
        }

        Instruction::F64Max => {
            op2::<f64, f64, _>(rt, f64::max)?;
        }

        Instruction::F32Min => {
            op2::<f32, f32, _>(rt, f32::min)?;
        }

        Instruction::F64Min => {
            op2::<f64, f64, _>(rt, f64::min)?;
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
            rt.stack.push_i32(unsafe { transmute(f) });
            rt.ip += 1;
        }

        Instruction::I64ReinterpretF64 => {
            let f = rt.stack.pop_f64()?;
            rt.stack.push_i64(unsafe { transmute(f) });
            rt.ip += 1;
        }

        Instruction::F32ReinterpretI32 => {
            let f = rt.stack.pop_i32()?;
            rt.stack.push_f32(unsafe { transmute(f) });
            rt.ip += 1;
        }

        Instruction::F64ReinterpretI64 => {
            let f = rt.stack.pop_i64()?;
            rt.stack.push_f64(unsafe { transmute(f) });
            rt.ip += 1;
        }

        Instruction::F32Nearest => {
            let f = rt.stack.pop_f32()?;

            // NB. I don't understand this code, ported from reference interpreter

            let val = if f == 0.0f32 {
                f // preserve sign
            } else {
                let u = f.ceil();
                let d = f.floor();
                let um = (f - u).abs();
                let ud = (f - d).abs();
                let u_or_d = um < ud || (um == ud && (u / 2f32).floor() == u / 2f32);
                let f = if u_or_d { u } else { d };
                // TODO: canonicalize nan?
                f
            };

            rt.stack.push_f32(val);
            rt.ip += 1;
        }

        Instruction::F64Nearest => {
            let f = rt.stack.pop_f64()?;

            // NB. I don't understand this code, ported from reference interpreter

            let val = if f == 0.0f64 {
                f // preserve sign
            } else {
                let u = f.ceil();
                let d = f.floor();
                let um = (f - u).abs();
                let ud = (f - d).abs();
                let u_or_d = um < ud || (um == ud && (u / 2f64).floor() == u / 2f64);
                let f = if u_or_d { u } else { d };
                // TODO: canonicalize nan?
                f
            };

            rt.stack.push_f64(val);
            rt.ip += 1;
        }

        Instruction::F32Copysign => {
            op2::<f32, f32, _>(rt, Ieee754::copy_sign)?;
        }

        Instruction::F64Copysign => {
            op2::<f64, f64, _>(rt, Ieee754::copy_sign)?;
        }

        Instruction::I64ExtendUI32 => {
            let i = rt.stack.pop_i32()? as u32;
            rt.stack.push_i64((i as u64) as i64);
            rt.ip += 1;
        }

        Instruction::I64ExtendSI32 => {
            let i = rt.stack.pop_i32()?;
            rt.stack.push_i64(i as i64);
            rt.ip += 1;
        }

        Instruction::I32TruncUF32 => {
            let f = rt.stack.pop_f32()?;

            if f.is_nan() {
                return Err(ExecError::Trap);
            }

            if f >= (-(i32::MIN as f32) * 2f32) || f <= -1f32 {
                return Err(ExecError::Trap);
            }

            rt.stack.push_i32((f as i64) as i32);
            rt.ip += 1;

            // let trunc_f32_u x =
            //   if F32.ne x x then
            //     raise Numeric_error.InvalidConversionToInteger
            //   else
            //     let xf = F32.to_float x in
            //     if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0 then
            //       raise Numeric_error.IntegerOverflow
            //     else
            //       Int64.(to_int32 (of_float xf))
        }

        Instruction::I32TruncSF32 => {
            let f = rt.stack.pop_f32()?;

            if f.is_nan() {
                return Err(ExecError::Trap);
            }

            if f >= (-(i32::MIN as f32) * 2f32) || f <= (i32::MIN as f32) {
                return Err(ExecError::Trap);
            }

            rt.stack.push_i32(f as i32);
            rt.ip += 1;

            // let trunc_f32_s x =
            //   if F32.ne x x then
            //     raise Numeric_error.InvalidConversionToInteger
            //   else
            //     let xf = F32.to_float x in
            //     if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) then
            //       raise Numeric_error.IntegerOverflow
            //     else
            //       Int32.of_float xf
        }

        Instruction::I32TruncSF64 => {
            let f = rt.stack.pop_f64()?;

            if f.is_nan() {
                return Err(ExecError::Trap);
            }

            if f >= -(i32::MIN as f64) || f < (i32::MIN as f64 - 1f64) {
                return Err(ExecError::Trap);
            }

            rt.stack.push_i32(f as i32);
            rt.ip += 1;

            // let trunc_f64_s x =
            //   if F64.ne x x then
            //     raise Numeric_error.InvalidConversionToInteger
            //   else
            //     let xf = F64.to_float x in
            //     if xf >= -.Int32.(to_float min_int) || xf <= Int32.(to_float min_int) -. 1.0 then
            //       raise Numeric_error.IntegerOverflow
            //     else
            //       Int32.of_float xf
        }

        Instruction::I32TruncUF64 => {
            let f = rt.stack.pop_f64()?;

            if f.is_nan() {
                return Err(ExecError::Trap);
            }

            if f >= -(i32::MIN as f64) * 2f64 || f < -1f64 {
                return Err(ExecError::Trap);
            }

            rt.stack.push_i32((f as i64) as i32);
            rt.ip += 1;

            // let trunc_f64_u x =
            //   if F64.ne x x then
            //     raise Numeric_error.InvalidConversionToInteger
            //   else
            //     let xf = F64.to_float x in
            //     if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0 then
            //       raise Numeric_error.IntegerOverflow
            //     else
            //       Int64.(to_int32 (of_float xf))
        }

        Instruction::I64TruncSF32 => {
            let f = rt.stack.pop_f32()? as f64;

            if f.is_nan() {
                return Err(ExecError::Trap);
            }

            if f >= -(i64::MIN as f64) || f < (i64::MIN as f64) {
                return Err(ExecError::Trap);
            }

            rt.stack.push_i64(f as i64);
            rt.ip += 1;

            // let trunc_f32_s x =
            //   if F32.ne x x then
            //     raise Numeric_error.InvalidConversionToInteger
            //   else
            //     let xf = F32.to_float x in
            //     if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
            //       raise Numeric_error.IntegerOverflow
            //     else
            //       Int64.of_float xf
        }

        Instruction::I64TruncUF32 => {
            let f = rt.stack.pop_f32()? as f64;

            if f.is_nan() {
                return Err(ExecError::Trap);
            }

            if f >= -(i64::MIN as f64) * 2f64 || f < -1f64 {
                return Err(ExecError::Trap);
            }

            let val = if f >= -(i64::MIN as f64) {
                ((f - 10f64.powi(63)) as i64) ^ i64::MIN
            } else {
                f as i64
            };

            rt.stack.push_i64(val);
            rt.ip += 1;

            // let trunc_f32_u x =
            //   if F32.ne x x then
            //     raise Numeric_error.InvalidConversionToInteger
            //   else
            //     let xf = F32.to_float x in
            //     if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
            //       raise Numeric_error.IntegerOverflow
            //     else if xf >= -.Int64.(to_float min_int) then
            //       Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
            //     else
            //       Int64.of_float xf
        }

        Instruction::I64TruncSF64 => {
            let f = rt.stack.pop_f64()?;

            if f.is_nan() {
                return Err(ExecError::Trap);
            }

            if f >= -(i64::MIN as f64) || f < (i64::MIN as f64) {
                return Err(ExecError::Trap);
            }

            rt.stack.push_i64(f as i64);
            rt.ip += 1;

            // let trunc_f64_s x =
            //   if F64.ne x x then
            //     raise Numeric_error.InvalidConversionToInteger
            //   else
            //     let xf = F64.to_float x in
            //     if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
            //       raise Numeric_error.IntegerOverflow
            //     else
            //       Int64.of_float xf
        }

        Instruction::I64TruncUF64 => {
            let f = rt.stack.pop_f64()?;

            if f.is_nan() {
                return Err(ExecError::Trap);
            }

            if f >= -(i64::MIN as f64) * 2f64 || f <= -1f64 {
                return Err(ExecError::Trap);
            }

            let val = if f >= -(i64::MIN as f64) {
                ((f - 1f64.powi(63)) as i64) ^ i64::MIN
            } else {
                f as i64
            };

            rt.stack.push_i64(val);
            rt.ip += 1;

            // let trunc_f64_u x =
            //   if F64.ne x x then
            //     raise Numeric_error.InvalidConversionToInteger
            //   else
            //     let xf = F64.to_float x in
            //     if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
            //       raise Numeric_error.IntegerOverflow
            //     else if xf >= -.Int64.(to_float min_int) then
            //       Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
            //     else
            //       Int64.of_float xf
        }

        Instruction::F32ConvertSI32 => {
            let i = rt.stack.pop_i32()?;
            rt.stack.push_f32(i as f32);
            rt.ip += 1;

            // let convert_i32_s x =
            //   F32.of_float (Int32.to_float x)
        }

        Instruction::F32ConvertUI32 => {
            let i = rt.stack.pop_i32()?;

            let val = if i >= 0 {
                i as f32
            } else {
                (i.wrapping_shr(1) | (i & 0b1)) as f32 * 2f32
            };

            rt.stack.push_f32(val);
            rt.ip += 1;

            // let convert_i32_u x =
            //   F32.of_float Int32.(
            //     if x >= zero then to_float x else
            //     to_float (logor (shift_right_logical x 1) (logand x 1l)) *. 2.0
            //   )
        }

        Instruction::F32ConvertSI64 => {
            let i = rt.stack.pop_i64()?;
            let val = if i.abs() < 0x10_0000_0000_0000 {
                i as f32
            } else {
                let r = if i & 0xfff == 0 { 0 } else { 1 };

                (((i >> 12) | r) as f32) * 10f32.powi(12)
            };

            rt.stack.push_f32(val);
            rt.ip += 1;

            // let convert_i64_s x =
            //   F32.of_float Int64.(
            //     if abs x < 0x10_0000_0000_0000L then to_float x else
            //     let r = if logand x 0xfffL = 0L then 0L else 1L in
            //     to_float (logor (shift_right x 12) r) *. 0x1p12
            //   )
        }

        Instruction::F32ConvertUI64 => {
            let i = rt.stack.pop_i64()?;

            let val = if i < 0x10_0000_0000_0000 {
                i as f32
            } else {
                let r = if i & 0xfff == 0 { 0 } else { 1 };

                ((i >> 12) | r) as f32 * 10f32.powi(12)
            };

            rt.stack.push_f32(val);
            rt.ip += 1;

            // let convert_i64_u x =
            //   F32.of_float Int64.(
            //     if I64.lt_u x 0x10_0000_0000_0000L then to_float x else
            //     let r = if logand x 0xfffL = 0L then 0L else 1L in
            //     to_float (logor (shift_right_logical x 12) r) *. 0x1p12
            //   )
        }

        Instruction::F64ConvertSI32 => {
            let i = rt.stack.pop_i32()?;
            rt.stack.push_f64(i as f64);
            rt.ip += 1;
        }

        Instruction::F64ConvertSI64 => {
            let i = rt.stack.pop_i64()?;
            rt.stack.push_f64(i as f64);
            rt.ip += 1;
        }

        Instruction::F64ConvertUI32 => {
            let i = rt.stack.pop_i32()?;
            rt.stack
                .push_f64(((i as i64) & 0x0000_0000_ffff_ffff) as f64);
            rt.ip += 1;

            // let convert_i32_u x =
            //   F64.of_float Int64.(to_float (logand (of_int32 x) 0x0000_0000_ffff_ffffL))
        }

        Instruction::F64ConvertUI64 => {
            let i = rt.stack.pop_i64()?;
            let val = if i > 0 {
                i as f64
            } else {
                (((i >> 1) | (i & 1)) as f64) * 2f64
            };
            rt.stack.push_f64(val);
            rt.ip += 1;

            // let convert_i64_u x =
            //   F64.of_float Int64.(
            //     if x >= zero then to_float x else
            //     to_float (logor (shift_right_logical x 1) (logand x 1L)) *. 2.0
            //   )
        }

        Instruction::F32DemoteF64 => {
            let f = rt.stack.pop_f64()?;

            let val = if f.is_nan() {
                f as f32
            } else {
                let bits: u64 = unsafe { transmute(f) };
                let sign_field = (bits >> 63) << 31;
                let signi_field = (bits << 12) >> 41;
                let fields = sign_field | signi_field;
                let bits_32: u32 = 0x7fc0_000 | (fields as u32);
                let f_32: f32 = unsafe { transmute(bits_32) };
                f_32
            };

            rt.stack.push_f32(val);
            rt.ip += 1;

            // let demote_f64 x =
            //   let xf = F64.to_float x in
            //   if xf = xf then F32.of_float xf else
            //   let nan64bits = F64.to_bits x in
            //   let sign_field = Int64.(shift_left (shift_right_logical nan64bits 63) 31) in
            //   let significand_field = Int64.(shift_right_logical (shift_left nan64bits 12) 41) in
            //   let fields = Int64.logor sign_field significand_field in
            //   let nan32bits = Int32.logor 0x7fc0_0000l (I32_convert.wrap_i64 fields) in
            //   F32.of_bits nan32bits
        }

        Instruction::F64PromoteF32 => {
            let f = rt.stack.pop_f32()?;

            let val = if f.is_nan() {
                f as f64
            } else {
                let bits_u32: u32 = unsafe { transmute(f) };
                let bits_u64: u64 = bits_u32 as u64;
                let sign_field = (bits_u64 >> 31) << 63;
                let signi_field = (bits_u64 << 41) >> 12;
                let fields = sign_field | signi_field;
                let bits_64 = 0x7ff8_0000_0000_0000 | fields;
                let f_64: f64 = unsafe { transmute(bits_64) };
                f_64
            };

            rt.stack.push_f64(val);
            rt.ip += 1;

            // let promote_f32 x =
            //   let xf = F32.to_float x in
            //   if xf = xf then F64.of_float xf else
            //   let nan32bits = I64_convert.extend_i32_u (F32.to_bits x) in
            //   let sign_field = Int64.(shift_left (shift_right_logical nan32bits 31) 63) in
            //   let significand_field = Int64.(shift_right_logical (shift_left nan32bits 41) 12) in
            //   let fields = Int64.logor sign_field significand_field in
            //   let nan64bits = Int64.logor 0x7ff8_0000_0000_0000L fields in
            //   F64.of_bits nan64bits
        }

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

        Instruction::Atomics(_)
        | Instruction::Simd(_)
        | Instruction::SignExt(_)
        | Instruction::Bulk(_) => {
            return Err(ExecError::Panic(format!(
                "Instruction not implemented: {:?}",
                instr
            )));
        }
    }

    Ok(())
}

fn op2<A: StackValue, B: StackValue, F: Fn(A, A) -> B>(rt: &mut Runtime, op: F) -> Result<()> {
    let val2 = A::pop(&mut rt.stack)?;
    let val1 = A::pop(&mut rt.stack)?;
    let ret = op(val1, val2);
    ret.push(&mut rt.stack);
    rt.ip += 1;
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
