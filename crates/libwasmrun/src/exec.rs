use crate::frame::FrameStack;
use crate::mem::Mem;
use crate::spectest;
use crate::stack::{Block, BlockKind, EndOrBreak, Stack, StackValue};
use crate::store::{Func, Global, ModuleIdx, Store};
pub use crate::value::Value;
use crate::{ExecError, Result};

use fxhash::FxHashMap;
use ieee754::Ieee754;
use parity_wasm::elements as wasm;
use wasm::{Instruction, SignExtInstruction};

use std::mem::{replace, transmute};
use std::rc::Rc;

type Addr = u32;

type FuncIdx = u32;

pub const PAGE_SIZE: usize = 65536;
pub const MAX_PAGES: u32 = 65536; // (2**32 - 1 / PAGE_SIZE), or 0x10000

#[derive(Debug, Default)]
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

impl Module {
    fn add_fun(&mut self, fun_addr: u32) -> u32 {
        let ret = self.func_addrs.len();
        self.func_addrs.push(fun_addr);
        ret as u32
    }

    fn add_table(&mut self, table_addr: u32) -> u32 {
        let ret = self.table_addrs.len();
        self.table_addrs.push(table_addr);
        ret as u32
    }

    fn add_mem(&mut self, mem_addr: u32) -> u32 {
        let ret = self.mem_addrs.len();
        self.mem_addrs.push(mem_addr);
        ret as u32
    }

    fn add_global(&mut self, global_addr: u32) -> u32 {
        let ret = self.global_addrs.len();
        self.global_addrs.push(global_addr);
        ret as u32
    }

    fn add_type(&mut self, ty: wasm::FunctionType) -> u32 {
        let ret = self.types.len();
        self.types.push(ty);
        ret as u32
    }

    fn get_exported_fun(&self, fun: &str) -> Option<Addr> {
        for export in &self.exports {
            if export.field() == fun {
                if let wasm::Internal::Function(fun_idx) = export.internal() {
                    return Some(self.func_addrs[*fun_idx as usize]);
                }
            }
        }
        None
    }

    fn get_exported_table(&self, table: &str) -> Option<Addr> {
        for export in &self.exports {
            if export.field() == table {
                if let wasm::Internal::Table(tbl_idx) = export.internal() {
                    return Some(self.table_addrs[*tbl_idx as usize]);
                }
            }
        }
        None
    }

    fn get_exported_mem(&self, mem: &str) -> Option<Addr> {
        for export in &self.exports {
            if export.field() == mem {
                if let wasm::Internal::Memory(mem_idx) = export.internal() {
                    return Some(self.mem_addrs[*mem_idx as usize]);
                }
            }
        }
        None
    }

    fn get_exported_global(&self, global: &str) -> Option<Addr> {
        for export in &self.exports {
            if export.field() == global {
                if let wasm::Internal::Global(global_idx) = export.internal() {
                    return Some(self.global_addrs[*global_idx as usize]);
                }
            }
        }
        None
    }
}

pub struct Runtime {
    /// The heap
    store: Store,
    /// Value stack
    pub stack: Stack,
    /// Call stack
    pub frames: FrameStack,
    /// Allocated modules
    pub modules: Vec<Module>,
    /// Maps registered modules to their indices in `modules`
    module_names: FxHashMap<String, usize>,
    /// Instruction pointer
    ip: u32,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            store: Default::default(),
            stack: Default::default(),
            frames: Default::default(),
            modules: Default::default(),
            module_names: Default::default(),
            ip: Default::default(),
        }
    }

    pub fn new_test() -> Self {
        let mut rt = Runtime::new();
        allocate_spectest(&mut rt);
        rt
    }

    pub fn clear_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
    }

    pub fn get_module(&self, idx: ModuleIdx) -> &Module {
        &self.modules[idx]
    }

    pub fn get_module_start(&self, idx: ModuleIdx) -> Option<FuncIdx> {
        self.modules[idx].start
    }

    pub fn pop_value(&mut self) -> Option<Value> {
        self.stack.pop_value_opt().unwrap()
    }

    pub fn push_value(&mut self, value: Value) {
        self.stack.push_value(value).unwrap()
    }

    pub fn register_module(&mut self, name: String, module_addr: usize) {
        self.module_names.insert(name, module_addr);
    }

    pub fn get_global(&mut self, module_idx: ModuleIdx, name: &str) -> Option<Value> {
        let global_addr = self.modules[module_idx].get_exported_global(name)?;
        Some(self.store.globals[global_addr as usize].value)
    }
}

pub fn allocate_spectest(rt: &mut Runtime) {
    // https://github.com/WebAssembly/spec/blob/7526564b56c30250b66504fe795e9c1e88a938af/interpreter/host/spectest.ml

    let module_idx = rt.modules.len();
    let mut module: Module = Default::default();

    let table_addr = rt.store.allocate_table(vec![None; 10]);
    let table_idx = module.add_table(table_addr);
    module.exports.push(wasm::ExportEntry::new(
        "table".to_string(),
        wasm::Internal::Table(table_idx),
    ));

    let mem_addr = rt.store.allocate_mem(Mem::new(1, Some(2)));
    let mem_idx = module.add_mem(mem_addr);
    module.exports.push(wasm::ExportEntry::new(
        "memory".to_string(),
        wasm::Internal::Memory(mem_idx),
    ));

    let global_i32_addr = rt.store.allocate_global(Global {
        value: Value::I32(666),
        mutable: false,
    });
    let global_i32_idx = module.add_global(global_i32_addr);
    module.exports.push(wasm::ExportEntry::new(
        "global_i32".to_string(),
        wasm::Internal::Global(global_i32_idx),
    ));

    let global_i64_addr = rt.store.allocate_global(Global {
        value: Value::I64(666),
        mutable: false,
    });
    let global_i64_idx = module.add_global(global_i64_addr);
    module.exports.push(wasm::ExportEntry::new(
        "global_i64".to_string(),
        wasm::Internal::Global(global_i64_idx),
    ));

    let global_f32_addr = rt.store.allocate_global(Global {
        value: Value::F32(666.6f32),
        mutable: false,
    });
    let global_f32_idx = module.add_global(global_f32_addr);
    module.exports.push(wasm::ExportEntry::new(
        "global_f32".to_string(),
        wasm::Internal::Global(global_f32_idx),
    ));

    let global_f64_addr = rt.store.allocate_global(Global {
        value: Value::F64(666.6f64),
        mutable: false,
    });
    let global_f64_idx = module.add_global(global_f64_addr);
    module.exports.push(wasm::ExportEntry::new(
        "global_f64".to_string(),
        wasm::Internal::Global(global_f64_idx),
    ));

    let print_ty = module.add_type(wasm::FunctionType::new(vec![], vec![]));
    let print_addr = rt
        .store
        .allocate_host_fun(module_idx, print_ty, Rc::new(spectest::print));
    let print_idx = module.add_fun(print_addr as u32);
    module.exports.push(wasm::ExportEntry::new(
        "print".to_string(),
        wasm::Internal::Function(print_idx),
    ));

    let print_i32_ty = module.add_type(wasm::FunctionType::new(vec![wasm::ValueType::I32], vec![]));
    let print_i32_addr =
        rt.store
            .allocate_host_fun(module_idx, print_i32_ty, Rc::new(spectest::print_i32));
    let print_i32_idx = module.add_fun(print_i32_addr as u32);
    module.exports.push(wasm::ExportEntry::new(
        "print_i32".to_string(),
        wasm::Internal::Function(print_i32_idx),
    ));

    let print_i32_f32_ty = module.add_type(wasm::FunctionType::new(
        vec![wasm::ValueType::I32, wasm::ValueType::F32],
        vec![],
    ));
    let print_i32_f32_addr = rt.store.allocate_host_fun(
        module_idx,
        print_i32_f32_ty,
        Rc::new(spectest::print_i32_f32),
    );
    let print_i32_f32_idx = module.add_fun(print_i32_f32_addr as u32);
    module.exports.push(wasm::ExportEntry::new(
        "print_i32_f32".to_string(),
        wasm::Internal::Function(print_i32_f32_idx),
    ));

    let print_f64_f64_ty = module.add_type(wasm::FunctionType::new(
        vec![wasm::ValueType::F64, wasm::ValueType::F64],
        vec![],
    ));
    let print_f64_f64_addr = rt.store.allocate_host_fun(
        module_idx,
        print_f64_f64_ty,
        Rc::new(spectest::print_f64_f64),
    );
    let print_f64_f64_idx = module.add_fun(print_f64_f64_addr as u32);
    module.exports.push(wasm::ExportEntry::new(
        "print_f64_f64".to_string(),
        wasm::Internal::Function(print_f64_f64_idx),
    ));

    let print_f32_ty = module.add_type(wasm::FunctionType::new(vec![wasm::ValueType::F32], vec![]));
    let print_f32_addr =
        rt.store
            .allocate_host_fun(module_idx, print_f32_ty, Rc::new(spectest::print_f32));
    let print_f32_idx = module.add_fun(print_f32_addr as u32);
    module.exports.push(wasm::ExportEntry::new(
        "print_f32".to_string(),
        wasm::Internal::Function(print_f32_idx),
    ));

    let print_f64_ty = module.add_type(wasm::FunctionType::new(vec![wasm::ValueType::F64], vec![]));
    let print_f64_addr =
        rt.store
            .allocate_host_fun(module_idx, print_f64_ty, Rc::new(spectest::print_f64));
    let print_f64_idx = module.add_fun(print_f64_addr as u32);
    module.exports.push(wasm::ExportEntry::new(
        "print_f64".to_string(),
        wasm::Internal::Function(print_f64_idx),
    ));

    rt.modules.push(module);
    rt.module_names.insert("spectest".to_string(), module_idx);
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

    // Add imported stuff to the module
    if let Some(import_section) = parsed_module.import_section_mut() {
        for import in import_section.entries_mut().drain(..) {
            let module_name = import.module();
            let field_name = import.field();

            let module_idx = match rt.module_names.get(module_name) {
                Some(module_idx) => *module_idx,
                None => {
                    return Err(ExecError::Panic(format!(
                        "Can't find imported module {:?}",
                        module_name
                    )));
                }
            };
            let imported_module = &rt.modules[module_idx];

            match import.external() {
                wasm::External::Function(_fun_ty_idx) => {
                    let fun_addr = imported_module.get_exported_fun(field_name).unwrap();
                    inst.func_addrs.push(fun_addr);
                }
                wasm::External::Table(_tbl_ty) => {
                    // FIXME: should we copy the table or share it?
                    // Copy the table:
                    // let tbl_addr = imported_module.get_exported_table(field_name).unwrap();
                    // let tbl = rt.store.tables[tbl_addr as usize].clone();
                    // let tbl_addr = rt.store.allocate_table(tbl);
                    // inst.table_addrs.push(tbl_addr);

                    // Share the table:
                    let tbl_addr = imported_module.get_exported_table(field_name).unwrap();
                    inst.table_addrs.push(tbl_addr);
                }
                wasm::External::Memory(_mem_ty) => {
                    let mem_addr = imported_module.get_exported_mem(field_name).unwrap();
                    inst.mem_addrs.push(mem_addr);
                }
                wasm::External::Global(_gbl_ty) => {
                    let global_addr = imported_module.get_exported_global(field_name).unwrap();
                    inst.global_addrs.push(global_addr);
                }
            }
        }
    }

    // Allocate functions
    if let Some(code_section) = parsed_module.code_section_mut() {
        for (fun_idx, fun) in replace(code_section.bodies_mut(), vec![])
            .into_iter()
            .enumerate()
        {
            let fun_addr = rt.store.funcs.len();

            let function_section = parsed_module.function_section().ok_or_else(|| {
                ExecError::Panic("Module has a code section but no function section".to_string())
            })?;

            rt.store.allocate_fun(
                module_idx,
                function_section.entries()[fun_idx].type_ref(),
                fun_addr as u32,
                fun,
            )?;
            inst.func_addrs.push(fun_addr as u32);
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
                .map(|init_expr| get_const_expr_val(rt, &inst, init_expr.code()));

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
                if elem_idx >= table.len() {
                    table.resize(elem_idx + 1, None);
                }
                *table.get_mut(elem_idx).ok_or_else(|| {
                    ExecError::Panic(format!("Elem index out of bounds: {}", elem_idx))
                })? = Some(inst.func_addrs[elem as usize]);
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
            let value = get_const_expr_val(rt, &inst, global.init_expr().code())?;
            rt.store.globals.push(Global {
                value,
                mutable: global.global_type().is_mutable(),
            });
            inst.global_addrs.push(global_idx as u32);
        }
    }

    // Get names of functions
    // NB. This is not used for calling functions as we can only call exported functions, which
    // already have names in the export section.
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
            let mem_index: u32 = data.index();
            let offset = match data.offset() {
                None => 0,
                Some(offset_expr) => match get_const_expr_val(rt, &inst, offset_expr.code())? {
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

            let mem_addr = inst.mem_addrs[mem_index as usize];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.set_range(offset, values)?;
        }
    }

    // Initialize exports
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

    // Set start
    inst.start = parsed_module.start_section();
    let start = inst.start;

    // Done
    rt.modules.push(inst);

    if let Some(start_idx) = start {
        invoke(rt, module_idx, start_idx)?;
        finish(rt)?;
    }

    Ok(module_idx)
}

fn get_const_expr_val(rt: &Runtime, module: &Module, instrs: &[Instruction]) -> Result<Value> {
    use Instruction::*;
    match instrs {
        [I32Const(value), End] => Ok(Value::I32(*value)),
        [I64Const(value), End] => Ok(Value::I64(*value)),
        [F32Const(value), End] => Ok(Value::F32(unsafe { transmute(*value) })),
        [F64Const(value), End] => Ok(Value::F64(unsafe { transmute(*value) })),
        [GetGlobal(idx), End] => {
            Ok(rt.store.globals[module.global_addrs[*idx as usize] as usize].value)
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
    invoke_direct(rt, fun_addr)
}

fn invoke_direct(rt: &mut Runtime, fun_addr: u32) -> Result<()> {
    let func =
        match rt.store.funcs.get(fun_addr as usize).ok_or_else(|| {
            ExecError::Panic(format!("Function address out of bounds: {}", fun_addr))
        })? {
            Func::Wasm(fun) => fun,
            Func::Host(fun) => {
                let host_func = fun.fun.clone();
                host_func(rt);
                rt.ip += 1;
                return Ok(());
            }
        };

    debug_assert!(match func.fun.code().elements().last().unwrap() {
        Instruction::End => true,
        other => panic!("Last instruction of function is not 'end': {:?}", other),
    });

    let fun_ty = &rt.modules[func.module_idx].types[func.ty_idx as usize];
    let arg_tys = fun_ty.params();
    rt.frames.push(func, arg_tys);

    // Set locals for arguments
    let n_args = fun_ty.params().len();
    let n_rets = fun_ty.results().len();

    for local_idx in (0..n_args).rev() {
        let arg_val = rt.stack.pop_value()?;
        rt.frames
            .current_mut()?
            .set_local(local_idx as u32, arg_val)?;
    }

    rt.stack
        .push_fun_block(rt.ip + 1, n_args as u32, n_rets as u32);
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
    let current_fun_addr = match rt.frames.current() {
        Ok(current_fun) => current_fun.fun_addr,
        Err(_) => {
            return Ok(());
        }
    };
    let current_fun = match &rt.store.funcs[current_fun_addr as usize] {
        Func::Wasm(fun) => fun,
        Func::Host { .. } => {
            return Err(ExecError::Panic("single_step: host function".to_string()));
        }
    };

    assert!((rt.ip as usize) < current_fun.fun.code().elements().len());

    // if rt.ip as usize >= current_fun.fun.code().elements().len() {
    //     // TODO: Why is this branch taken? It think all functions end with an `end`?
    //     // TODO: Should this really take one step?
    //     return Ok(());
    // }

    // Instruction is just 3 words so clonning here should be fine, and avoid borrowchk issues
    // later on
    let instr = current_fun.fun.code().elements()[rt.ip as usize].clone();
    let module_idx = current_fun.module_idx;

    // println!(
    //     "ip={}, instruction={:?}, stack={:?}, call stack={:?}",
    //     rt.ip, instr, rt.stack, rt.frames,
    // );

    match instr {
        Instruction::GrowMemory(mem_ref) => {
            assert_eq!(mem_ref, 0);

            let mem_addr = rt.modules[module_idx as usize].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];

            let current_pages = mem.size_pages();
            let new_pages = rt.stack.pop_i32()? as u32;
            let total_pages = current_pages + new_pages;

            if total_pages > mem.max_pages().unwrap_or(MAX_PAGES) {
                rt.stack.push_i32(-1)?;
            } else {
                mem.add_pages(new_pages);
                rt.stack.push_i32(current_pages as i32)?;
            }

            rt.ip += 1;
        }

        Instruction::CurrentMemory(mem_ref) => {
            // memory.size
            assert_eq!(mem_ref, 0);
            let mem = &mut rt.store.mems[module_idx];
            rt.stack.push_i32(mem.size_pages() as i32)?;
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
                rt.stack.push_value(val2)?;
            } else {
                rt.stack.push_value(val1)?;
            }
            rt.ip += 1;
        }

        Instruction::I32Store(_align, offset) => {
            let value = rt.stack.pop_i32()?;
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 4)?;

            let [b1, b2, b3, b4] = value.to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 3] = b4;

            rt.ip += 1;
        }

        Instruction::F32Store(_align, offset) => {
            let value = rt.stack.pop_f32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 4)?;

            let [b1, b2, b3, b4] = value.to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 3] = b4;

            rt.ip += 1;
        }

        Instruction::I64Store(_align, offset) => {
            let value = rt.stack.pop_i64()?;
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 8)?;

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

        Instruction::F64Store(_align, offset) => {
            let value = rt.stack.pop_f64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 8)?;

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

        Instruction::I64Store8(_align, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 1)?;

            let val = c as u8;
            mem[addr] = val;

            rt.ip += 1;
        }

        Instruction::I64Store16(_align, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 2)?;

            let [b1, b2] = (c as u16).to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;

            rt.ip += 1;
        }

        Instruction::I64Store32(_align, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 4)?;

            let [b1, b2, b3, b4] = (c as u32).to_le_bytes();

            mem[addr] = b1;
            mem[addr + 1] = b2;
            mem[addr + 2] = b3;
            mem[addr + 3] = b4;

            rt.ip += 1;
        }

        Instruction::I64Load8S(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 1)?;

            let val = mem[addr];
            rt.stack.push_i64(((val as i64) << 56) >> 56)?;

            rt.ip += 1;
        }

        Instruction::I32Load(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 4)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            rt.stack.push_i32(i32::from_le_bytes([b1, b2, b3, b4]))?;
            rt.ip += 1;
        }

        Instruction::F32Load(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 4)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            rt.stack.push_f32(f32::from_le_bytes([b1, b2, b3, b4]))?;
            rt.ip += 1;
        }

        Instruction::I64Load(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 8)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            let b5 = mem[addr + 4];
            let b6 = mem[addr + 5];
            let b7 = mem[addr + 6];
            let b8 = mem[addr + 7];
            rt.stack
                .push_i64(i64::from_le_bytes([b1, b2, b3, b4, b5, b6, b7, b8]))?;
            rt.ip += 1;
        }

        Instruction::F64Load(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 8)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            let b5 = mem[addr + 4];
            let b6 = mem[addr + 5];
            let b7 = mem[addr + 6];
            let b8 = mem[addr + 7];
            rt.stack
                .push_f64(f64::from_le_bytes([b1, b2, b3, b4, b5, b6, b7, b8]))?;
            rt.ip += 1;
        }

        Instruction::I32Load8U(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 1)?;

            let b = mem[addr];
            rt.stack.push_i32(i32::from_le_bytes([b, 0, 0, 0]))?;
            rt.ip += 1;
        }

        Instruction::I32Load8S(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 1)?;

            let b = mem[addr];
            let val = i8::from_le_bytes([b]) as i32;
            rt.stack.push_i32(val)?;
            rt.ip += 1;
        }

        Instruction::I64Load8U(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 1)?;

            let b = mem[addr];
            rt.stack
                .push_i64(i64::from_le_bytes([b, 0, 0, 0, 0, 0, 0, 0]))?;
            rt.ip += 1;
        }

        Instruction::I32Load16U(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 2)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            rt.stack.push_i32(i32::from_le_bytes([b1, b2, 0, 0]))?;
            rt.ip += 1;
        }

        Instruction::I32Load16S(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 2)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];

            rt.stack.push_i32(i16::from_le_bytes([b1, b2]) as i32)?;
            rt.ip += 1;
        }

        Instruction::I64Load16U(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 2)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            rt.stack
                .push_i64(i64::from_le_bytes([b1, b2, 0, 0, 0, 0, 0, 0]))?;
            rt.ip += 1;
        }

        Instruction::I64Load16S(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 2)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];

            rt.stack.push_i64(i16::from_le_bytes([b1, b2]) as i64)?;
            rt.ip += 1;
        }

        Instruction::I64Load32U(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 4)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];
            rt.stack
                .push_i64(i64::from_le_bytes([b1, b2, b3, b4, 0, 0, 0, 0]))?;
            rt.ip += 1;
        }

        Instruction::I64Load32S(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 4)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            let b3 = mem[addr + 2];
            let b4 = mem[addr + 3];

            rt.stack
                .push_i64(i32::from_le_bytes([b1, b2, b3, b4]) as i64)?;
            rt.ip += 1;
        }

        Instruction::I32Store8(_align, offset) => {
            let c = rt.stack.pop_i32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 1)?;

            mem[addr] = c as u8;
            rt.ip += 1;
        }

        Instruction::I32Store16(_align, offset) => {
            let c = rt.stack.pop_i32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.modules[module_idx].mem_addrs[0];
            let mem = &mut rt.store.mems[mem_addr as usize];
            mem.check_range(addr, 2)?;

            let [b1, b2] = (c as u16).to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;
            rt.ip += 1;
        }

        Instruction::GetLocal(idx) => {
            let val = rt.frames.current()?.get_local(idx)?;
            rt.stack.push_value(val)?;
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
            rt.stack.push_value(val)?;
            rt.ip += 1;
        }

        Instruction::GetGlobal(idx) => {
            let global_idx = rt.modules[module_idx].global_addrs[idx as usize];
            let value = rt.store.globals[global_idx as usize].value;
            rt.stack.push_value(value)?;
            rt.ip += 1;
        }

        Instruction::SetGlobal(idx) => {
            let global_idx = rt.modules[module_idx].global_addrs[idx as usize];
            let value = rt.stack.pop_value()?;
            rt.store.globals[global_idx as usize].value = value;
            rt.ip += 1;
        }

        Instruction::I32Const(i) => {
            rt.stack.push_i32(i)?;
            rt.ip += 1;
        }

        Instruction::I64Const(i) => {
            rt.stack.push_i64(i)?;
            rt.ip += 1;
        }

        Instruction::F32Const(f) => {
            rt.stack.push_f32(unsafe { transmute(f) })?;
            rt.ip += 1;
        }

        Instruction::F64Const(f) => {
            rt.stack.push_f64(unsafe { transmute(f) })?;
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
            op1::<i32, bool, _>(rt, |i| i == 0)?;
        }

        Instruction::I64Eqz => {
            op1::<i64, bool, _>(rt, |i| i == 0)?;
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
            op2::<u32, bool, _>(rt, |a, b| a < b)?;
        }

        Instruction::I64LtU => {
            op2::<u64, bool, _>(rt, |a, b| a < b)?;
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
            op2_trap::<i32, i32, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap)
                } else {
                    i32::checked_div(a, b).ok_or(ExecError::Trap)
                }
            })?;
        }

        Instruction::F32Div => {
            op2::<f32, f32, _>(rt, ::std::ops::Div::div)?;
        }

        Instruction::I64DivS => {
            op2_trap::<i64, i64, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap)
                } else {
                    i64::checked_div(a, b).ok_or(ExecError::Trap)
                }
            })?;
        }

        Instruction::F64Div => {
            op2::<f64, f64, _>(rt, ::std::ops::Div::div)?;
        }

        Instruction::I32DivU => {
            op2_trap::<u32, u32, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap)
                } else {
                    Ok(u32::wrapping_div(a, b))
                }
            })?;
        }

        Instruction::I64DivU => {
            op2_trap::<u64, u64, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap)
                } else {
                    Ok(u64::wrapping_div(a, b))
                }
            })?;
        }

        Instruction::I32Ctz => {
            op1::<i32, i32, _>(rt, |i| i.trailing_zeros() as i32)?;
        }

        Instruction::I64Ctz => {
            op1::<i64, i64, _>(rt, |i| i.trailing_zeros() as i64)?;
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
            op1::<i32, i32, _>(rt, |i| unsafe { ::core::arch::x86_64::_popcnt32(i) })?;
        }

        Instruction::I64Popcnt => {
            op1::<i64, i64, _>(rt, |i| unsafe { ::core::arch::x86_64::_popcnt64(i) as i64 })?;
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
            op1::<i64, i32, _>(rt, |i| (i % 2i64.pow(32)) as i32)?;
        }

        Instruction::I32RemS => {
            op2_trap::<i32, i32, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap)
                } else {
                    Ok(a.wrapping_rem(b))
                }
            })?;
        }

        Instruction::I64RemS => {
            op2_trap::<i64, i64, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap)
                } else {
                    Ok(a.wrapping_rem(b))
                }
            })?;
        }

        Instruction::I32RemU => {
            op2_trap::<u32, u32, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap)
                } else {
                    Ok(a.wrapping_rem(b))
                }
            })?;
        }

        Instruction::I64RemU => {
            op2_trap::<u64, u64, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap)
                } else {
                    Ok(a.wrapping_rem(b))
                }
            })?;
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
            op2::<u64, u64, _>(rt, |a, b| a.wrapping_shl(b as u32))?; // FIXME shift amount
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
            op1::<i32, i32, _>(rt, |i| i.leading_zeros() as i32)?;
        }

        Instruction::I64Clz => {
            op1::<i64, i64, _>(rt, |i| i.leading_zeros() as i64)?;
        }

        Instruction::F32Max => {
            op2::<f32, f32, _>(rt, |a, b| {
                if a == b {
                    f32::from_bits(a.to_bits() | b.to_bits())
                } else if a > b {
                    a
                } else if a < b {
                    b
                } else {
                    f32::NAN // TODO: canonicalize?
                }
            })?;

            // let max x y =
            //   let xf = to_float x in
            //   let yf = to_float y in
            //   (* max -0 0 is 0 *)
            //   if xf = yf then Rep.logand x y else
            //   if xf > yf then x else
            //   if xf < yf then y else
            //   determine_binary_nan x y
        }

        Instruction::F64Max => {
            op2::<f64, f64, _>(rt, |a, b| {
                if a == b {
                    f64::from_bits(a.to_bits() | b.to_bits())
                } else if a > b {
                    a
                } else if a < b {
                    b
                } else {
                    f64::NAN // TODO: canonicalize?
                }
            })?;
        }

        Instruction::F32Min => {
            op2::<f32, f32, _>(rt, |a, b| {
                if a == b {
                    f32::from_bits(a.to_bits() | b.to_bits())
                } else if a < b {
                    a
                } else if a > b {
                    b
                } else {
                    f32::NAN // TODO: canonicalize?
                }
            })?;

            //   let min x y =
            //     let xf = to_float x in
            //     let yf = to_float y in
            //     (* min -0 0 is -0 *)
            //     if xf = yf then Rep.logor x y else
            //     if xf < yf then x else
            //     if xf > yf then y else
            //     determine_binary_nan x y
        }

        Instruction::F64Min => {
            op2::<f64, f64, _>(rt, |a, b| {
                if a == b {
                    f64::from_bits(a.to_bits() | b.to_bits())
                } else if a < b {
                    a
                } else if a > b {
                    b
                } else {
                    f64::NAN // TODO: canonicalize?
                }
            })?;
        }

        Instruction::F32Neg => {
            op1::<f32, f32, _>(rt, |f| {
                let (sign, exp, signi) = f.decompose();
                f32::recompose(!sign, exp, signi)
            })?;
        }

        Instruction::F64Neg => {
            op1::<f64, f64, _>(rt, |f| {
                let (sign, exp, signi) = f.decompose();
                f64::recompose(!sign, exp, signi)
            })?;
        }

        Instruction::F32Sqrt => {
            op1::<f32, f32, _>(rt, f32::sqrt)?;
        }

        Instruction::F64Sqrt => {
            op1::<f64, f64, _>(rt, f64::sqrt)?;
        }

        Instruction::F32Abs => {
            op1::<f32, f32, _>(rt, |f| {
                let (_, exp, signi) = f.decompose();
                f32::recompose(false, exp, signi)
            })?;
        }

        Instruction::F64Abs => {
            op1::<f64, f64, _>(rt, |f| {
                let (_, exp, signi) = f.decompose();
                f64::recompose(false, exp, signi)
            })?;
        }

        Instruction::F32Ceil => {
            op1::<f32, f32, _>(rt, f32::ceil)?;
        }

        Instruction::F64Ceil => {
            op1::<f64, f64, _>(rt, f64::ceil)?;
        }

        Instruction::F32Floor => {
            op1::<f32, f32, _>(rt, f32::floor)?;
        }

        Instruction::F64Floor => {
            op1::<f64, f64, _>(rt, f64::floor)?;
        }

        Instruction::F32Trunc => {
            op1::<f32, f32, _>(rt, f32::trunc)?;
        }

        Instruction::F64Trunc => {
            op1::<f64, f64, _>(rt, f64::trunc)?;
        }

        Instruction::I32ReinterpretF32 => {
            op1::<f32, i32, _>(rt, |f| unsafe { transmute(f) })?;
        }

        Instruction::I64ReinterpretF64 => {
            op1::<f64, i64, _>(rt, |f| unsafe { transmute(f) })?;
        }

        Instruction::F32ReinterpretI32 => {
            op1::<i32, f32, _>(rt, |f| unsafe { transmute(f) })?;
        }

        Instruction::F64ReinterpretI64 => {
            op1::<i64, f64, _>(rt, |f| unsafe { transmute(f) })?;
        }

        Instruction::F32Nearest => {
            op1::<f32, f32, _>(rt, |f|
                // NB. I don't understand this code, ported from reference interpreter
                if f == 0.0f32 {
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
                })?;
        }

        Instruction::F64Nearest => {
            op1::<f64, f64, _>(rt, |f|
                // NB. I don't understand this code, ported from reference interpreter
                if f == 0.0f64 {
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
                })?;
        }

        Instruction::F32Copysign => {
            op2::<f32, f32, _>(rt, Ieee754::copy_sign)?;
        }

        Instruction::F64Copysign => {
            op2::<f64, f64, _>(rt, Ieee754::copy_sign)?;
        }

        Instruction::I64ExtendUI32 => {
            // FIXME: Same, can't use op1
            let i = rt.stack.pop_i32()? as u32;
            rt.stack.push_i64((i as u64) as i64)?;
            rt.ip += 1;
        }

        Instruction::I64ExtendSI32 => {
            let i = rt.stack.pop_i32()?;
            rt.stack.push_i64(i as i64)?;
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

            rt.stack.push_i32((f as i64) as i32)?;
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

            rt.stack.push_i32(f as i32)?;
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

            rt.stack.push_i32(f as i32)?;
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

            rt.stack.push_i32((f as i64) as i32)?;
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

            rt.stack.push_i64(f as i64)?;
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
                ((f - 2f64.powi(63)) as i64) ^ i64::MIN
            } else {
                f as i64
            };

            rt.stack.push_i64(val)?;
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

            rt.stack.push_i64(f as i64)?;
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
                ((f - 2f64.powi(63)) as i64) ^ i64::MIN
            } else {
                f as i64
            };

            rt.stack.push_i64(val)?;
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
            op1::<i32, f32, _>(rt, |i| i as f32)?;

            // let convert_i32_s x =
            //   F32.of_float (Int32.to_float x)
        }

        Instruction::F32ConvertUI32 => {
            op1::<i32, f32, _>(rt, |i| {
                if i >= 0 {
                    i as f32
                } else {
                    let i = i as u32;
                    ((i >> 1) | (i & 0b1)) as f32 * 2f32
                }
            })?;

            // let convert_i32_u x =
            //   F32.of_float Int32.(
            //     if x >= zero then to_float x else
            //     to_float (logor (shift_right_logical x 1) (logand x 1l)) *. 2.0
            //   )
        }

        Instruction::F32ConvertSI64 => {
            op1::<i64, f32, _>(rt, |i| {
                if i.abs() < 0x10_0000_0000_0000 {
                    i as f32
                } else {
                    let r = if i & 0xfff == 0 { 0 } else { 1 };
                    ((i >> 12) | r) as f32 * 2f32.powi(12)
                }
            })?;

            // let convert_i64_s x =
            //   F32.of_float Int64.(
            //     if abs x < 0x10_0000_0000_0000L then to_float x else
            //     let r = if logand x 0xfffL = 0L then 0L else 1L in
            //     to_float (logor (shift_right x 12) r) *. 0x1p12
            //   )
        }

        Instruction::F32ConvertUI64 => {
            op1::<u64, f32, _>(rt, |i| {
                if i < 0x10_0000_0000_0000 {
                    i as f32
                } else {
                    let r = if i & 0xfff == 0 { 0 } else { 1 };
                    ((i >> 12) | r) as f32 * 2f32.powi(12)
                }
            })?;

            // let convert_i64_u x =
            //   F32.of_float Int64.(
            //     if I64.lt_u x 0x10_0000_0000_0000L then to_float x else
            //     let r = if logand x 0xfffL = 0L then 0L else 1L in
            //     to_float (logor (shift_right_logical x 12) r) *. 0x1p12
            //   )
        }

        Instruction::F64ConvertSI32 => {
            op1::<i32, f64, _>(rt, |i| i as f64)?;
        }

        Instruction::F64ConvertSI64 => {
            op1::<i64, f64, _>(rt, |i| i as f64)?;
        }

        Instruction::F64ConvertUI32 => {
            op1::<u32, f64, _>(rt, |i| ((i as u64) & 0x0000_0000_ffff_ffff) as f64)?;

            // let convert_i32_u x =
            //   F64.of_float Int64.(to_float (logand (of_int32 x) 0x0000_0000_ffff_ffffL))
        }

        Instruction::F64ConvertUI64 => {
            op1::<i64, f64, _>(rt, |i| {
                if i >= 0 {
                    i as f64
                } else {
                    let i = i as u64;
                    ((i >> 1) | (i & 1)) as f64 * 2f64
                }
            })?;

            // let convert_i64_u x =
            //   F64.of_float Int64.(
            //     if x >= zero then to_float x else
            //     to_float (logor (shift_right_logical x 1) (logand x 1L)) *. 2.0
            //   )
        }

        Instruction::F32DemoteF64 => {
            op1::<f64, f32, _>(rt, |f| f as f32)?;

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
            op1::<f32, f64, _>(rt, |f| f as f64)?;

            // let extend_i32_u x = Int64.logand (Int64.of_int32 x) 0x0000_0000_ffff_ffffL
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

        Instruction::SignExt(sign_ext) => match sign_ext {
            SignExtInstruction::I32Extend8S => {
                op1::<i32, i32, _>(rt, |i| (i << 24) >> 24)?;
            }
            SignExtInstruction::I32Extend16S => {
                op1::<i32, i32, _>(rt, |i| (i << 16) >> 16)?;
            }
            SignExtInstruction::I64Extend8S => {
                op1::<i64, i64, _>(rt, |i| (i << 56) >> 56)?;
            }
            SignExtInstruction::I64Extend16S => {
                op1::<i64, i64, _>(rt, |i| (i << 48) >> 48)?;
            }
            SignExtInstruction::I64Extend32S => {
                op1::<i64, i64, _>(rt, |i| (i << 32) >> 32)?;
            }
        },

        Instruction::Call(func_idx) => {
            // NB. invoke updates the ip
            invoke(rt, module_idx, func_idx)?;
        }

        Instruction::CallIndirect(sig, table_ref) => {
            let elem_idx = rt.stack.pop_i32()?;

            let table_addr = rt.modules[module_idx].table_addrs[table_ref as usize];
            let fun_addr = match rt.store.tables[table_addr as usize].get(elem_idx as usize) {
                Some(Some(fun_addr)) => *fun_addr,
                Some(None) => {
                    // TODO: This should be a trap?
                    return Err(ExecError::Panic("Table index not initialized".to_string()));
                }
                None => {
                    // "undefined element"
                    return Err(ExecError::Trap);
                }
            };

            // Check function type
            let call_instr_ty = &rt.modules[module_idx].types[sig as usize];
            let fun_module_idx = rt.store.funcs[fun_addr as usize].module_idx();
            let actual_fun_ty_idx = rt.store.funcs[fun_addr as usize].ty_idx();
            let actual_ty = &rt.modules[fun_module_idx].types[actual_fun_ty_idx as usize];
            // NB. We can't use (==) here because of the 'form' fields of FunctionTypes. TODO:
            // replace parity_wasm.
            if call_instr_ty.params() != actual_ty.params()
                || call_instr_ty.results() != actual_ty.results()
            {
                return Err(ExecError::Trap);
            }

            invoke_direct(rt, fun_addr)?;
        }

        Instruction::Drop => {
            let _ = rt.stack.pop_value();
            rt.ip += 1;
        }

        Instruction::Unreachable => {
            return Err(ExecError::Trap);
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        //                          Control flow instructions                                     //
        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::Block(block_ty) => {
            let (n_args, n_rets) = block_arity(rt, module_idx, block_ty);
            let mut args = Vec::with_capacity(n_args as usize);
            for _ in 0..n_args {
                args.push(rt.stack.pop_value()?);
            }

            let cont = match current_fun.block_to_end.get(&rt.ip) {
                None => {
                    return Err(ExecError::Panic(
                        "Couldn't find continuation of block".to_string(),
                    ));
                }
                Some(cont) => *cont,
            };

            rt.stack.push_block(cont, n_args, n_rets);

            for arg in args.into_iter().rev() {
                rt.stack.push_value(arg)?;
            }

            rt.ip += 1;
        }

        Instruction::Loop(block_ty) => {
            let (n_args, n_rets) = block_arity(rt, module_idx, block_ty);
            let mut args = Vec::with_capacity(n_args as usize);
            for _ in 0..n_args {
                args.push(rt.stack.pop_value()?);
            }

            rt.stack.push_loop(rt.ip, n_args, n_rets);

            for arg in args.into_iter().rev() {
                rt.stack.push_value(arg)?;
            }

            rt.ip += 1;
        }

        Instruction::If(block_ty) => {
            let cond = rt.stack.pop_i32()?;

            let (n_args, n_rets) = block_arity(rt, module_idx, block_ty);
            let mut args = Vec::with_capacity(n_args as usize);
            for _ in 0..n_args {
                args.push(rt.stack.pop_value()?);
            }

            let cont = match current_fun.block_to_end.get(&rt.ip) {
                None => {
                    return Err(ExecError::Panic(
                        "Couldn't find continuation of if".to_string(),
                    ));
                }
                Some(cont) => *cont,
            };

            rt.stack.push_block(cont, n_args, n_rets);
            for arg in args.into_iter().rev() {
                rt.stack.push_value(arg)?;
            }

            if cond != 0 {
                // true
                rt.ip += 1;
            } else {
                // false
                match current_fun.if_to_else.get(&rt.ip) {
                    None => {
                        // No else branch, jump to 'end', which will pop the block we've just
                        // pushed above. TODO: Maybe push it later?
                        rt.ip = cont;
                    }
                    Some(else_idx) => {
                        rt.ip = *else_idx + 1; // first instruction in else
                    }
                };
            }
        }

        Instruction::Else => {
            // 'end' of an 'if' block
            br(rt, 0)?;
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

        Instruction::End => {
            // Kinda like 'br(0)', but also works as 'return'. Then returning from a function we
            // use the continuation of the frame. Otherwise we bump ip by one.

            // Get return vals
            let return_arity = rt.stack.return_arity(0, EndOrBreak::End)?;
            let mut vals = Vec::with_capacity(return_arity as usize);
            for _ in 0..return_arity {
                vals.push(rt.stack.pop_value()?);
            }

            let Block { cont, kind, .. } = rt.stack.pop_block()?;

            for val in vals.into_iter().rev() {
                rt.stack.push_value(val)?;
            }

            let cont = if kind == BlockKind::Fun {
                rt.frames.pop();
                cont
            } else {
                rt.ip + 1
            };

            rt.ip = cont;
        }

        Instruction::Return => {
            ret(rt)?;
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::Atomics(_) | Instruction::Simd(_) | Instruction::Bulk(_) => {
            return Err(ExecError::Panic(format!(
                "Instruction not implemented: {:?}",
                instr
            )));
        }
    }

    Ok(())
}

fn trapping_add(a: u32, b: u32) -> Result<u32> {
    a.checked_add(b).ok_or(ExecError::Trap)
}

fn op1<A: StackValue, B: StackValue, F: Fn(A) -> B>(rt: &mut Runtime, op: F) -> Result<()> {
    let val = A::pop(&mut rt.stack)?;
    let ret = op(val);
    ret.push(&mut rt.stack);
    rt.ip += 1;
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

fn op2_trap<A: StackValue, B: StackValue, F: Fn(A, A) -> Result<B>>(
    rt: &mut Runtime,
    op: F,
) -> Result<()> {
    let val2 = A::pop(&mut rt.stack)?;
    let val1 = A::pop(&mut rt.stack)?;
    let ret = op(val1, val2)?;
    ret.push(&mut rt.stack);
    rt.ip += 1;
    Ok(())
}

fn block_arity(rt: &Runtime, module_idx: usize, ty: wasm::BlockType) -> (u32, u32) {
    match ty {
        wasm::BlockType::Value(_) => (0, 1),
        wasm::BlockType::NoResult => (0, 0),
        wasm::BlockType::TypeIdx(ty_idx) => {
            let ty = &rt.modules[module_idx].types[ty_idx as usize];
            (ty.params().len() as u32, ty.results().len() as u32)
        }
    }
}

fn br(rt: &mut Runtime, n_blocks: u32) -> Result<()> {
    // Get return vals
    let return_arity = rt.stack.return_arity(n_blocks, EndOrBreak::Break)?;
    let mut vals = Vec::with_capacity(return_arity as usize);
    for _ in 0..return_arity {
        vals.push(rt.stack.pop_value()?);
    }

    // Pop blocks
    for _ in 0..n_blocks {
        let Block { kind, .. } = rt.stack.pop_block()?;
        assert!(kind != BlockKind::Fun);
    }

    // Last block
    let Block { cont, kind, .. } = rt.stack.pop_block()?;

    // println!("br({}) arity={}, cont={}", n_blocks, return_arity, cont);

    for val in vals.into_iter().rev() {
        rt.stack.push_value(val)?;
    }

    // `cont` is the `end` instruction for the last block but we wan't to execute `end` as we've
    // already adjusted the stack. Continue from the next instruction after `end` (which could be
    // `end` for another block).
    // let current_fun_idx = rt.frames.current()?.fun_idx;
    // match &rt.store.funcs[current_fun_idx as usize]
    //     .fun
    //     .code()
    //     .elements()[cont as usize]
    // {
    //     Instruction::End => {}
    //     other => {
    //         panic!("Continuation of a block is not 'end': {:?}", other);
    //     }
    // }

    match kind {
        BlockKind::Top => panic!(),
        BlockKind::Block => {
            rt.ip = cont + 1;
        }
        BlockKind::Loop => {
            rt.ip = cont;
        }
        BlockKind::Fun => {
            rt.frames.pop();
            rt.ip = cont;
        }
    }

    Ok(())
}

fn ret(rt: &mut Runtime) -> Result<()> {
    let current_fun_addr = rt.frames.current()?.fun_addr;
    let current_fun = match &rt.store.funcs[current_fun_addr as usize] {
        Func::Wasm(fun) => fun,
        Func::Host { .. } => {
            return Err(ExecError::Panic("ret: host function".to_string()));
        }
    };
    let module_idx = current_fun.module_idx;
    let ty_idx = current_fun.ty_idx;

    let fun_return_arity = rt.modules[module_idx].types[ty_idx as usize]
        .results()
        .len();

    let mut vals = Vec::with_capacity(fun_return_arity);
    for _ in 0..fun_return_arity {
        vals.push(rt.stack.pop_value()?);
    }

    let Block { cont, .. } = rt.stack.pop_fun_block()?;

    for val in vals.into_iter().rev() {
        rt.stack.push_value(val)?;
    }

    rt.frames.pop();
    rt.ip = cont;

    Ok(())
}
