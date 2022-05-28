use crate::export::Export;
use crate::frame::FrameStack;
use crate::fun::Fun;
use crate::mem::Mem;
use crate::module::{FunIdx, GlobalIdx, MemIdx, Module, TableIdx, TypeIdx};
use crate::stack::{Block, BlockKind, EndOrBreak, Stack, StackValue};
use crate::store::{FunAddr, Global, ModuleAddr, Store};
use crate::value::{self, Value};
use crate::wasi::allocate_wasi;
use crate::{ExecError, Result};
use crate::{HostFunDecl, MemAddr};

use fxhash::FxHashMap;
use ieee754::Ieee754;
use libwasmrun_syntax::elements as wasm;
use wasi_common::{WasiCtx, WasiCtxBuilder};
use wasm::{Instruction, SignExtInstruction};

use std::fmt;
use std::iter;
use std::mem::take;
use std::rc::Rc;

pub(crate) const PAGE_SIZE: usize = 65536;
pub(crate) const MAX_PAGES: u32 = 65536; // (2**32 - 1 / PAGE_SIZE), or 0x10000

#[derive(Debug, Clone, Copy)]
pub enum Trap {
    /// Undefined table element called
    UndefinedElement,
    /// Uninitialized table element called
    UninitializedElement,
    /// Indirect function call target doesn't have expected type
    IndirectCallTypeMismatch,
    /// Out of bounds table element index
    OOBTableElementIdx,
    /// Out of bounds memory access
    OOBMemoryAccess,
    /// Integer divide by zero
    IntDivideByZero,
    /// Integer overflow
    IntOverflow,
    /// Invalid conversion to integer
    InvalidConvToInt,
    /// 'unreachable' instruction executed
    Unreachable,
}

impl fmt::Display for Trap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Trap::UndefinedElement => "undefined element".fmt(f),
            Trap::UninitializedElement => "uninitialized element".fmt(f),
            Trap::IndirectCallTypeMismatch => "indirect call type mismatch".fmt(f),
            Trap::OOBTableElementIdx => "out of bounds element".fmt(f),
            Trap::OOBMemoryAccess => "out of bound memory access".fmt(f),
            Trap::IntDivideByZero => "divide by zero".fmt(f),
            Trap::IntOverflow => "integer overflow".fmt(f),
            Trap::InvalidConvToInt => "invalid conversion to integer".fmt(f),
            Trap::Unreachable => "unreachable executed".fmt(f),
        }
    }
}

pub struct Runtime {
    /// The heap
    pub store: Store,
    /// Value and continuation stack
    pub(crate) stack: Stack,
    /// Call stack. Frames hold locals.
    pub(crate) frames: FrameStack,
    /// WASI state
    pub(crate) wasi_ctx: WasiCtx,
    /// Maps registered modules to their module addresses
    module_names: FxHashMap<String, ModuleAddr>,
    /// Instruction pointer
    ip: u32,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            store: Default::default(),
            stack: Default::default(),
            frames: Default::default(),
            wasi_ctx: WasiCtx::new(iter::empty::<String>()).unwrap(),
            module_names: Default::default(),
            ip: Default::default(),
        }
    }

    pub fn new_with_wasi(program_args: Vec<String>) -> Self {
        let mut wasi_builder = WasiCtxBuilder::new();
        wasi_builder.args(program_args);
        wasi_builder.inherit_stderr();
        wasi_builder.inherit_stdout();

        let wasi_ctx = wasi_builder.build().unwrap();
        Runtime::new_with_wasi_ctx(wasi_ctx)
    }

    pub fn new_with_wasi_ctx(wasi_ctx: WasiCtx) -> Self {
        let mut store = Default::default();
        let wasi_addr = allocate_wasi(&mut store);

        let mut module_names: FxHashMap<String, ModuleAddr> = Default::default();
        module_names.insert("wasi_snapshot_preview1".to_owned(), wasi_addr);
        module_names.insert("wasi_unstable".to_owned(), wasi_addr);

        Runtime {
            store,
            stack: Default::default(),
            frames: Default::default(),
            wasi_ctx,
            module_names,
            ip: Default::default(),
        }
    }

    pub fn new_test() -> Self {
        let mut rt = Runtime::new();
        allocate_spectest(&mut rt);
        rt
    }

    pub fn backtrace(&self) -> &FrameStack {
        &self.frames
    }

    pub fn clear_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
    }

    pub(crate) fn get_module(&self, addr: ModuleAddr) -> &Module {
        self.store.get_module(addr)
    }

    pub fn get_module_mem_addr(&self, module_addr: ModuleAddr, mem_idx: MemIdx) -> MemAddr {
        self.store.get_module(module_addr).get_mem(mem_idx)
    }

    pub fn pop_value(&mut self) -> Option<Value> {
        self.stack.pop_value_opt().unwrap()
    }

    pub fn pop_i32(&mut self) -> Result<i32> {
        self.stack.pop_i32()
    }

    pub fn push_value(&mut self, value: Value) {
        self.stack.push_value(value).unwrap()
    }

    pub fn register_module(&mut self, name: String, module_addr: ModuleAddr) {
        self.module_names.insert(name, module_addr);
    }

    pub fn get_global(&mut self, module_addr: ModuleAddr, name: &str) -> Option<Value> {
        let global_addr = self
            .store
            .get_module(module_addr)
            .get_exported_global(name)?;
        Some(self.store.get_global(global_addr).value)
    }

    pub fn get_local(&self, local_idx: u32) -> Result<Value> {
        self.frames.current()?.get_local(local_idx)
    }

    pub fn set_local(&mut self, local_idx: u32, value: Value) -> Result<()> {
        self.frames.current_mut()?.set_local(local_idx, value)
    }

    /// Allocate a module with just host functions and nothing else
    pub fn allocate_host_module(
        &mut self,
        module_name: String,
        fns: Vec<(String, HostFunDecl)>,
    ) -> ModuleAddr {
        let module_addr = self.store.next_module_addr();
        let mut module: Module = Default::default();

        for (host_fn_name, host_fn) in fns {
            let HostFunDecl {
                arg_tys,
                ret_tys,
                fun,
            } = host_fn;

            let ty_idx = module.add_type(wasm::FunctionType::new(arg_tys, ret_tys));
            let fun_addr = self.store.allocate_host_fun(module_addr, ty_idx, fun);
            let fun_idx = module.add_fun(fun_addr);
            module.add_export(Export::new_fun(host_fn_name, fun_idx));
        }

        let module_addr_ = self.store.allocate_module(module);
        assert_eq!(module_addr, module_addr_);

        self.module_names.insert(module_name, module_addr);

        module_addr
    }
}

pub(crate) fn allocate_spectest(rt: &mut Runtime) {
    // https://github.com/WebAssembly/spec/blob/7526564b56c30250b66504fe795e9c1e88a938af/interpreter/host/spectest.ml

    let module_addr = rt.store.next_module_addr();
    let mut module: Module = Default::default();

    let table_addr = rt.store.allocate_table(vec![None; 10]);
    let table_idx = module.add_table(table_addr);
    module.add_export(Export::new_table("table".to_owned(), table_idx));

    let mem_addr = rt.store.allocate_mem(Mem::new(1, Some(2)));
    let mem_idx = module.add_mem(mem_addr);
    module.add_export(Export::new_mem("memory".to_owned(), mem_idx));

    let global_i32_addr = rt.store.allocate_global(Global {
        value: Value::I32(666),
        mutable: false,
    });
    let global_i32_idx = module.add_global(global_i32_addr);
    module.add_export(Export::new_global("global_i32".to_owned(), global_i32_idx));

    let global_i64_addr = rt.store.allocate_global(Global {
        value: Value::I64(666),
        mutable: false,
    });
    let global_i64_idx = module.add_global(global_i64_addr);
    module.add_export(Export::new_global("global_i64".to_owned(), global_i64_idx));

    let global_f32_addr = rt.store.allocate_global(Global {
        value: Value::F32(666.6f32),
        mutable: false,
    });
    let global_f32_idx = module.add_global(global_f32_addr);
    module.add_export(Export::new_global("global_f32".to_owned(), global_f32_idx));

    let global_f64_addr = rt.store.allocate_global(Global {
        value: Value::F64(666.6f64),
        mutable: false,
    });
    let global_f64_idx = module.add_global(global_f64_addr);
    module.add_export(Export::new_global("global_f64".to_owned(), global_f64_idx));

    let print_ty = module.add_type(wasm::FunctionType::new(vec![], vec![]));
    let print_addr = rt
        .store
        .allocate_host_fun(module_addr, print_ty, Rc::new(|_, _| Ok(vec![])));
    let print_idx = module.add_fun(print_addr);
    module.add_export(Export::new_fun("print".to_owned(), print_idx));

    let print_i32_ty = module.add_type(wasm::FunctionType::new(vec![wasm::ValueType::I32], vec![]));
    let print_i32_addr =
        rt.store
            .allocate_host_fun(module_addr, print_i32_ty, Rc::new(|_, _| Ok(vec![])));
    let print_i32_idx = module.add_fun(print_i32_addr);
    module.add_export(Export::new_fun("print_i32".to_owned(), print_i32_idx));

    let print_i32_f32_ty = module.add_type(wasm::FunctionType::new(
        vec![wasm::ValueType::I32, wasm::ValueType::F32],
        vec![],
    ));
    let print_i32_f32_addr =
        rt.store
            .allocate_host_fun(module_addr, print_i32_f32_ty, Rc::new(|_, _| Ok(vec![])));
    let print_i32_f32_idx = module.add_fun(print_i32_f32_addr);
    module.add_export(Export::new_fun(
        "print_i32_f32".to_owned(),
        print_i32_f32_idx,
    ));

    let print_f64_f64_ty = module.add_type(wasm::FunctionType::new(
        vec![wasm::ValueType::F64, wasm::ValueType::F64],
        vec![],
    ));
    let print_f64_f64_addr =
        rt.store
            .allocate_host_fun(module_addr, print_f64_f64_ty, Rc::new(|_, _| Ok(vec![])));
    let print_f64_f64_idx = module.add_fun(print_f64_f64_addr);
    module.add_export(Export::new_fun(
        "print_f64_f64".to_owned(),
        print_f64_f64_idx,
    ));

    let print_f32_ty = module.add_type(wasm::FunctionType::new(vec![wasm::ValueType::F32], vec![]));
    let print_f32_addr =
        rt.store
            .allocate_host_fun(module_addr, print_f32_ty, Rc::new(|_, _| Ok(vec![])));
    let print_f32_idx = module.add_fun(print_f32_addr);
    module.add_export(Export::new_fun("print_f32".to_owned(), print_f32_idx));

    let print_f64_ty = module.add_type(wasm::FunctionType::new(vec![wasm::ValueType::F64], vec![]));
    let print_f64_addr =
        rt.store
            .allocate_host_fun(module_addr, print_f64_ty, Rc::new(|_, _| Ok(vec![])));
    let print_f64_idx = module.add_fun(print_f64_addr);
    module.add_export(Export::new_fun("print_f64".to_owned(), print_f64_idx));

    rt.store.allocate_module(module);
    rt.module_names.insert("spectest".to_string(), module_addr);
}

pub fn allocate_module(rt: &mut Runtime, parsed_module: wasm::Module) -> Result<ModuleAddr> {
    // https://webassembly.github.io/spec/core/exec/modules.html
    let mut parsed_module = match parsed_module.parse_names() {
        Ok(m) => m,
        Err((_, m)) => m,
    };

    let module_addr = rt.store.next_module_addr();

    let mut inst = Module::default();

    if let Some(type_section) = parsed_module.type_section_mut() {
        for ty in type_section.types_mut().drain(..) {
            match ty {
                wasm::Type::Function(fun_ty) => {
                    inst.add_type(fun_ty);
                }
            }
        }
    }

    // Number of functions seen in imports. Used to get function indices in code section
    let mut n_funs = 0;

    // Add imported stuff to the module
    if let Some(import_section) = parsed_module.import_section_mut() {
        for import in import_section.entries_mut().drain(..) {
            let module_name = import.module();
            let field_name = import.field();

            let imported_module_addr = match rt.module_names.get(module_name) {
                Some(module_addr) => *module_addr,
                None => {
                    return Err(ExecError::Panic(format!(
                        "Can't find imported module {:?}",
                        module_name
                    )));
                }
            };
            let imported_module = rt.store.get_module(imported_module_addr);

            match import.external() {
                wasm::External::Function(_fun_ty_idx) => {
                    n_funs += 1;
                    let fun_addr = imported_module.get_exported_fun(field_name).unwrap();
                    inst.add_fun(fun_addr);
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
                    inst.add_table(tbl_addr);
                }
                wasm::External::Memory(_mem_ty) => {
                    let mem_addr = imported_module.get_exported_mem(field_name).unwrap();
                    inst.add_mem(mem_addr);
                }
                wasm::External::Global(_gbl_ty) => {
                    let global_addr = imported_module.get_exported_global(field_name).unwrap();
                    inst.add_global(global_addr);
                }
            }
        }
    }

    // Allocate functions
    if let Some(code_section) = parsed_module.code_section_mut().take() {
        for (fun_idx, fun) in take(code_section.bodies_mut()).into_iter().enumerate() {
            let function_section = parsed_module.function_section().ok_or_else(|| {
                ExecError::Panic("Module has a code section but no function section".to_string())
            })?;

            let fun_idx_ = fun_idx + n_funs;

            let fun_addr = rt.store.allocate_fun(
                module_addr,
                TypeIdx(function_section.entries()[fun_idx].type_ref()),
                fun,
                parsed_module
                    .names_section()
                    .and_then(|names| names.functions())
                    .and_then(|fun_names| fun_names.names().get(fun_idx_ as u32))
                    .cloned(),
                parsed_module
                    .names_section()
                    .and_then(|names| names.locals())
                    .and_then(|funs| funs.local_names().get(fun_idx_ as u32).cloned()),
            )?;

            inst.add_fun(fun_addr);
        }
    }

    // Allocate tables
    if let Some(table_section) = parsed_module.table_section_mut() {
        for table in table_section.entries_mut().drain(..) {
            let table_addr = rt
                .store
                .allocate_table(vec![None; table.limits().initial() as usize]);
            inst.add_table(table_addr);
        }
    }

    // Allocate table elements
    if let Some(element_section) = parsed_module.elements_section() {
        for elements in element_section.entries() {
            // https://webassembly.github.io/spec/core/syntax/modules.html#syntax-elem
            match &elements.mode {
                wasm::ElementSegmentMode::Active { table_idx, offset } => {
                    // Spec: "An active element segment copies its elements into a table during
                    // instantiation, as specified by a table index and a constant expression
                    // defining an offset into that table."
                    // TODO: Do we need a const evaluator for stuff evaluatable in instantiation
                    // time?
                    let offset = match offset.code() {
                        &[Instruction::I32Const(offset), Instruction::End] => offset as usize,
                        &[Instruction::I64Const(offset), Instruction::End] => offset as usize,
                        other => todo!("Unhandled offset expression: {:?}", other),
                    };

                    let table = rt.store.get_table_mut(inst.get_table(TableIdx(*table_idx)));

                    for (elem_idx, elem) in elements.init.iter().enumerate() {
                        let elem_idx = offset + elem_idx;
                        if elem_idx >= table.len() {
                            table.resize(elem_idx + 1, None);
                        }
                        let func_idx = match elem.code() {
                            &[Instruction::I32Const(func_idx), Instruction::End] => func_idx,
                            other => todo!("Unhandled element expression: {:?}", other),
                        };
                        table[elem_idx] = Some(inst.get_fun(FunIdx(func_idx as u32)));
                    }
                }
                wasm::ElementSegmentMode::Passive => {
                    // Spec: "A passive element segment’s elements can be copied to a table using
                    // the `table.init` instruction."
                    todo!("Passive element segments: {:?}", elements)
                }
                wasm::ElementSegmentMode::Declarative => {
                    // Spec: "A declarative element segment is not available at runtime but merely
                    // serves to forward-declare references that are formed in code with
                    // instructions like `ref.func`.
                    todo!("Declarative element segments: {:?}", elements)
                }
            }
        }
    }

    // Allocate memories
    if let Some(memory_section) = parsed_module.memory_section_mut() {
        assert!(memory_section.entries().len() <= 1); // No more than 1 currently
        for mem in memory_section.entries_mut().drain(..) {
            let mem_addr = rt
                .store
                .allocate_mem(Mem::new(mem.limits().initial(), mem.limits().maximum()));
            inst.add_mem(mem_addr);
        }
    }

    // Allcoate globals
    if let Some(global_section) = parsed_module.global_section_mut() {
        for global in global_section.entries_mut().drain(..) {
            let value = get_const_expr_val(rt, &inst, global.init_expr().code())?;
            let global_addr = rt.store.allocate_global(Global {
                value,
                mutable: global.global_type().is_mutable(),
            });
            inst.add_global(global_addr);
        }
    }

    // Get names of functions
    // NB. This is not used for calling functions as we can only call exported functions, which
    // already have names in the export section.
    if let Some(names_section) = parsed_module.names_section_mut() {
        if let Some(function_names) = names_section.functions_mut() {
            for (fun_idx, fun_name) in take(function_names.names_mut()).into_iter() {
                inst.add_fun_name(fun_name, FunIdx(fun_idx));
            }
        }
    }

    // Initialize memories with 'data' section
    if let Some(data_section) = parsed_module.data_section() {
        for data in data_section.entries() {
            let mem_idx = MemIdx(data.index());
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

            let mem_addr = inst.get_mem(mem_idx);
            let mem = &mut rt.store.get_mem_mut(mem_addr);
            mem.set_range(offset, values)?;
        }
    }

    // Initialize exports
    if let Some(export_section) = parsed_module.export_section_mut() {
        for mut export in export_section.entries_mut().drain(..) {
            let export_field = take(export.field_mut());
            let export = match export.internal() {
                wasm::Internal::Function(fun_idx) => {
                    let idx = FunIdx(*fun_idx);
                    inst.add_fun_name(export_field.clone(), idx);
                    Export::new_fun(export_field, idx)
                }
                wasm::Internal::Table(table_idx) => {
                    Export::new_table(export_field, TableIdx(*table_idx))
                }
                wasm::Internal::Memory(mem_idx) => Export::new_mem(export_field, MemIdx(*mem_idx)),
                wasm::Internal::Global(global_idx) => {
                    Export::new_global(export_field, GlobalIdx(*global_idx))
                }
            };
            inst.add_export(export);
        }
    }

    // Set start
    if let Some(start_idx) = parsed_module.start_section() {
        inst.set_start(FunIdx(start_idx));
    }
    let start = inst.get_start();

    // Done
    rt.store.allocate_module(inst);

    if let Some(start_idx) = start {
        invoke(rt, module_addr, start_idx)?;
        finish(rt)?;
    }

    Ok(module_addr)
}

fn get_const_expr_val(rt: &Runtime, module: &Module, instrs: &[Instruction]) -> Result<Value> {
    use Instruction::*;
    match instrs {
        [I32Const(value), End] => Ok(Value::I32(*value)),
        [I64Const(value), End] => Ok(Value::I64(*value)),
        [F32Const(value), End] => Ok(Value::F32(f32::from_bits(*value))),
        [F64Const(value), End] => Ok(Value::F64(f64::from_bits(*value))),
        [GetGlobal(idx), End] => Ok(rt
            .store
            .get_global(module.get_global(GlobalIdx(*idx)))
            .value),
        other => Err(ExecError::Panic(format!("Global initializer: {:?}", other))),
    }
}

pub fn invoke_by_name(rt: &mut Runtime, module_addr: ModuleAddr, fun_name: &str) -> Result<()> {
    match rt.store.get_module(module_addr).get_fun_name(fun_name) {
        None => Err(ExecError::Panic(format!("Unknown function: {}", fun_name))),
        Some(fun_idx) => invoke(rt, module_addr, fun_idx),
    }
}

pub(crate) fn invoke(rt: &mut Runtime, module_addr: ModuleAddr, fun_idx: FunIdx) -> Result<()> {
    let fun_addr = rt.store.get_module(module_addr).get_fun(fun_idx);
    invoke_direct(rt, fun_addr)
}

fn invoke_direct(rt: &mut Runtime, fun_addr: FunAddr) -> Result<()> {
    let fun = rt.store.get_fun(fun_addr);

    let caller = match fun {
        Fun::Host(_) => rt.frames.current().ok().map(|frame| frame.fun_addr),
        Fun::Wasm(_) => None,
    };

    let fun_ty = rt
        .store
        .get_module(fun.module_addr())
        .get_type(fun.ty_idx());

    let arg_tys = fun_ty.params();

    let n_args = fun_ty.params().len();
    let n_rets = fun_ty.results().len();

    rt.frames.push(fun, arg_tys);

    for local_idx in (0..n_args).rev() {
        let arg_val = rt.stack.pop_value()?;
        rt.frames
            .current_mut()?
            .set_local(local_idx as u32, arg_val)?;
    }

    rt.stack
        .push_fun_block(rt.ip + 1, n_args as u32, n_rets as u32);

    match fun {
        Fun::Wasm(fun) => {
            debug_assert!(match fun.fun.code().elements().last().unwrap() {
                Instruction::End => true,
                other => panic!("Last instruction of function is not 'end': {:?}", other),
            });

            rt.ip = 0;

            Ok(())
        }
        Fun::Host(fun) => {
            let caller_mem_addr = caller.and_then(|caller| {
                let caller_module_addr = rt.store.get_fun(caller).module_addr();
                rt.store
                    .get_module(caller_module_addr)
                    .get_mem_opt(MemIdx(0))
            });

            let vals = (fun.fun.clone())(rt, caller_mem_addr)?;

            rt.stack.pop_fun_block()?;
            rt.frames.pop();

            for val in vals.into_iter().rev() {
                rt.stack.push_value(val)?;
            }

            rt.ip += 1;

            Ok(())
        }
    }
}

pub fn finish(rt: &mut Runtime) -> Result<()> {
    while !rt.frames.is_empty() {
        single_step(rt)?;
    }
    Ok(())
}

pub(crate) fn single_step(rt: &mut Runtime) -> Result<()> {
    let current_fun_addr = match rt.frames.current() {
        Ok(current_fun) => current_fun.fun_addr,
        Err(_) => {
            return Ok(());
        }
    };
    let current_fun = match &rt.store.get_fun(current_fun_addr) {
        Fun::Wasm(fun) => fun,
        Fun::Host { .. } => {
            return Err(ExecError::Panic("single_step: host function".to_string()));
        }
    };

    assert!((rt.ip as usize) < current_fun.fun.code().elements().len());

    // Instruction is just 3 words so clonning here should be fine, and avoid borrowchk issues
    // later on
    let instr = current_fun.fun.code().elements()[rt.ip as usize].clone();
    let module_addr = current_fun.module_addr;

    // println!(
    //     "ip={}, instruction={:?}, stack={:?}, call stack={:?}",
    //     rt.ip, instr, rt.stack, rt.frames,
    // );

    match instr {
        Instruction::GrowMemory(mem_ref) => {
            assert_eq!(mem_ref, 0);

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);

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
            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
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

            let mem_addr = rt.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_32(addr, value as u32)?;

            rt.ip += 1;
        }

        Instruction::F32Store(_align, offset) => {
            let value = rt.stack.pop_f32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_32(addr, value.to_bits())?;

            rt.ip += 1;
        }

        Instruction::I64Store(_align, offset) => {
            let value = rt.stack.pop_i64()?;
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_64(addr, value as u64)?;

            rt.ip += 1;
        }

        Instruction::F64Store(_align, offset) => {
            let value = rt.stack.pop_f64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_64(addr, value.to_bits())?;

            rt.ip += 1;
        }

        Instruction::I64Store8(_align, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);
            mem.check_range(addr, 1)?;

            let val = c as u8;
            mem[addr] = val;

            rt.ip += 1;
        }

        Instruction::I64Store16(_align, offset) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);
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

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);
            mem.check_range(addr, 4)?;

            mem.store_32(addr, c as u32)?;

            rt.ip += 1;
        }

        Instruction::I64Load8S(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 1)?;

            let val = mem[addr];
            rt.stack.push_i64(((val as i64) << 56) >> 56)?;

            rt.ip += 1;
        }

        Instruction::I32Load(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);

            rt.stack.push_i32(mem.load_32(addr)? as i32)?;

            rt.ip += 1;
        }

        Instruction::F32Load(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);

            rt.stack.push_f32(f32::from_bits(mem.load_32(addr)?))?;

            rt.ip += 1;
        }

        Instruction::I64Load(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
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

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
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

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 1)?;

            let b = mem[addr];
            rt.stack.push_i32(i32::from_le_bytes([b, 0, 0, 0]))?;
            rt.ip += 1;
        }

        Instruction::I32Load8S(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 1)?;

            let b = mem[addr];
            let val = i8::from_le_bytes([b]) as i32;
            rt.stack.push_i32(val)?;
            rt.ip += 1;
        }

        Instruction::I64Load8U(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 1)?;

            let b = mem[addr];
            rt.stack
                .push_i64(i64::from_le_bytes([b, 0, 0, 0, 0, 0, 0, 0]))?;
            rt.ip += 1;
        }

        Instruction::I32Load16U(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 2)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];
            rt.stack.push_i32(i32::from_le_bytes([b1, b2, 0, 0]))?;
            rt.ip += 1;
        }

        Instruction::I32Load16S(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 2)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];

            rt.stack.push_i32(i16::from_le_bytes([b1, b2]) as i32)?;
            rt.ip += 1;
        }

        Instruction::I64Load16U(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
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

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);
            mem.check_range(addr, 2)?;

            let b1 = mem[addr];
            let b2 = mem[addr + 1];

            rt.stack.push_i64(i16::from_le_bytes([b1, b2]) as i64)?;
            rt.ip += 1;
        }

        Instruction::I64Load32U(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);

            rt.stack.push_i64(mem.load_32(addr)? as i64)?;

            rt.ip += 1;
        }

        Instruction::I64Load32S(_align, offset) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem(mem_addr);

            rt.stack.push_i64((mem.load_32(addr)? as i32) as i64)?;

            rt.ip += 1;
        }

        Instruction::I32Store8(_align, offset) => {
            let c = rt.stack.pop_i32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);
            mem.check_range(addr, 1)?;

            mem[addr] = c as u8;
            rt.ip += 1;
        }

        Instruction::I32Store16(_align, offset) => {
            let c = rt.stack.pop_i32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(0));
            let mem = rt.store.get_mem_mut(mem_addr);
            mem.check_range(addr, 2)?;

            let [b1, b2] = (c as u16).to_le_bytes();
            mem[addr] = b1;
            mem[addr + 1] = b2;
            rt.ip += 1;
        }

        Instruction::GetLocal(idx) => {
            let val = rt.get_local(idx)?;
            rt.stack.push_value(val)?;
            rt.ip += 1;
        }

        Instruction::SetLocal(idx) => {
            let val = rt.stack.pop_value()?;
            rt.set_local(idx, val)?;
            rt.ip += 1;
        }

        Instruction::TeeLocal(idx) => {
            let val = rt.stack.pop_value()?;
            rt.set_local(idx, val)?;
            rt.stack.push_value(val)?;
            rt.ip += 1;
        }

        Instruction::GetGlobal(idx) => {
            let global_addr = rt.get_module(module_addr).get_global(GlobalIdx(idx));
            let value = rt.store.get_global(global_addr).value;
            rt.stack.push_value(value)?;
            rt.ip += 1;
        }

        Instruction::SetGlobal(idx) => {
            let global_addr = rt.get_module(module_addr).get_global(GlobalIdx(idx));
            let value = rt.stack.pop_value()?;
            rt.store.get_global_mut(global_addr).value = value;
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
            rt.stack.push_f32(f32::from_bits(f))?;
            rt.ip += 1;
        }

        Instruction::F64Const(f) => {
            rt.stack.push_f64(f64::from_bits(f))?;
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
            op2::<f32, f32, _>(rt, |a, b| value::canonicalize_f32_nan(a + b))?;
        }

        Instruction::I64Add => {
            op2::<i64, i64, _>(rt, i64::wrapping_add)?;
        }

        Instruction::F64Add => {
            op2::<f64, f64, _>(rt, |a, b| value::canonicalize_f64_nan(a + b))?;
        }

        Instruction::I32Sub => {
            op2::<i32, i32, _>(rt, i32::wrapping_sub)?;
        }

        Instruction::F32Sub => {
            op2::<f32, f32, _>(rt, |a, b| value::canonicalize_f32_nan(a - b))?;
        }

        Instruction::I64Sub => {
            op2::<i64, i64, _>(rt, i64::wrapping_sub)?;
        }

        Instruction::F64Sub => {
            op2::<f64, f64, _>(rt, |a, b| value::canonicalize_f64_nan(a - b))?;
        }

        Instruction::I32Mul => {
            op2::<i32, i32, _>(rt, i32::wrapping_mul)?;
        }

        Instruction::F32Mul => {
            op2::<f32, f32, _>(rt, |a, b| value::canonicalize_f32_nan(a * b))?;
        }

        Instruction::I64Mul => {
            op2::<i64, i64, _>(rt, i64::wrapping_mul)?;
        }

        Instruction::F64Mul => {
            op2::<f64, f64, _>(rt, |a, b| value::canonicalize_f64_nan(a * b))?;
        }

        Instruction::I32DivS => {
            op2_trap::<i32, i32, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap(Trap::IntDivideByZero))
                } else {
                    i32::checked_div(a, b).ok_or(ExecError::Trap(Trap::IntOverflow))
                }
            })?;
        }

        Instruction::F32Div => {
            op2::<f32, f32, _>(rt, |a, b| value::canonicalize_f32_nan(a / b))?;
        }

        Instruction::I64DivS => {
            op2_trap::<i64, i64, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap(Trap::IntDivideByZero))
                } else {
                    i64::checked_div(a, b).ok_or(ExecError::Trap(Trap::IntOverflow))
                }
            })?;
        }

        Instruction::F64Div => {
            op2::<f64, f64, _>(rt, |a, b| value::canonicalize_f64_nan(a / b))?;
        }

        Instruction::I32DivU => {
            op2_trap::<u32, u32, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap(Trap::IntDivideByZero))
                } else {
                    Ok(u32::wrapping_div(a, b))
                }
            })?;
        }

        Instruction::I64DivU => {
            op2_trap::<u64, u64, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap(Trap::IntDivideByZero))
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
                    Err(ExecError::Trap(Trap::IntDivideByZero))
                } else {
                    Ok(a.wrapping_rem(b))
                }
            })?;
        }

        Instruction::I64RemS => {
            op2_trap::<i64, i64, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap(Trap::IntDivideByZero))
                } else {
                    Ok(a.wrapping_rem(b))
                }
            })?;
        }

        Instruction::I32RemU => {
            op2_trap::<u32, u32, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap(Trap::IntDivideByZero))
                } else {
                    Ok(a.wrapping_rem(b))
                }
            })?;
        }

        Instruction::I64RemU => {
            op2_trap::<u64, u64, _>(rt, |a, b| {
                if b == 0 {
                    Err(ExecError::Trap(Trap::IntDivideByZero))
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
                    f32::from_bits(a.to_bits() & b.to_bits())
                } else if a > b {
                    a
                } else if a < b {
                    b
                } else {
                    value::canonical_f32_nan()
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
                    f64::from_bits(a.to_bits() & b.to_bits())
                } else if a > b {
                    a
                } else if a < b {
                    b
                } else {
                    value::canonical_f64_nan()
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
                    value::canonical_f32_nan()
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
                    value::canonical_f64_nan()
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
            op1::<f32, f32, _>(rt, |a| value::canonicalize_f32_nan(a.sqrt()))?;
        }

        Instruction::F64Sqrt => {
            op1::<f64, f64, _>(rt, |a| value::canonicalize_f64_nan(a.sqrt()))?;
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
            op1::<f32, f32, _>(rt, |a| value::canonicalize_f32_nan(a.ceil()))?;
        }

        Instruction::F64Ceil => {
            op1::<f64, f64, _>(rt, |a| value::canonicalize_f64_nan(a.ceil()))?;
        }

        Instruction::F32Floor => {
            op1::<f32, f32, _>(rt, |a| value::canonicalize_f32_nan(a.floor()))?;
        }

        Instruction::F64Floor => {
            op1::<f64, f64, _>(rt, |a| value::canonicalize_f64_nan(a.floor()))?;
        }

        Instruction::F32Trunc => {
            op1::<f32, f32, _>(rt, |a| value::canonicalize_f32_nan(a.trunc()))?;
        }

        Instruction::F64Trunc => {
            op1::<f64, f64, _>(rt, |a| value::canonicalize_f64_nan(a.trunc()))?;
        }

        Instruction::I32ReinterpretF32 => {
            op1::<f32, u32, _>(rt, f32::to_bits)?;
        }

        Instruction::I64ReinterpretF64 => {
            op1::<f64, u64, _>(rt, f64::to_bits)?;
        }

        Instruction::F32ReinterpretI32 => {
            op1::<u32, f32, _>(rt, f32::from_bits)?;
        }

        Instruction::F64ReinterpretI64 => {
            op1::<u64, f64, _>(rt, f64::from_bits)?;
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
                    value::canonicalize_f32_nan(f)
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
                    value::canonicalize_f64_nan(f)
                })?;
        }

        Instruction::F32Copysign => {
            op2::<f32, f32, _>(rt, Ieee754::copy_sign)?;
        }

        Instruction::F64Copysign => {
            op2::<f64, f64, _>(rt, Ieee754::copy_sign)?;
        }

        Instruction::I64ExtendUI32 => {
            op1::<u32, u64, _>(rt, |i| i as u64)?;
        }

        Instruction::I64ExtendSI32 => {
            op1::<i32, i64, _>(rt, |i| i as i64)?;
        }

        Instruction::I32TruncUF32 => {
            op1_trap::<f32, i32, _>(rt, |f| {
                if f.is_nan() {
                    return Err(ExecError::Trap(Trap::InvalidConvToInt));
                }

                if f >= (-(i32::MIN as f32) * 2f32) || f <= -1f32 {
                    return Err(ExecError::Trap(Trap::IntOverflow));
                }

                Ok((f as i64) as i32)
            })?;

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
            op1_trap::<f32, i32, _>(rt, |f| {
                if f.is_nan() {
                    return Err(ExecError::Trap(Trap::InvalidConvToInt));
                }

                if f >= -(i32::MIN as f32) || f < (i32::MIN as f32) {
                    return Err(ExecError::Trap(Trap::IntOverflow));
                }

                Ok(f as i32)
            })?;

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
            op1_trap::<f64, i32, _>(rt, |f| {
                if f.is_nan() {
                    return Err(ExecError::Trap(Trap::InvalidConvToInt));
                }

                if f >= -(i32::MIN as f64) || f <= (i32::MIN as f64 - 1f64) {
                    return Err(ExecError::Trap(Trap::IntOverflow));
                }

                Ok(f as i32)
            })?;

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
            op1_trap::<f64, i32, _>(rt, |f| {
                if f.is_nan() {
                    return Err(ExecError::Trap(Trap::InvalidConvToInt));
                }

                if f >= -(i32::MIN as f64) * 2f64 || f <= -1f64 {
                    return Err(ExecError::Trap(Trap::IntOverflow));
                }

                Ok((f as i64) as i32)
            })?;

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
            op1_trap::<f32, i64, _>(rt, |f| {
                let f = f as f64;

                if f.is_nan() {
                    return Err(ExecError::Trap(Trap::InvalidConvToInt));
                }

                if f >= -(i64::MIN as f64) || f < (i64::MIN as f64) {
                    return Err(ExecError::Trap(Trap::IntOverflow));
                }

                Ok(f as i64)
            })?;

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
            op1_trap::<f32, i64, _>(rt, |f| {
                let f = f as f64;

                if f.is_nan() {
                    return Err(ExecError::Trap(Trap::InvalidConvToInt));
                }

                if f >= -(i64::MIN as f64) * 2f64 || f <= -1f64 {
                    return Err(ExecError::Trap(Trap::IntOverflow));
                }

                Ok(if f >= -(i64::MIN as f64) {
                    ((f - 2f64.powi(63)) as i64) ^ i64::MIN
                } else {
                    f as i64
                })
            })?;

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
            op1_trap::<f64, i64, _>(rt, |f| {
                if f.is_nan() {
                    return Err(ExecError::Trap(Trap::InvalidConvToInt));
                }

                if f >= -(i64::MIN as f64) || f < (i64::MIN as f64) {
                    return Err(ExecError::Trap(Trap::IntOverflow));
                }

                Ok(f as i64)
            })?;

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
            op1_trap::<f64, i64, _>(rt, |f| {
                if f.is_nan() {
                    return Err(ExecError::Trap(Trap::InvalidConvToInt));
                }

                if f >= -(i64::MIN as f64) * 2f64 || f <= -1f64 {
                    return Err(ExecError::Trap(Trap::IntOverflow));
                }

                Ok(if f >= -(i64::MIN as f64) {
                    ((f - 2f64.powi(63)) as i64) ^ i64::MIN
                } else {
                    f as i64
                })
            })?;

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
                if i != i64::MIN && i.abs() < 0x10_0000_0000_0000 {
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
            op1::<f64, f32, _>(rt, |f| value::canonicalize_f32_nan(f as f32))?;

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
            op1::<f32, f64, _>(rt, |f| value::canonicalize_f64_nan(f as f64))?;

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

        Instruction::I32TruncSatSF32 => {
            op1::<f32, i32, _>(rt, |f| {
                if f.is_nan() {
                    0
                } else if f < i32::MIN as f32 {
                    i32::MIN
                } else if f >= -(i32::MIN as f32) {
                    i32::MAX
                } else {
                    f as i32
                }
            })?;

            // let trunc_sat_f32_s x =
            //   if F32.ne x x then
            //     0l
            //   else
            //     let xf = F32.to_float x in
            //     if xf < Int32.(to_float min_int) then
            //       Int32.min_int
            //     else if xf >= -.Int32.(to_float min_int) then
            //       Int32.max_int
            //     else
            //       Int32.of_float xf
        }

        Instruction::I32TruncSatUF32 => {
            op1::<f32, i32, _>(rt, |f| {
                if f.is_nan() {
                    0
                } else if f <= -1.0 {
                    0
                } else if f >= -(i32::MIN as f32) * 2.0f32 {
                    -1
                } else {
                    (f as i64) as i32
                }
            })?;

            // let trunc_sat_f32_u x =
            //   if F32.ne x x then
            //     0l
            //   else
            //     let xf = F32.to_float x in
            //     if xf <= -1.0 then
            //       0l
            //     else if xf >= -.Int32.(to_float min_int) *. 2.0 then
            //       -1l
            //     else
            //       Int64.(to_int32 (of_float xf))
        }

        Instruction::I32TruncSatSF64 => {
            op1::<f64, i32, _>(rt, |f| {
                if f.is_nan() {
                    0
                } else if f < i32::MIN as f64 {
                    i32::MIN
                } else if f >= -(i32::MIN as f64) {
                    i32::MAX
                } else {
                    f as i32
                }
            })?;

            // let trunc_sat_f64_s x =
            //   if F64.ne x x then
            //     0l
            //   else
            //     let xf = F64.to_float x in
            //     if xf < Int32.(to_float min_int) then
            //       Int32.min_int
            //     else if xf >= -.Int32.(to_float min_int) then
            //       Int32.max_int
            //     else
            //       Int32.of_float xf
        }

        Instruction::I32TruncSatUF64 => {
            op1::<f64, i32, _>(rt, |f| {
                if f.is_nan() {
                    0
                } else if f <= -1.0f64 {
                    0
                } else if f >= -(i32::MIN as f64) * 2.0f64 {
                    -1
                } else {
                    (f as i64) as i32
                }
            })?;

            // let trunc_sat_f64_u x =
            //   if F64.ne x x then
            //     0l
            //   else
            //     let xf = F64.to_float x in
            //     if xf <= -1.0 then
            //       0l
            //     else if xf >= -.Int32.(to_float min_int) *. 2.0 then
            //       -1l
            //     else
            //       Int64.(to_int32 (of_float xf))
        }

        Instruction::I64TruncSatSF32 => {
            op1::<f32, i64, _>(rt, |f| {
                if f.is_nan() {
                    0
                } else if f < (i64::MIN as f32) {
                    i64::MIN
                } else if f >= -(i64::MIN as f32) {
                    i64::MAX
                } else {
                    f as i64
                }
            })?;

            // let trunc_sat_f32_s x =
            //   if F32.ne x x then
            //     0L
            //   else
            //     let xf = F32.to_float x in
            //     if xf < Int64.(to_float min_int) then
            //       Int64.min_int
            //     else if xf >= -.Int64.(to_float min_int) then
            //       Int64.max_int
            //     else
            //       Int64.of_float xf
        }

        Instruction::I64TruncSatUF32 => {
            op1::<f32, i64, _>(rt, |f| {
                if f.is_nan() {
                    0
                } else if f <= -1.0f32 {
                    0
                } else if f >= -(i64::MIN as f32) * 2.0f32 {
                    -1
                } else if f >= -(i64::MIN as f32) {
                    (f - 9223372036854775808.0f32) as i64 | i64::MIN
                } else {
                    f as i64
                }
            })?;

            // let trunc_sat_f32_u x =
            //   if F32.ne x x then
            //     0L
            //   else
            //     let xf = F32.to_float x in
            //     if xf <= -1.0 then
            //       0L
            //     else if xf >= -.Int64.(to_float min_int) *. 2.0 then
            //       -1L
            //     else if xf >= -.Int64.(to_float min_int) then
            //       Int64.(logxor (of_float (xf -. 9223372036854775808.0)) min_int)
            //     else
            //       Int64.of_float xf
        }

        Instruction::I64TruncSatSF64 => {
            op1::<f64, i64, _>(rt, |f| {
                if f.is_nan() {
                    0
                } else if f < i64::MIN as f64 {
                    i64::MIN
                } else if f >= -(i64::MIN as f64) {
                    i64::MAX
                } else {
                    f as i64
                }
            })?;

            // let trunc_sat_f64_s x =
            //   if F64.ne x x then
            //     0L
            //   else
            //     let xf = F64.to_float x in
            //     if xf < Int64.(to_float min_int) then
            //       Int64.min_int
            //     else if xf >= -.Int64.(to_float min_int) then
            //       Int64.max_int
            //     else
            //       Int64.of_float xf
        }

        Instruction::I64TruncSatUF64 => {
            op1::<f64, i64, _>(rt, |f| {
                if f.is_nan() {
                    0
                } else if f <= -1.0f64 {
                    0
                } else if f >= -(i64::MIN as f64) * 2.0f64 {
                    -1
                } else if f >= -(i64::MIN as f64) {
                    (f - 9223372036854775808.0f64) as i64 | i64::MIN
                } else {
                    f as i64
                }
            })?;

            // let trunc_sat_f64_u x =
            //   if F64.ne x x then
            //     0L
            //   else
            //     let xf = F64.to_float x in
            //     if xf <= -1.0 then
            //       0L
            //     else if xf >= -.Int64.(to_float min_int) *. 2.0 then
            //       -1L
            //     else if xf >= -.Int64.(to_float min_int) then
            //       Int64.(logxor (of_float (xf -. 9223372036854775808.0)) min_int)
            //     else
            //       Int64.of_float xf
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        //                          Control flow instructions                                     //
        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::Call(fun_idx) => {
            // NB. invoke updates the ip
            invoke(rt, module_addr, FunIdx(fun_idx))?;
        }

        Instruction::CallIndirect(sig, table_ref) => {
            let elem_idx = rt.stack.pop_i32()?;

            let table_addr = rt
                .get_module(module_addr)
                .get_table(TableIdx(u32::from(table_ref)));
            let fun_addr = match rt.store.get_table(table_addr).get(elem_idx as usize) {
                Some(Some(fun_addr)) => *fun_addr,
                Some(None) => {
                    return Err(ExecError::Trap(Trap::UninitializedElement));
                }
                None => {
                    return Err(ExecError::Trap(Trap::UndefinedElement));
                }
            };

            // Check function type
            let call_instr_ty = rt.store.get_module(module_addr).get_type(TypeIdx(sig));
            let fun_module_addr = rt.store.get_fun(fun_addr).module_addr();
            let actual_fun_ty_idx = rt.store.get_fun(fun_addr).ty_idx();
            let actual_ty = &rt
                .store
                .get_module(fun_module_addr)
                .get_type(actual_fun_ty_idx);
            // NB. We can't use (==) here because of the 'form' fields of FunctionTypes. TODO:
            // replace libwasmrun_syntax.
            if call_instr_ty.params() != actual_ty.params()
                || call_instr_ty.results() != actual_ty.results()
            {
                return Err(ExecError::Trap(Trap::IndirectCallTypeMismatch));
            }

            invoke_direct(rt, fun_addr)?;
        }

        Instruction::Drop => {
            let _ = rt.stack.pop_value();
            rt.ip += 1;
        }

        Instruction::Unreachable => {
            return Err(ExecError::Trap(Trap::Unreachable));
        }

        Instruction::Block(block_ty) => {
            let (n_args, n_rets) = block_arity(rt, module_addr, block_ty);
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
            let (n_args, n_rets) = block_arity(rt, module_addr, block_ty);
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

            let (n_args, n_rets) = block_arity(rt, module_addr, block_ty);
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
        Instruction::Atomics(_)
        | Instruction::Simd(_)
        | Instruction::MemoryInit(_)
        | Instruction::MemoryDrop(_)
        | Instruction::MemoryCopy
        | Instruction::MemoryFill
        | Instruction::TableInit { .. }
        | Instruction::TableDrop(_)
        | Instruction::TableCopy
        | Instruction::RefNull(_)
        | Instruction::RefIsNull
        | Instruction::RefFunc(_) => {
            return Err(ExecError::Panic(format!(
                "Instruction not implemented: {:?}",
                instr
            )));
        }
    }

    Ok(())
}

fn trapping_add(a: u32, b: u32) -> Result<u32> {
    a.checked_add(b)
        .ok_or(ExecError::Trap(Trap::OOBMemoryAccess))
}

fn op1<A: StackValue, B: StackValue, F: Fn(A) -> B>(rt: &mut Runtime, op: F) -> Result<()> {
    op1_trap(rt, |a| Ok(op(a)))
}

fn op1_trap<A: StackValue, B: StackValue, F: Fn(A) -> Result<B>>(
    rt: &mut Runtime,
    op: F,
) -> Result<()> {
    let val = A::pop(&mut rt.stack)?;
    let ret = op(val)?;
    ret.push(&mut rt.stack);
    rt.ip += 1;
    Ok(())
}

fn op2<A: StackValue, B: StackValue, F: Fn(A, A) -> B>(rt: &mut Runtime, op: F) -> Result<()> {
    op2_trap(rt, |a, b| Ok(op(a, b)))
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

fn block_arity(rt: &Runtime, module_addr: ModuleAddr, ty: wasm::BlockType) -> (u32, u32) {
    match ty {
        wasm::BlockType::Value(_) => (0, 1),
        wasm::BlockType::NoResult => (0, 0),
        wasm::BlockType::TypeIndex(ty_idx) => {
            let ty = &rt.store.get_module(module_addr).get_type(TypeIdx(ty_idx));
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
    let current_fun = match rt.store.get_fun(current_fun_addr) {
        Fun::Wasm(fun) => fun,
        Fun::Host { .. } => {
            return Err(ExecError::Panic("ret: host function".to_string()));
        }
    };
    let module_addr = current_fun.module_addr;
    let ty_idx = current_fun.ty_idx;

    let fun_return_arity = rt
        .store
        .get_module(module_addr)
        .get_type(ty_idx)
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
