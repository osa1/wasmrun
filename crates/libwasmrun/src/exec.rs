#![allow(clippy::if_same_then_else)]

mod simd;

use crate::collections::Map;
use crate::const_eval::eval_const_expr;
use crate::export::Export;
use crate::frame::FrameStack;
use crate::fun::Fun;
use crate::mem::Mem;
use crate::module::{
    DataIdx, ElemIdx, FunIdx, GlobalIdx, MemIdx, Module, TableIdx, TagIdx, TypeIdx,
};
use crate::stack::{Block, BlockKind, EndOrBreak, Stack, StackValue};
use crate::store::{Exception, ExnAddr, FunAddr, Global, ModuleAddr, Store, Table};
use crate::types::TypeCanonicalizer;
use crate::value::{self, Ref, Value};
use crate::wasi::allocate_wasi;
use crate::HostFunDecl;
use crate::{ExecError, Result};

use ieee754::Ieee754;
use libwasmrun_syntax as wasm;
use wasi_common::{WasiCtx, WasiCtxBuilder};
use wasm::{Instruction, MemArg, SignExtInstruction};

use std::fmt;
use std::iter;
use std::mem::take;
use std::rc::Rc;

pub(crate) const PAGE_SIZE: usize = 65536;
pub(crate) const MAX_PAGES: u32 = 65536; // (2**32 - 1 / PAGE_SIZE), or 0x10000

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Trap {
    /// Undefined table element called.
    UndefinedElement,

    /// Uninitialized table element called.
    UninitializedElement,

    /// Indirect function call target doesn't have expected type.
    IndirectCallTypeMismatch,

    /// Element out of bounds.
    ElementOOB,

    /// Out of bounds memory access.
    OOBMemoryAccess,

    /// Out of bounds table access.
    OOBTableAccess,

    /// Integer divide by zero.
    IntDivideByZero,

    /// Integer overflow.
    IntOverflow,

    /// Invalid conversion to integer.
    InvalidConvToInt,

    /// 'unreachable' instruction executed.
    Unreachable,

    /// `call_indirect` called with an extern reference table.
    CallIndirectOnExternRef,

    /// Null reference in `call_ref` or `return_call_ref`.
    NullFunction,

    /// Null reference in `ref_as_non_null`.
    NullReference,

    /// Null reference in a struct instruction.
    NullStructReference,

    /// Null reference in an i31 instruction.
    NullI31Reference,
}

impl fmt::Display for Trap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Trap::UndefinedElement => "undefined element".fmt(f),
            Trap::UninitializedElement => "uninitialized element".fmt(f),
            Trap::IndirectCallTypeMismatch => "indirect call type mismatch".fmt(f),
            Trap::ElementOOB => "element out of bounds".fmt(f),
            Trap::OOBMemoryAccess => "out of bound memory access".fmt(f),
            Trap::OOBTableAccess => "out of bound table access".fmt(f),
            Trap::IntDivideByZero => "divide by zero".fmt(f),
            Trap::IntOverflow => "integer overflow".fmt(f),
            Trap::InvalidConvToInt => "invalid conversion to integer".fmt(f),
            Trap::Unreachable => "unreachable executed".fmt(f),
            Trap::CallIndirectOnExternRef => "`call_indirect` with an extern ref table".fmt(f),
            Trap::NullFunction => "null reference in `call_ref` or `return_call_ref`".fmt(f),
            Trap::NullReference => "null reference in `ref_as_non_null`".fmt(f),
            Trap::NullStructReference => "null struct reference in a struct instruction".fmt(f),
            Trap::NullI31Reference => "null reference in an i31 instruction".fmt(f),
        }
    }
}

pub struct Runtime {
    /// Stores modules, functions, tables etc.
    pub store: Store,

    /// Value and continuation stack.
    pub(crate) stack: Stack,

    /// Call stack. Frames hold locals.
    pub(crate) frames: FrameStack,

    /// WASI state.
    pub(crate) wasi_ctx: WasiCtx,

    /// Maps registered modules to their module addresses.
    module_names: Map<String, ModuleAddr>,

    /// Instruction pointer in the current function.
    ip: u32,

    /// The exception when the execution ends with an unhandled exception.
    pub unhandled_exception: Option<ExnAddr>,

    #[allow(unused)]
    type_canonicalizer: TypeCanonicalizer,
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
            unhandled_exception: None,
            type_canonicalizer: TypeCanonicalizer::default(),
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

        let mut module_names: Map<String, ModuleAddr> = Default::default();
        module_names.insert("wasi_snapshot_preview1".to_owned(), wasi_addr);
        module_names.insert("wasi_unstable".to_owned(), wasi_addr);

        Runtime {
            store,
            wasi_ctx,
            module_names,
            ..Runtime::new()
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

    // Used in spec tests to resolve `ref.func` arguments to test functions
    pub fn get_module_fun_addr(&self, module_addr: ModuleAddr, fun_idx: u32) -> FunAddr {
        self.store.get_module(module_addr).get_fun(FunIdx(fun_idx))
    }

    pub fn pop_value(&mut self) -> Option<Value> {
        self.stack.pop_value_opt().unwrap()
    }

    pub fn pop_i32(&mut self) -> Result<i32> {
        self.stack.pop_i32()
    }

    pub fn pop_ref(&mut self) -> Result<Ref> {
        self.stack.pop_ref()
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

            let ty_idx = module.add_type(wasm::CompType::Func(wasm::FunctionType::new(
                arg_tys, ret_tys,
            )));
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

    let table_addr = rt.store.allocate_table(Table::new(
        Ref::Null(wasm::HeapType::Func),
        wasm::TableType::new(wasm::ReferenceType::funcref(), 10, None),
    ));
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

    let print_ty = module.add_type(wasm::CompType::Func(wasm::FunctionType::new(
        vec![],
        vec![],
    )));
    let print_addr = rt
        .store
        .allocate_host_fun(module_addr, print_ty, Rc::new(|_, _| Ok(vec![])));
    let print_idx = module.add_fun(print_addr);
    module.add_export(Export::new_fun("print".to_owned(), print_idx));

    let print_i32_ty = module.add_type(wasm::CompType::Func(wasm::FunctionType::new(
        vec![wasm::ValueType::I32],
        vec![],
    )));
    let print_i32_addr =
        rt.store
            .allocate_host_fun(module_addr, print_i32_ty, Rc::new(|_, _| Ok(vec![])));
    let print_i32_idx = module.add_fun(print_i32_addr);
    module.add_export(Export::new_fun("print_i32".to_owned(), print_i32_idx));

    let print_i64_ty = module.add_type(wasm::CompType::Func(wasm::FunctionType::new(
        vec![wasm::ValueType::I64],
        vec![],
    )));
    let print_i64_addr =
        rt.store
            .allocate_host_fun(module_addr, print_i64_ty, Rc::new(|_, _| Ok(vec![])));
    let print_i64_idx = module.add_fun(print_i64_addr);
    module.add_export(Export::new_fun("print_i64".to_owned(), print_i64_idx));

    let print_i32_f32_ty = module.add_type(wasm::CompType::Func(wasm::FunctionType::new(
        vec![wasm::ValueType::I32, wasm::ValueType::F32],
        vec![],
    )));
    let print_i32_f32_addr =
        rt.store
            .allocate_host_fun(module_addr, print_i32_f32_ty, Rc::new(|_, _| Ok(vec![])));
    let print_i32_f32_idx = module.add_fun(print_i32_f32_addr);
    module.add_export(Export::new_fun(
        "print_i32_f32".to_owned(),
        print_i32_f32_idx,
    ));

    let print_f64_f64_ty = module.add_type(wasm::CompType::Func(wasm::FunctionType::new(
        vec![wasm::ValueType::F64, wasm::ValueType::F64],
        vec![],
    )));
    let print_f64_f64_addr =
        rt.store
            .allocate_host_fun(module_addr, print_f64_f64_ty, Rc::new(|_, _| Ok(vec![])));
    let print_f64_f64_idx = module.add_fun(print_f64_f64_addr);
    module.add_export(Export::new_fun(
        "print_f64_f64".to_owned(),
        print_f64_f64_idx,
    ));

    let print_f32_ty = module.add_type(wasm::CompType::Func(wasm::FunctionType::new(
        vec![wasm::ValueType::F32],
        vec![],
    )));
    let print_f32_addr =
        rt.store
            .allocate_host_fun(module_addr, print_f32_ty, Rc::new(|_, _| Ok(vec![])));
    let print_f32_idx = module.add_fun(print_f32_addr);
    module.add_export(Export::new_fun("print_f32".to_owned(), print_f32_idx));

    let print_f64_ty = module.add_type(wasm::CompType::Func(wasm::FunctionType::new(
        vec![wasm::ValueType::F64],
        vec![],
    )));
    let print_f64_addr =
        rt.store
            .allocate_host_fun(module_addr, print_f64_ty, Rc::new(|_, _| Ok(vec![])));
    let print_f64_idx = module.add_fun(print_f64_addr);
    module.add_export(Export::new_fun("print_f64".to_owned(), print_f64_idx));

    rt.store.allocate_module(module);
    rt.module_names.insert("spectest".to_string(), module_addr);
}

pub fn instantiate(rt: &mut Runtime, parsed_module: wasm::Module) -> Result<ModuleAddr> {
    // https://webassembly.github.io/spec/core/exec/modules.html
    let mut parsed_module = match parsed_module.parse_names() {
        Ok(m) => m,
        Err((_, m)) => m,
    };

    let module_addr = rt.store.allocate_module(Module::default());

    if let Some(type_section) = parsed_module.type_section_mut() {
        for rec in type_section.entries_mut().drain(..) {
            for sub_ty in rec.tys {
                rt.store
                    .get_module_mut(module_addr)
                    .add_type(sub_ty.comp_ty);
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
                None => exec_panic!("Can't find imported module {:?}", module_name),
            };
            let imported_module = rt.store.get_module(imported_module_addr);

            match import.external() {
                wasm::External::Function(_fun_ty_idx) => {
                    n_funs += 1;
                    let fun_addr = imported_module.get_exported_fun(field_name).unwrap();
                    rt.store.get_module_mut(module_addr).add_fun(fun_addr);
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
                    rt.store.get_module_mut(module_addr).add_table(tbl_addr);
                }
                wasm::External::Memory(_mem_ty) => {
                    let mem_addr = imported_module.get_exported_mem(field_name).unwrap();
                    rt.store.get_module_mut(module_addr).add_mem(mem_addr);
                }
                wasm::External::Global(_gbl_ty) => {
                    let global_addr = imported_module.get_exported_global(field_name).unwrap();
                    rt.store.get_module_mut(module_addr).add_global(global_addr);
                }
                wasm::External::Tag(_ty_idx) => {
                    let tag_addr = imported_module.get_exported_tag(field_name).unwrap();
                    rt.store.get_module_mut(module_addr).add_tag(tag_addr);
                }
            }
        }
    }

    // Allocate tags
    if let Some(tag_section) = parsed_module.tag_section_mut() {
        for tag in tag_section.entries_mut().drain(..) {
            let tag_type = rt
                .store
                .get_module(module_addr)
                .get_type(TypeIdx(tag.type_))
                .as_function_type()
                .unwrap();
            let tag_addr = rt.store.allocate_tag(tag_type.clone());
            rt.store.get_module_mut(module_addr).add_tag(tag_addr);
        }
    }

    // Allocate functions
    if let Some(code_section) = parsed_module.code_section_mut().take() {
        for (fun_idx, fun) in take(code_section.entries_mut()).into_iter().enumerate() {
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

            rt.store.get_module_mut(module_addr).add_fun(fun_addr);
        }
    }

    // Allocate tables
    if let Some(table_section) = parsed_module.table_section_mut() {
        for wasm::Table { ty, init } in table_section.entries_mut().drain(..) {
            let elem = match init {
                Some(init) => eval_const_expr(rt, module_addr, init.code())?.expect_ref(),
                None => Ref::Null(ty.elem_type.heap_ty),
            };
            let table = Table::new(elem, ty);
            let table_addr = rt.store.allocate_table(table);
            rt.store.get_module_mut(module_addr).add_table(table_addr);
        }
    }

    // Allocate memories
    if let Some(memory_section) = parsed_module.memory_section_mut() {
        for mem in memory_section.entries_mut().drain(..) {
            let (initial, max) = match mem.limits() {
                wasm::Limits::Limits32(limits) => (limits.initial(), limits.maximum()),
                wasm::Limits::Limits64(_) => exec_panic!("64-bit memories not implements"),
            };
            let mem_addr = rt.store.allocate_mem(Mem::new(initial, max));
            rt.store.get_module_mut(module_addr).add_mem(mem_addr);
        }
    }

    // Allcoate globals
    if let Some(global_section) = parsed_module.global_section_mut() {
        for global in global_section.entries_mut().drain(..) {
            let value = eval_const_expr(rt, module_addr, global.init_expr().code())?;
            let global_addr = rt.store.allocate_global(Global {
                value,
                mutable: global.global_type().is_mutable(),
            });
            rt.store.get_module_mut(module_addr).add_global(global_addr);
        }
    }

    // Allocate table elements
    if let Some(element_section) = parsed_module.elements_section() {
        for elem @ wasm::ElementSegment {
            ref_type: _,
            init,
            mode,
        } in element_section.entries()
        {
            let elem_addr = rt.store.allocate_elem(elem.clone());
            rt.store.get_module_mut(module_addr).add_elem(elem_addr);

            // https://webassembly.github.io/spec/core/syntax/modules.html#syntax-elem
            match mode {
                wasm::ElementSegmentMode::Active { table_idx, offset } => {
                    // Spec: "An active element segment copies its elements into a table during
                    // instantiation, as specified by a table index and a constant expression
                    // defining an offset into that table."
                    let offset =
                        eval_const_expr(rt, module_addr, offset.code())?.expect_i32() as usize;
                    let table_addr = rt
                        .store
                        .get_module(module_addr)
                        .get_table(TableIdx(*table_idx));
                    let table_len = rt.store.get_table(table_addr).len();

                    // table.init
                    let oob_dst = match offset.checked_add(init.len()) {
                        Some(dst) => dst > table_len,
                        None => true,
                    };

                    if oob_dst {
                        return Err(ExecError::Trap(Trap::OOBTableAccess));
                    }

                    for (elem_idx, elem) in init.iter().enumerate() {
                        let elem_idx = offset + elem_idx;
                        if elem_idx >= table_len {
                            return Err(ExecError::Trap(Trap::ElementOOB));
                        }
                        let elem = match elem.code() {
                            &[Instruction::RefFunc(func_idx), Instruction::End] => Ref::Func(
                                rt.store.get_module(module_addr).get_fun(FunIdx(func_idx)),
                            ),
                            &[Instruction::RefNull(heap_ty), Instruction::End] => {
                                Ref::Null(heap_ty)
                            }
                            &[Instruction::GetGlobal(global_idx), Instruction::End] => {
                                let global_addr = rt
                                    .store
                                    .get_module(module_addr)
                                    .get_global(GlobalIdx(global_idx));
                                match &rt.store.get_global(global_addr).value {
                                    Value::Ref(ref_) => *ref_,
                                    other => exec_panic!(
                                        "Element get.global value is not a reference: {:?}",
                                        other
                                    ),
                                }
                            }
                            other => exec_panic!("Unhandled element expression: {:?}", other),
                        };
                        rt.store.get_table_mut(table_addr).set(elem_idx, elem)?;

                        // elem.drop
                        rt.store.get_elem_mut(elem_addr).init.clear();
                    }
                }
                wasm::ElementSegmentMode::Passive => {
                    // Spec: "A passive element segment’s elements can be copied to a table using
                    // the `table.init` instruction."
                    // I think we just store it somewhere so that it can be used in `table.init`?
                }
                wasm::ElementSegmentMode::Declarative => {
                    // Spec: "A declarative element segment is not available at runtime but merely
                    // serves to forward-declare references that are formed in code with
                    // instructions like `ref.func`".
                    // For now just clear the elements to make it unavailable at runtime. We should
                    // probably just allocate a placeholder for declarative and active segments.
                    rt.store.get_elem_mut(elem_addr).init.clear();
                }
            }
        }
    }

    // Get names of functions
    // NB. This is not used for calling functions as we can only call exported functions, which
    // already have names in the export section.
    if let Some(names_section) = parsed_module.names_section_mut() {
        if let Some(function_names) = names_section.functions_mut() {
            for (fun_idx, fun_name) in take(function_names.names_mut()).into_iter() {
                rt.store
                    .get_module_mut(module_addr)
                    .add_fun_name(fun_name, FunIdx(fun_idx));
            }
        }
    }

    // Initialize memories with 'data' section
    if let Some(data_section) = parsed_module.data_section() {
        for seg @ wasm::DataSegment { data, mode } in data_section.entries() {
            // TODO: I think active segments don't need to be store in the module instance or heap,
            // but they get an index. If this is the case then we don't need to store `DataSegment`
            // in the module instance, just store an empty vec for active segments and store
            // passive segment data.
            let data_addr = rt.store.allocate_data(seg.clone());
            rt.store.get_module_mut(module_addr).add_data(data_addr);
            match mode {
                wasm::DataSegmentMode::Passive => {}
                wasm::DataSegmentMode::Active { mem_idx, offset } => {
                    let offset =
                        eval_const_expr(rt, module_addr, offset.code())?.expect_i32() as u32;
                    let mem_addr = rt
                        .store
                        .get_module(module_addr)
                        .get_mem(MemIdx(*mem_idx as u32));
                    let mem = &mut rt.store.get_mem_mut(mem_addr);

                    // memory.init amt=`data.len()` src=0 dst=`offset`
                    let amt = data.len();
                    let src = 0;
                    let dst = offset as usize;

                    if src + amt > data.len() || dst + amt > mem.len() {
                        return Err(ExecError::Trap(Trap::OOBMemoryAccess));
                    }

                    mem.set_range(offset, data)?;

                    // data.drop
                    rt.store.get_data_mut(data_addr).data.clear();
                }
            }
        }
    }

    // Initialize exports
    if let Some(export_section) = parsed_module.export_section_mut() {
        for mut export in export_section.entries_mut().drain(..) {
            let export_field = take(export.field_mut());
            let export = match export.internal() {
                wasm::Internal::Function(fun_idx) => {
                    let idx = FunIdx(*fun_idx);
                    rt.store
                        .get_module_mut(module_addr)
                        .add_fun_name(export_field.clone(), idx);
                    Export::new_fun(export_field, idx)
                }
                wasm::Internal::Table(table_idx) => {
                    Export::new_table(export_field, TableIdx(*table_idx))
                }
                wasm::Internal::Memory(mem_idx) => Export::new_mem(export_field, MemIdx(*mem_idx)),
                wasm::Internal::Global(global_idx) => {
                    Export::new_global(export_field, GlobalIdx(*global_idx))
                }
                wasm::Internal::Tag(tag_idx) => Export::new_tag(export_field, TagIdx(*tag_idx)),
            };
            rt.store.get_module_mut(module_addr).add_export(export);
        }
    }

    // Set start
    if let Some(start_idx) = parsed_module.start_section() {
        rt.store
            .get_module_mut(module_addr)
            .set_start(FunIdx(start_idx));
    }

    let start = rt.store.get_module(module_addr).get_start();

    if let Some(start_idx) = start {
        invoke(rt, module_addr, start_idx)?;
        finish(rt)?;
    }

    Ok(module_addr)
}

pub fn invoke_by_name(rt: &mut Runtime, module_addr: ModuleAddr, fun_name: &str) -> Result<()> {
    match rt.store.get_module(module_addr).get_fun_name(fun_name) {
        None => exec_panic!("Unknown function: {}", fun_name),
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
        .get_type(fun.ty_idx())
        .as_function_type()
        .unwrap();

    let n_args = fun_ty.params().len();
    let n_rets = fun_ty.results().len();

    // Pop the arguments
    let mut args: Vec<Value> = Vec::with_capacity(n_args);
    for _ in 0..n_args {
        args.push(rt.stack.pop_value()?);
    }

    args.reverse();
    rt.frames.push(fun, args);

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

/// Same as `invoke_direct`, but does a tail call.
fn tail_call(rt: &mut Runtime, fun_addr: FunAddr) -> Result<()> {
    let callee_fun = rt.store.get_fun(fun_addr);
    let callee_fun_ty = rt
        .store
        .get_module(callee_fun.module_addr())
        .get_type(callee_fun.ty_idx())
        .as_function_type()
        .unwrap();
    let callee_n_args = callee_fun_ty.params().len();
    let callee_n_rets = callee_fun_ty.results().len();

    // Pop the arguments
    let mut args: Vec<Value> = Vec::with_capacity(callee_n_args);
    for _ in 0..callee_n_args {
        args.push(rt.stack.pop_value()?);
    }

    // Pop the current frame
    let caller_frame = rt.stack.pop_fun_block()?;
    rt.frames.pop();
    debug_assert_eq!(callee_n_rets, caller_frame.n_rets as usize);

    // Push the new frame
    args.reverse();
    rt.frames.push(callee_fun, args);

    rt.stack.push_fun_block(
        caller_frame.cont,
        callee_n_args as u32,
        callee_n_rets as u32,
    );

    rt.ip = 0;

    Ok(())
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
        Fun::Host { .. } => exec_panic!("single_step: host function"),
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
        ////////////////////////////////////////////////////////////////////////////////////////////
        // Memory instructions
        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::MemoryGrow(mem_idx) => {
            let mem_addr = rt
                .store
                .get_module(module_addr)
                .get_mem(MemIdx(u32::from(mem_idx)));
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

        Instruction::MemorySize(mem_idx) => {
            let mem_addr = rt
                .store
                .get_module(module_addr)
                .get_mem(MemIdx(u32::from(mem_idx)));
            let mem = rt.store.get_mem(mem_addr);
            rt.stack.push_i32(mem.size_pages() as i32)?;
            rt.ip += 1;
        }

        Instruction::MemoryInit(mem_idx, data_idx) => {
            let mem_addr = rt
                .store
                .get_module(module_addr)
                .get_mem(MemIdx(u32::from(mem_idx)));
            let mem = rt.store.get_mem(mem_addr);

            let amt = rt.stack.pop_i32()? as usize;
            let src = rt.stack.pop_i32()? as usize;
            let dst = rt.stack.pop_i32()? as usize;

            // println!(
            //     "memory_init data_idx={} amt={} src={} dst={}",
            //     data_idx, amt, src, dst
            // );

            let data_addr = rt.get_module(module_addr).get_data(DataIdx(data_idx));
            let data = rt.store.get_data(data_addr);

            // TODO: Active data segments can also be used after instantiation, but I think (?) the
            // offset expression is not used. Maybe store just the data?
            // assert_eq!(data.mode, wasm::DataSegmentMode::Passive);

            let data = &data.data;

            if src + amt > data.len() || dst + amt > mem.len() {
                return Err(ExecError::Trap(Trap::OOBMemoryAccess));
            }

            let data = data.to_vec(); // avoid aliasing

            // println!("memory_init data={:?}", data);

            let mem = rt.store.get_mem_mut(mem_addr);
            mem.set_range(dst as u32, &data[src..src + amt])?;

            rt.ip += 1;
        }

        Instruction::DataDrop(data_idx) => {
            let data_addr = rt.get_module(module_addr).get_data(DataIdx(data_idx));
            let data = rt.store.get_data_mut(data_addr);
            data.data.clear();
            rt.ip += 1;
        }

        Instruction::MemoryCopy(dst_mem_idx, src_mem_idx) => {
            let amt = rt.stack.pop_i32()? as usize;
            let src = rt.stack.pop_i32()? as usize;
            let dst = rt.stack.pop_i32()? as usize;

            let src_mem_addr = rt
                .get_module(module_addr)
                .get_mem(MemIdx(u32::from(src_mem_idx)));

            let dst_mem_addr = rt
                .get_module(module_addr)
                .get_mem(MemIdx(u32::from(dst_mem_idx)));

            // let src_mem = rt.store.get_mem_mut(src_mem_addr);

            let oob = match (src.checked_add(amt), dst.checked_add(amt)) {
                (None, _) | (_, None) => true,
                (Some(src_end), Some(dst_end)) => {
                    src_end > rt.store.get_mem_mut(src_mem_addr).len()
                        || dst_end > rt.store.get_mem_mut(dst_mem_addr).len()
                }
            };

            if oob {
                return Err(ExecError::Trap(Trap::OOBMemoryAccess));
            }

            let src_mem = rt.store.get_mem_mut(src_mem_addr);
            let src_data = src_mem.mem[src..src + amt].to_vec();
            let dst_mem = rt.store.get_mem_mut(dst_mem_addr);
            dst_mem.set_range(dst as u32, &src_data)?;

            rt.ip += 1;
        }

        Instruction::MemoryFill(mem_idx) => {
            let amt = rt.stack.pop_i32()? as usize;
            let val = rt.stack.pop_i32()? as u8;
            let dst = rt.stack.pop_i32()? as usize;

            let mem_addr = rt
                .get_module(module_addr)
                .get_mem(MemIdx(u32::from(mem_idx)));
            let mem = rt.store.get_mem_mut(mem_addr);

            let oob = match dst.checked_add(amt) {
                Some(copy_end) => copy_end > mem.len(),
                None => true,
            };

            if oob {
                return Err(ExecError::Trap(Trap::OOBMemoryAccess));
            }

            for i in 0..amt {
                mem.mem[dst + i] = val;
            }

            rt.ip += 1;
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::Nop => {
            rt.ip += 1;
        }

        Instruction::Select(_) => {
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

        Instruction::I32Store(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let value = rt.stack.pop_i32()?;
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_32_le(addr, value as u32)?;

            rt.ip += 1;
        }

        Instruction::F32Store(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let value = rt.stack.pop_f32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_32_le(addr, value.to_bits())?;

            rt.ip += 1;
        }

        Instruction::I64Store(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let value = rt.stack.pop_i64()?;
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_64_le(addr, value as u64)?;

            rt.ip += 1;
        }

        Instruction::F64Store(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let value = rt.stack.pop_f64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_64_le(addr, value.to_bits())?;

            rt.ip += 1;
        }

        Instruction::I64Store8(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_8(addr, c as u8)?;

            rt.ip += 1;
        }

        Instruction::I64Store16(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_16_le(addr, c as u16)?;

            rt.ip += 1;
        }

        Instruction::I64Store32(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let c = rt.stack.pop_i64()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_32_le(addr, c as u32)?;

            rt.ip += 1;
        }

        Instruction::I64Load8S(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let val = mem.load_8(addr)?;
            rt.stack.push_i64(val as i8 as i64)?;

            rt.ip += 1;
        }

        Instruction::I32Load(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            rt.stack.push_i32(mem.load_32_le(addr)? as i32)?;

            rt.ip += 1;
        }

        Instruction::F32Load(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            rt.stack.push_f32(f32::from_bits(mem.load_32_le(addr)?))?;

            rt.ip += 1;
        }

        Instruction::I64Load(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let val = mem.load_64_le(addr)?;
            rt.stack.push_i64(val as i64)?;

            rt.ip += 1;
        }

        Instruction::F64Load(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let val = mem.load_64_le(addr)?;
            rt.stack.push_f64(f64::from_bits(val))?;

            rt.ip += 1;
        }

        Instruction::I32Load8U(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let b = mem.load_8(addr)?;
            rt.stack.push_i32(b as i32)?;

            rt.ip += 1;
        }

        Instruction::I32Load8S(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let val = mem.load_8(addr)?;
            rt.stack.push_i32(val as i8 as i32)?;

            rt.ip += 1;
        }

        Instruction::I64Load8U(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let b = mem.load_8(addr)?;
            rt.stack.push_i64(b as i64)?;

            rt.ip += 1;
        }

        Instruction::I32Load16U(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let val = mem.load_16_le(addr)?;
            rt.stack.push_i32(val as i32)?;

            rt.ip += 1;
        }

        Instruction::I32Load16S(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let val = mem.load_16_le(addr)?;
            rt.stack.push_i32(val as i16 as i32)?;

            rt.ip += 1;
        }

        Instruction::I64Load16U(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let val = mem.load_16_le(addr)?;
            rt.stack.push_i64(val as i64)?;

            rt.ip += 1;
        }

        Instruction::I64Load16S(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            let val = mem.load_16_le(addr)?;
            rt.stack.push_i64(val as i16 as i64)?;

            rt.ip += 1;
        }

        Instruction::I64Load32U(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            rt.stack.push_i64(mem.load_32_le(addr)? as i64)?;

            rt.ip += 1;
        }

        Instruction::I64Load32S(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem(mem_addr);

            rt.stack.push_i64((mem.load_32_le(addr)? as i32) as i64)?;

            rt.ip += 1;
        }

        Instruction::I32Store8(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let c = rt.stack.pop_i32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_8(addr, c as u8)?;

            rt.ip += 1;
        }

        Instruction::I32Store16(MemArg {
            align: _,
            offset,
            mem_idx,
        }) => {
            let c = rt.stack.pop_i32()?;

            let addr = rt.stack.pop_i32()? as u32;
            let addr = trapping_add(addr, offset)?;

            let mem_addr = rt.store.get_module(module_addr).get_mem(MemIdx(mem_idx));
            let mem = rt.store.get_mem_mut(mem_addr);

            mem.store_16_le(addr, c as u16)?;

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
            op1::<i32, i32, _>(rt, |i| i.count_ones() as i32)?;
        }

        Instruction::I64Popcnt => {
            op1::<i64, i64, _>(rt, |i| i.count_ones() as i64)?;
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
            op1::<i64, i32, _>(rt, |i| i as i32)?;
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
            op2::<f32, f32, _>(rt, f32_max)?;

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
            op2::<f64, f64, _>(rt, f64_max)?;
        }

        Instruction::F32Min => {
            op2::<f32, f32, _>(rt, f32_min)?;

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
            op2::<f64, f64, _>(rt, f64_min)?;
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
            op1::<f32, f32, _>(rt, f32_nearest)?;
        }

        Instruction::F64Nearest => {
            op1::<f64, f64, _>(rt, f64_nearest)?;
        }

        Instruction::F32Copysign => {
            op2::<f32, f32, _>(rt, f32::copysign)?;
        }

        Instruction::F64Copysign => {
            op2::<f64, f64, _>(rt, f64::copysign)?;
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
            op1::<i32, f32, _>(rt, f32_convert_u_i32)?;

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
            op1::<f32, i32, _>(rt, i32_trunc_sat_s_f32)?;
        }

        Instruction::I32TruncSatUF32 => {
            op1::<f32, i32, _>(rt, i32_trunc_sat_u_f32)?;
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
            op1::<f64, i32, _>(rt, i32_trunc_sat_s_f64)?;
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
            let fun_addr = rt.store.get_module(module_addr).get_fun(FunIdx(fun_idx));
            invoke_direct(rt, fun_addr)?;
        }

        Instruction::CallIndirect(type_idx, table_ref) => {
            let elem_idx = rt.stack.pop_i32()?;
            let fun_addr = get_fun_addr_from_table(rt, module_addr, elem_idx, table_ref)?;

            if !check_fun_type(rt, type_idx, module_addr, fun_addr) {
                return Err(ExecError::Trap(Trap::IndirectCallTypeMismatch));
            }

            invoke_direct(rt, fun_addr)?;
        }

        Instruction::ReturnCall(fun_idx) => {
            let fun_addr = rt.store.get_module(module_addr).get_fun(FunIdx(fun_idx));
            tail_call(rt, fun_addr)?;
        }

        Instruction::ReturnCallIndirect(type_idx, table_ref) => {
            let elem_idx = rt.stack.pop_i32()?;
            let fun_addr = get_fun_addr_from_table(rt, module_addr, elem_idx, table_ref)?;

            if !check_fun_type(rt, type_idx, module_addr, fun_addr) {
                return Err(ExecError::Trap(Trap::IndirectCallTypeMismatch));
            }

            tail_call(rt, fun_addr)?;
        }

        Instruction::CallRef(type_idx) => {
            let ref_ = rt.stack.pop_ref()?;
            let fun_addr = match ref_ {
                Ref::Extern(_) | Ref::Exn(_) | Ref::Struct(_) | Ref::Array(_) | Ref::I31(_) => {
                    exec_panic!("call_ref: reference is not a function")
                }
                Ref::Null(_) => return Err(ExecError::Trap(Trap::NullFunction)),
                Ref::Func(fun_addr) => fun_addr,
            };
            if !check_fun_type(rt, type_idx, module_addr, fun_addr) {
                return Err(ExecError::Trap(Trap::IndirectCallTypeMismatch));
            }
            invoke_direct(rt, fun_addr)?;
        }

        Instruction::ReturnCallRef(type_idx) => {
            let ref_ = rt.stack.pop_ref()?;
            let fun_addr = match ref_ {
                Ref::Extern(_) | Ref::Exn(_) | Ref::Struct(_) | Ref::Array(_) | Ref::I31(_) => {
                    exec_panic!("return_call_ref: reference is not a function")
                }
                Ref::Null(_) => return Err(ExecError::Trap(Trap::NullFunction)),
                Ref::Func(fun_addr) => fun_addr,
            };
            if !check_fun_type(rt, type_idx, module_addr, fun_addr) {
                return Err(ExecError::Trap(Trap::IndirectCallTypeMismatch));
            }
            tail_call(rt, fun_addr)?;
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

            let end_ip = match current_fun.block_to_end.get(&rt.ip) {
                None => exec_panic!("Couldn't find continuation of block"),
                Some(end) => *end,
            };

            rt.stack.push_block(end_ip + 1, n_args, n_rets);

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

            let end_ip = match current_fun.block_to_end.get(&rt.ip) {
                None => exec_panic!("Couldn't find continuation of if"),
                Some(end_ip) => *end_ip,
            };

            rt.stack.push_block(end_ip + 1, n_args, n_rets);
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
                        rt.ip = end_ip;
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
            // Kinda like 'br(0)', but also works as 'return'. When returning from a function we
            // use the continuation of the frame. Otherwise we bump `ip` by one.

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
                .as_function_type()
                .unwrap()
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
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Table instructions
        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::TableGet(table_idx) => {
            let table_addr = rt.get_module(module_addr).get_table(TableIdx(table_idx));
            let table = rt.store.get_table(table_addr);
            let elem_idx = rt.stack.pop_i32()?;
            let elem = match table.get(elem_idx as usize) {
                Some(elem) => elem,
                None => return Err(ExecError::Trap(Trap::OOBTableAccess)),
            };
            rt.stack.push_value(Value::Ref(*elem))?;
            rt.ip += 1;
        }

        Instruction::TableSet(table_idx) => {
            let table_addr = rt.get_module(module_addr).get_table(TableIdx(table_idx));
            let table = rt.store.get_table_mut(table_addr);
            let value = rt.stack.pop_ref()?;
            let elem_idx = rt.stack.pop_i32()?;
            table.set(elem_idx as usize, value)?;
            rt.ip += 1;
        }

        Instruction::TableSize(table_idx) => {
            let table_addr = rt.get_module(module_addr).get_table(TableIdx(table_idx));
            let table = rt.store.get_table_mut(table_addr);
            let len = table.len();
            rt.stack.push_value(Value::I32(len as i32))?;
            rt.ip += 1;
        }

        Instruction::TableGrow(table_idx) => {
            let table_addr = rt.get_module(module_addr).get_table(TableIdx(table_idx));
            let table = rt.store.get_table_mut(table_addr);
            let n = rt.stack.pop_i32()?;
            let val = rt.stack.pop_ref()?;
            let ret = match table.grow(n as usize, val) {
                Some(old_size) => old_size as i32,
                None => -1,
            };
            rt.stack.push_value(Value::I32(ret))?;
            rt.ip += 1;
        }

        Instruction::TableFill(table_idx) => {
            let table_addr = rt.get_module(module_addr).get_table(TableIdx(table_idx));
            let table = rt.store.get_table_mut(table_addr);

            // Starting from `idx`, set `amt` elements to`val`
            let amt = rt.stack.pop_i32()?;
            let val = rt.stack.pop_ref()?;
            let idx = rt.stack.pop_i32()?;
            if !table.fill(idx as usize, amt as usize, val) {
                return Err(ExecError::Trap(Trap::OOBTableAccess));
            }
            rt.ip += 1;
        }

        Instruction::TableCopy(dst, src) => {
            // Elements first copied into an intermediate vector to avoid borrow check issues
            // (aliasing). Note that `dst` and `src` can be the same table.
            let src = rt
                .store
                .get_table(rt.get_module(module_addr).get_table(TableIdx(src)));

            // Copy `amt` elements from `src[src_idx]` to `dst[dst_idx]`
            let amt = rt.stack.pop_i32()? as usize;
            let src_idx = rt.stack.pop_i32()? as usize;
            let dst_idx = rt.stack.pop_i32()? as usize;

            {
                let oob = match src_idx.checked_add(amt) {
                    Some(src_idx_) => src_idx_ > src.len(),
                    None => true,
                };

                if oob {
                    return Err(ExecError::Trap(Trap::OOBTableAccess));
                }
            }

            let mut elems: Vec<Ref> = Vec::with_capacity(amt);
            for i in src_idx..src_idx + amt {
                elems.push(*src.get(i).unwrap()); // bounds already checked
            }

            let dst = rt
                .store
                .get_table_mut(rt.get_module(module_addr).get_table(TableIdx(dst)));

            {
                let oob = match dst_idx.checked_add(amt) {
                    Some(dst_idx_) => dst_idx_ > dst.len(),
                    None => true,
                };

                if oob {
                    return Err(ExecError::Trap(Trap::OOBTableAccess));
                }
            }

            for (elem_idx, elem) in elems.into_iter().enumerate() {
                let ret = dst.set(dst_idx + elem_idx, elem); // bounds already checked
                debug_assert_eq!(ret, Ok(()));
            }

            rt.ip += 1;
        }

        Instruction::TableInit {
            elem_idx,
            table_idx,
        } => {
            let table_addr = rt.get_module(module_addr).get_table(TableIdx(table_idx));
            let table = rt.store.get_table(table_addr);

            let elem_addr = rt.get_module(module_addr).get_elem(ElemIdx(elem_idx));
            let elem = rt.store.get_elem(elem_addr);

            let amt = rt.stack.pop_i32()? as usize;
            let src = rt.stack.pop_i32()? as usize;
            let dst = rt.stack.pop_i32()? as usize;

            let oob_src = match src.checked_add(amt) {
                Some(src) => src > elem.init.len(),
                None => true,
            };

            let oob_dst = match dst.checked_add(amt) {
                Some(dst) => dst > table.len(),
                None => true,
            };

            if oob_src || oob_dst {
                return Err(ExecError::Trap(Trap::OOBTableAccess));
            }

            let mut fun_addrs: Vec<Ref> = Vec::with_capacity(amt);

            for i in 0..amt {
                let elem: &wasm::InitExpr = &elem.init[src + i];
                match elem.code() {
                    &[Instruction::RefFunc(fun_idx), Instruction::End] => {
                        let fun_addr = rt.store.get_module(module_addr).get_fun(FunIdx(fun_idx));
                        fun_addrs.push(Ref::Func(fun_addr));
                    }
                    _ => todo!(),
                }
            }

            let table = rt.store.get_table_mut(table_addr);
            for (addr_idx, addr) in fun_addrs.into_iter().enumerate() {
                let ret = table.set(dst + addr_idx, addr); // bounds already checked
                debug_assert_eq!(ret, Ok(()));
            }

            rt.ip += 1;
        }

        Instruction::ElemDrop(elem_idx) => {
            let elem_addr = rt.get_module(module_addr).get_elem(ElemIdx(elem_idx));
            let elem = rt.store.get_elem_mut(elem_addr);
            elem.init.clear();
            rt.ip += 1;
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Ref instructions
        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::RefNull(heap_ty) => {
            rt.stack.push_ref(Ref::Null(heap_ty))?;
            rt.ip += 1;
        }

        Instruction::RefIsNull => {
            let val = rt.stack.pop_ref()?;
            rt.stack.push_i32(i32::from(val.is_null()))?;
            rt.ip += 1;
        }

        Instruction::RefFunc(fun_idx) => {
            let fun_addr = rt.store.get_module(module_addr).get_fun(FunIdx(fun_idx));
            rt.stack.push_ref(Ref::Func(fun_addr))?;
            rt.ip += 1;
        }

        Instruction::RefAsNonNull => {
            let ref_ = rt.stack.pop_ref()?;
            if ref_.is_null() {
                return Err(ExecError::Trap(Trap::NullReference));
            }
            rt.stack.push_ref(ref_)?;
            rt.ip += 1;
        }

        Instruction::BrOnNull(n_blocks) => {
            let ref_ = rt.stack.pop_ref()?;
            if ref_.is_null() {
                br(rt, n_blocks)?;
            } else {
                rt.stack.push_ref(ref_)?;
                rt.ip += 1;
            }
        }

        Instruction::BrOnNonNull(n_blocks) => {
            let ref_ = rt.stack.pop_ref()?;
            if ref_.is_null() {
                rt.ip += 1;
            } else {
                rt.stack.push_ref(ref_)?;
                br(rt, n_blocks)?;
            }
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Exception handling instructions
        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::TryTable(block_ty, try_table) => {
            // Same as 'block'
            let (n_args, n_rets) = block_arity(rt, module_addr, block_ty);
            let mut args = Vec::with_capacity(n_args as usize);
            for _ in 0..n_args {
                args.push(rt.stack.pop_value()?);
            }

            let end_ip = match current_fun.block_to_end.get(&rt.ip) {
                None => exec_panic!("Couldn't find continuation of block"),
                Some(cont) => cont,
            };

            rt.stack
                .push_try(try_table.clone(), end_ip + 1, n_args, n_rets);

            for arg in args.into_iter().rev() {
                rt.stack.push_value(arg)?;
            }

            rt.ip += 1;
        }

        Instruction::Throw(tag_idx) => {
            // Pop blocks until we find a `try` block with a catch block for the tag or a
            // catch-all.
            let exception_tag_addr = rt.store.get_module(module_addr).get_tag(TagIdx(tag_idx));
            let exception_tag = rt.store.get_tag(exception_tag_addr);
            let exception_n_args = exception_tag.ty.params().len();

            let mut exception_args = Vec::with_capacity(exception_n_args);
            for _ in 0..exception_n_args {
                exception_args.push(rt.stack.pop_value()?);
            }

            let exn_addr = rt.store.allocate_exn(Exception {
                addr: exception_tag_addr,
                args: exception_args,
            });

            throw(rt, exn_addr)?;
        }

        Instruction::ThrowRef => {
            let ref_ = rt.stack.pop_ref()?;
            if ref_.is_null() {
                return Err(ExecError::Trap(Trap::NullReference));
            }

            let exn_addr = match ref_ {
                Ref::Null(_) => {
                    return Err(ExecError::Trap(Trap::NullReference));
                }
                Ref::Func(_) | Ref::Extern(_) | Ref::Struct(_) | Ref::Array(_) | Ref::I31(_) => {
                    return Err(ExecError::Trap(Trap::NullReference)); // FIXME: trap kind
                }
                Ref::Exn(exn_addr) => exn_addr,
            };

            throw(rt, exn_addr)?;
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::Simd(simd_instr) => {
            simd::exec_simd_instr(rt, module_addr, simd_instr)?;
        }

        Instruction::Atomics(_) => {
            exec_panic!("Instruction not implemented: {:?}", instr);
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::StructNew(ty_idx) => {
            let struct_type: &wasm::StructType = rt
                .store
                .get_module(module_addr)
                .get_type(TypeIdx(ty_idx))
                .as_struct_type()
                .unwrap();

            let mut fields: Vec<Value> = Vec::with_capacity(struct_type.fields.len());

            for wasm::FieldType {
                storage_ty,
                mutability: _,
            } in struct_type.fields.iter().rev()
            {
                let value = rt.stack.pop_value().unwrap();
                match (storage_ty, value) {
                    (wasm::StorageType::Val(wasm::ValueType::I32), Value::I32(_))
                    | (wasm::StorageType::Val(wasm::ValueType::I64), Value::I64(_))
                    | (wasm::StorageType::Val(wasm::ValueType::F32), Value::F32(_))
                    | (wasm::StorageType::Val(wasm::ValueType::F64), Value::F64(_))
                    | (wasm::StorageType::Val(wasm::ValueType::V128), Value::I128(_)) => {
                        fields.push(value);
                    }

                    (wasm::StorageType::Val(wasm::ValueType::Reference(_)), Value::Ref(_)) => {
                        // TODO: Check reference type.
                        fields.push(value);
                    }

                    (wasm::StorageType::Packed(wasm::PackedType::I8), Value::I32(value)) => {
                        fields.push(Value::I32((value as i8) as i32));
                    }

                    (wasm::StorageType::Packed(wasm::PackedType::I16), Value::I32(value)) => {
                        fields.push(Value::I32((value as i16) as i32));
                    }

                    _ => todo!(),
                }
            }

            fields.reverse();

            let struct_addr = rt.store.allocate_struct(struct_type.clone(), fields);

            rt.stack.push_ref(Ref::Struct(struct_addr))?;

            rt.ip += 1;
        }

        Instruction::StructNewDefault(ty_idx) => {
            let struct_type = rt
                .store
                .get_module(module_addr)
                .get_type(TypeIdx(ty_idx))
                .as_struct_type()
                .unwrap();

            let fields: Vec<Value> = struct_type
                .fields
                .iter()
                .rev()
                .map(
                    |wasm::FieldType {
                         storage_ty,
                         mutability: _,
                     }| Value::default_from_storage_type(storage_ty),
                )
                .collect();

            let struct_addr = rt.store.allocate_struct(struct_type.clone(), fields);

            rt.stack.push_ref(Ref::Struct(struct_addr))?;

            rt.ip += 1;
        }

        Instruction::StructSet(ty_idx, field_idx) => {
            let mut field_value = rt.stack.pop_value()?;

            // If the field is packed, truncate the value and sign extend to 32 bits.
            let struct_type: &wasm::StructType = rt
                .store
                .get_module(module_addr)
                .get_type(TypeIdx(ty_idx))
                .as_struct_type()
                .unwrap();

            let field_type = &struct_type.fields[field_idx as usize];

            match field_type.storage_ty {
                wasm::StorageType::Val(_) => {}

                wasm::StorageType::Packed(wasm::PackedType::I8) => {
                    let i = field_value.expect_i32();
                    field_value = Value::I32((i as i8) as i32);
                }

                wasm::StorageType::Packed(wasm::PackedType::I16) => {
                    let i = field_value.expect_i32();
                    field_value = Value::I32((i as i16) as i32);
                }
            }

            let struct_ref = rt.stack.pop_ref()?;

            // Struct ref can be null.
            let struct_addr = match struct_ref {
                Ref::Null(_) => return Err(ExecError::Trap(Trap::NullStructReference)),
                Ref::Struct(struct_addr) => struct_addr,
                _ => {
                    // Validation error.
                    exec_panic!(
                        "struct.get argument is not a struct reference: {:?}",
                        struct_ref
                    )
                }
            };

            let struct_ = rt.store.get_struct_mut(struct_addr);
            struct_.fields[field_idx as usize] = field_value;

            rt.ip += 1;
        }

        // `get_s` is the same as `get` as we store packed values sign extended.
        Instruction::StructGet(_ty_idx, field_idx)
        | Instruction::StructGetS(_ty_idx, field_idx) => {
            let value = struct_get(rt, field_idx)?;
            rt.stack.push_value(value)?;
            rt.ip += 1;
        }

        Instruction::StructGetU(ty_idx, field_idx) => {
            let mut value = struct_get(rt, field_idx)?;

            // If the value is packed then it's sign extended to 32 bits, convert it to zero
            // extension.
            let struct_type: &wasm::StructType = rt
                .store
                .get_module(module_addr)
                .get_type(TypeIdx(ty_idx))
                .as_struct_type()
                .unwrap();

            let field_type = &struct_type.fields[field_idx as usize];

            match field_type.storage_ty {
                wasm::StorageType::Val(_) => {}

                wasm::StorageType::Packed(wasm::PackedType::I8) => {
                    let i = value.expect_i32();
                    value = Value::I32(((i as i8) as u8) as i32);
                }

                wasm::StorageType::Packed(wasm::PackedType::I16) => {
                    let i = value.expect_i32();
                    value = Value::I32(((i as i16) as u16) as i32);
                }
            }

            rt.stack.push_value(value)?;

            rt.ip += 1;
        }

        Instruction::RefI31 => {
            let value = rt.stack.pop_i32()?;
            let i31 = (value as u32) & 0x7f_ff_ff_ff;
            rt.stack.push_value(Value::Ref(Ref::I31(i31 as i32)))?;
            rt.ip += 1;
        }

        Instruction::I31GetS => {
            let mut value = match rt.stack.pop_ref()? {
                Ref::Null(_) => return Err(ExecError::Trap(Trap::NullI31Reference)),
                Ref::I31(value) => value,
                _ => panic!(),
            };

            if value & 0x40_00_00_00 != 0 {
                // Sign extend.
                value = ((value as u32) | 0x80_00_00_00) as i32;
            }

            rt.stack.push_value(Value::I32(value))?;
            rt.ip += 1;
        }

        Instruction::I31GetU => {
            let value = match rt.stack.pop_ref()? {
                Ref::Null(_) => return Err(ExecError::Trap(Trap::NullI31Reference)),
                Ref::I31(value) => value as u32,
                _ => panic!(),
            };
            rt.stack.push_value(Value::I32(value as i32))?;
            rt.ip += 1;
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        Instruction::RefEq
        | Instruction::ArrayNew(_)
        | Instruction::ArrayNewDefault(_)
        | Instruction::ArrayNewFixed(_, _)
        | Instruction::ArrayNewData(_, _)
        | Instruction::ArrayNewElem(_, _)
        | Instruction::ArrayGet(_)
        | Instruction::ArrayGetS(_)
        | Instruction::ArrayGetU(_)
        | Instruction::ArraySet(_)
        | Instruction::ArrayLen
        | Instruction::ArrayFill(_)
        | Instruction::ArrayCopy(_, _)
        | Instruction::ArrayInitData(_, _)
        | Instruction::ArrayInitElem(_, _)
        | Instruction::RefTest(_)
        | Instruction::RefTestNull(_)
        | Instruction::RefCast(_)
        | Instruction::RefCastNull(_)
        | Instruction::BrOnCast(_, _, _, _, _)
        | Instruction::BrOnCastFail(_, _, _, _, _)
        | Instruction::ExternInternalize
        | Instruction::ExternExternalize => exec_panic!("Instruction not implemented: {}", instr),
    }

    Ok(())
}

#[allow(unused)]
fn storage_type_size(ty: &wasm::StorageType) -> usize {
    match ty {
        wasm::StorageType::Val(val_ty) => match val_ty {
            wasm::ValueType::I32 | wasm::ValueType::F32 | wasm::ValueType::Reference(_) => 4,

            wasm::ValueType::I64 | wasm::ValueType::F64 => 8,

            wasm::ValueType::V128 => 16,
        },

        wasm::StorageType::Packed(packed_ty) => match packed_ty {
            wasm::PackedType::I8 => 1,
            wasm::PackedType::I16 => 2,
        },
    }
}

fn throw(rt: &mut Runtime, exn_addr: ExnAddr) -> Result<()> {
    let mut current_fun_addr = rt.frames.current().unwrap().fun_addr;

    let mut current_fun = match &rt.store.get_fun(current_fun_addr) {
        Fun::Wasm(fun) => fun,
        Fun::Host { .. } => panic!(),
    };

    'unwind: while let Ok(block) = rt.stack.pop_block() {
        let try_table: wasm::TryTableData = match block.kind {
            BlockKind::Top => todo!("Uncaught exception"),
            BlockKind::Block | BlockKind::Loop => continue,
            BlockKind::Fun => {
                rt.frames.pop();
                current_fun_addr = match rt.frames.current() {
                    Ok(frame) => frame.fun_addr,
                    Err(_) => {
                        rt.unhandled_exception = Some(exn_addr);
                        return Ok(());
                    }
                };
                current_fun = match rt.store.get_fun(current_fun_addr) {
                    Fun::Wasm(fun) => fun,
                    Fun::Host(_) => panic!(),
                };
                continue;
            }
            BlockKind::Try(table) => *table,
        };

        let exn = rt.store.get_exn(exn_addr);

        for (catch_kind, n_blocks) in try_table.table.iter() {
            match catch_kind {
                wasm::CatchKind::Catch(tag_idx) => {
                    let tag_addr = rt
                        .store
                        .get_module(current_fun.module_addr)
                        .get_tag(TagIdx(*tag_idx));

                    if tag_addr == exn.addr {
                        // Pop blocks.
                        for _ in 0..*n_blocks {
                            let Block { kind, .. } = rt.stack.pop_block()?;
                            assert!(kind != BlockKind::Fun);
                        }
                        let Block { cont, .. } = rt.stack.pop_block()?;

                        // Push tag values.
                        for value in exn.args.iter().rev() {
                            rt.stack.push_value(*value)?;
                        }

                        rt.ip = cont;
                        break 'unwind;
                    }
                }

                wasm::CatchKind::CatchRef(tag_idx) => {
                    let tag_addr = rt
                        .store
                        .get_module(current_fun.module_addr)
                        .get_tag(TagIdx(*tag_idx));

                    if tag_addr == exn.addr {
                        // Pop blocks.
                        for _ in 0..*n_blocks {
                            let Block { kind, .. } = rt.stack.pop_block()?;
                            assert!(kind != BlockKind::Fun);
                        }
                        let Block { cont, .. } = rt.stack.pop_block()?;

                        // Push tag values.
                        for value in exn.args.iter().rev() {
                            rt.stack.push_value(*value)?;
                        }

                        // Push exnref.
                        rt.stack.push_value(Value::Ref(Ref::Exn(exn_addr)))?;

                        rt.ip = cont;
                        break 'unwind;
                    }
                }

                wasm::CatchKind::CatchAll => {
                    // Pop blocks.
                    for _ in 0..*n_blocks {
                        let Block { kind, .. } = rt.stack.pop_block()?;
                        assert!(kind != BlockKind::Fun);
                    }
                    let Block { cont, .. } = rt.stack.pop_block()?;

                    rt.ip = cont;
                    break 'unwind;
                }

                wasm::CatchKind::CatchAllRef => {
                    // Pop blocks.
                    for _ in 0..*n_blocks {
                        let Block { kind, .. } = rt.stack.pop_block()?;
                        assert!(kind != BlockKind::Fun);
                    }
                    let Block { cont, .. } = rt.stack.pop_block()?;

                    // Push exnref.
                    rt.stack.push_value(Value::Ref(Ref::Exn(exn_addr)))?;

                    rt.ip = cont;
                    break 'unwind;
                }
            }
        }
    }

    Ok(())
}

fn get_fun_addr_from_table(
    rt: &Runtime,
    module_addr: ModuleAddr,
    elem_idx: i32,
    table_ref: u8,
) -> Result<FunAddr> {
    let table_addr = rt
        .get_module(module_addr)
        .get_table(TableIdx(u32::from(table_ref)));

    match rt.store.get_table(table_addr).get(elem_idx as usize) {
        Some(Ref::Func(fun_addr)) => Ok(*fun_addr),
        Some(Ref::Null(_ref_ty)) => Err(ExecError::Trap(Trap::UninitializedElement)),
        Some(Ref::Extern(_) | Ref::Exn(_) | Ref::Struct(_) | Ref::Array(_) | Ref::I31(_)) => {
            // TODO: Check table type to help with debugging
            // TODO: Incorrect trap kind.
            Err(ExecError::Trap(Trap::CallIndirectOnExternRef))
        }
        None => Err(ExecError::Trap(Trap::UndefinedElement)),
    }
}

fn check_fun_type(rt: &Runtime, sig: u32, module_addr: ModuleAddr, fun_addr: FunAddr) -> bool {
    let call_instr_ty = rt
        .store
        .get_module(module_addr)
        .get_type(TypeIdx(sig))
        .as_function_type()
        .unwrap();
    let fun_module_addr = rt.store.get_fun(fun_addr).module_addr();
    let actual_fun_ty_idx = rt.store.get_fun(fun_addr).ty_idx();
    let actual_ty = &rt
        .store
        .get_module(fun_module_addr)
        .get_type(actual_fun_ty_idx)
        .as_function_type()
        .unwrap();

    use wasm::{FunctionType, HeapType, ReferenceType, ValueType};

    fn ref_eq(
        rt: &Runtime,
        module_addr: ModuleAddr,
        ref1: &ReferenceType,
        ref2: &ReferenceType,
    ) -> bool {
        if ref1.nullable != ref2.nullable {
            return false;
        }

        match (ref1.heap_ty, ref2.heap_ty) {
            (HeapType::Extern, HeapType::Extern) => true,
            (HeapType::Func, HeapType::Func) => true,
            (HeapType::TypeIdx(_), HeapType::Func) | (HeapType::Func, HeapType::TypeIdx(_)) => true,
            (HeapType::TypeIdx(i1), HeapType::TypeIdx(i2)) => {
                let i1_t = rt
                    .store
                    .get_module(module_addr)
                    .get_type(TypeIdx(i1))
                    .as_function_type()
                    .unwrap();
                let i2_t = rt
                    .store
                    .get_module(module_addr)
                    .get_type(TypeIdx(i2))
                    .as_function_type()
                    .unwrap();
                compare_fun_tys(rt, module_addr, i1_t, i2_t)
            }
            (_, _) => false,
        }
    }

    fn type_eq(rt: &Runtime, module_addr: ModuleAddr, ty1: &ValueType, ty2: &ValueType) -> bool {
        match (ty1, ty2) {
            (ValueType::I32, ValueType::I32) => true,
            (ValueType::I64, ValueType::I64) => true,
            (ValueType::F32, ValueType::F32) => true,
            (ValueType::F64, ValueType::F64) => true,
            (ValueType::V128, ValueType::V128) => true,
            (ValueType::Reference(ref1), ValueType::Reference(ref2)) => {
                ref_eq(rt, module_addr, ref1, ref2)
            }
            (_, _) => false,
        }
    }

    fn compare_fun_tys(
        rt: &Runtime,
        module_addr: ModuleAddr,
        ty1: &FunctionType,
        ty2: &FunctionType,
    ) -> bool {
        if ty1.params().len() != ty2.params().len() || ty1.results().len() != ty2.results().len() {
            return false;
        }

        // No subtyping until GC proposal
        ty1.params()
            .iter()
            .zip(ty2.params().iter())
            .chain(ty1.results().iter().zip(ty2.results().iter()))
            .all(|(t1, t2)| type_eq(rt, module_addr, t1, t2))
    }

    compare_fun_tys(rt, module_addr, call_instr_ty, actual_ty)
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

fn i32_trunc_sat_s_f32(f: f32) -> i32 {
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
    if f.is_nan() {
        0
    } else if f < i32::MIN as f32 {
        i32::MIN
    } else if f >= -(i32::MIN as f32) {
        i32::MAX
    } else {
        f as i32
    }
}

fn i32_trunc_sat_u_f32(f: f32) -> i32 {
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
    if f.is_nan() {
        0
    } else if f <= -1.0 {
        0
    } else if f >= -(i32::MIN as f32) * 2.0f32 {
        -1
    } else {
        (f as i64) as i32
    }
}

fn i32_trunc_sat_s_f64(f: f64) -> i32 {
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
    if f.is_nan() {
        0
    } else if f <= -1.0f64 {
        0
    } else if f >= -(i32::MIN as f64) * 2.0f64 {
        -1
    } else {
        (f as i64) as i32
    }
}

fn f32_convert_u_i32(i: i32) -> f32 {
    if i >= 0 {
        i as f32
    } else {
        let i = i as u32;
        ((i >> 1) | (i & 0b1)) as f32 * 2f32
    }
}

fn f32_max(a: f32, b: f32) -> f32 {
    if a == b {
        f32::from_bits(a.to_bits() & b.to_bits())
    } else if a > b {
        a
    } else if a < b {
        b
    } else {
        value::canonical_f32_nan()
    }
}

fn f32_min(a: f32, b: f32) -> f32 {
    if a == b {
        f32::from_bits(a.to_bits() | b.to_bits())
    } else if a < b {
        a
    } else if a > b {
        b
    } else {
        value::canonical_f32_nan()
    }
}

fn f64_max(a: f64, b: f64) -> f64 {
    if a == b {
        f64::from_bits(a.to_bits() & b.to_bits())
    } else if a > b {
        a
    } else if a < b {
        b
    } else {
        value::canonical_f64_nan()
    }
}

fn f64_min(a: f64, b: f64) -> f64 {
    if a == b {
        f64::from_bits(a.to_bits() | b.to_bits())
    } else if a < b {
        a
    } else if a > b {
        b
    } else {
        value::canonical_f64_nan()
    }
}

fn f32_nearest(f: f32) -> f32 {
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
    }
}

fn f64_nearest(f: f64) -> f64 {
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
    }
}

fn block_arity(rt: &Runtime, module_addr: ModuleAddr, ty: wasm::BlockType) -> (u32, u32) {
    match ty {
        wasm::BlockType::Value(_) => (0, 1),
        wasm::BlockType::Empty => (0, 0),
        wasm::BlockType::TypeIndex(ty_idx) => {
            let ty = &rt
                .store
                .get_module(module_addr)
                .get_type(TypeIdx(ty_idx))
                .as_function_type()
                .unwrap();
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
        BlockKind::Block | BlockKind::Loop | BlockKind::Try { .. } => {
            rt.ip = cont;
        }
        BlockKind::Fun => {
            rt.frames.pop();
            rt.ip = cont;
        }
    }

    Ok(())
}

fn struct_get(rt: &mut Runtime, field_idx: u32) -> Result<Value> {
    let struct_ref = rt.stack.pop_ref()?;

    // Struct ref can be null.
    let struct_addr = match struct_ref {
        Ref::Null(_) => return Err(ExecError::Trap(Trap::NullStructReference)),
        Ref::Struct(struct_addr) => struct_addr,
        _ => {
            // Validation error.
            exec_panic!(
                "struct.get argument is not a struct reference: {:?}",
                struct_ref
            )
        }
    };

    let struct_ = rt.store.get_struct(struct_addr);

    Ok(struct_.fields[field_idx as usize])
}
