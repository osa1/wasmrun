use crate::exec::Runtime;
use crate::export::Export;
use crate::module::Module;
use crate::store::{ModuleAddr, Store};
use crate::{ExecError, Result};

use parity_wasm::elements as wasm;

use std::rc::Rc;

/// Initializes the 'wasi_snapshot_preview1' module.
pub fn allocate_wasi(store: &mut Store) -> ModuleAddr {
    let module_addr = store.next_module_addr();
    let mut module: Module = Default::default();

    let ty_i32x1 = module.add_type(wasm::FunctionType::new(vec![wasm::ValueType::I32], vec![]));
    let ty_i32x2_i32x1 = module.add_type(wasm::FunctionType::new(
        vec![wasm::ValueType::I32; 2],
        vec![wasm::ValueType::I32],
    ));
    let ty_i32x3_i32x1 = module.add_type(wasm::FunctionType::new(
        vec![wasm::ValueType::I32; 3],
        vec![wasm::ValueType::I32],
    ));
    let ty_i32x4_i32x1 = module.add_type(wasm::FunctionType::new(
        vec![wasm::ValueType::I32; 4],
        vec![wasm::ValueType::I32],
    ));

    {
        let proc_exit_addr =
            store.allocate_host_fun(module_addr, ty_i32x1, Rc::new(wasi_proc_exit));
        let proc_exit_idx = module.add_fun(proc_exit_addr);
        module.add_export(Export::new_fun("proc_exit".to_owned(), proc_exit_idx));
    }

    {
        let fd_write_addr =
            store.allocate_host_fun(module_addr, ty_i32x4_i32x1, Rc::new(wasi_fd_write));
        let fd_write_idx = module.add_fun(fd_write_addr);
        module.add_export(Export::new_fun("fd_write".to_owned(), fd_write_idx));
    }

    {
        let fd_prestat_get_addr =
            store.allocate_host_fun(module_addr, ty_i32x2_i32x1, Rc::new(wasi_fd_prestat_get));
        let fd_prestat_get_idx = module.add_fun(fd_prestat_get_addr);
        module.add_export(Export::new_fun(
            "fd_prestat_get".to_owned(),
            fd_prestat_get_idx,
        ));
    }

    {
        let fd_prestat_dir_name_addr = store.allocate_host_fun(
            module_addr,
            ty_i32x3_i32x1,
            Rc::new(wasi_fd_prestat_dir_name),
        );
        let fd_prestat_dir_name_idx = module.add_fun(fd_prestat_dir_name_addr);
        module.add_export(Export::new_fun(
            "fd_prestat_dir_name".to_owned(),
            fd_prestat_dir_name_idx,
        ));
    }

    {
        let environ_sizes_get_addr =
            store.allocate_host_fun(module_addr, ty_i32x2_i32x1, Rc::new(wasi_environ_sizes_get));
        let environ_sizes_get_idx = module.add_fun(environ_sizes_get_addr);
        module.add_export(Export::new_fun(
            "environ_sizes_get".to_owned(),
            environ_sizes_get_idx,
        ));
    }

    {
        let environ_get_addr =
            store.allocate_host_fun(module_addr, ty_i32x2_i32x1, Rc::new(wasi_environ_get));
        let environ_get_idx = module.add_fun(environ_get_addr);
        module.add_export(Export::new_fun("environ_get".to_owned(), environ_get_idx));
    }

    let module_addr_ = store.allocate_module(module);
    assert_eq!(module_addr, module_addr_);
    module_addr
}

// [i32] -> []
fn wasi_proc_exit(rt: &mut Runtime) -> Result<()> {
    let exit_code = rt.stack.pop_i32()?;
    ::std::process::exit(exit_code);
}

// [i32, i32, i32, i32] -> [i32]
fn wasi_fd_write(_rt: &mut Runtime) -> Result<()> {
    Err(ExecError::Panic("wasi_fd_write".to_string()))
}

// [i32, i32] -> [i32]
fn wasi_fd_prestat_get(_rt: &mut Runtime) -> Result<()> {
    Err(ExecError::Panic("wasi_fd_prestat_get".to_string()))
}

// [i32, i32, i32] -> [i32]
fn wasi_fd_prestat_dir_name(_rt: &mut Runtime) -> Result<()> {
    Err(ExecError::Panic("wasi_fd_prestat_dir_name".to_string()))
}

// [i32, i32] -> [i32]
fn wasi_environ_sizes_get(_rt: &mut Runtime) -> Result<()> {
    Err(ExecError::Panic("wasi_environ_sizes_get".to_string()))
}

// [i32, i32] -> [i32]
fn wasi_environ_get(_rt: &mut Runtime) -> Result<()> {
    Err(ExecError::Panic("wasi_environ_get".to_string()))
}
