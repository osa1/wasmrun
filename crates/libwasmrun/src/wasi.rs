//! Implements a module that provides the WASI API. References
//!
//! - API documentation: https://github.com/WebAssembly/WASI/blob/master/phases/snapshot/docs.md
//!   This is currently not too useful, see https://github.com/WebAssembly/WASI/issues/326
//!
//! - C header file for the API:
//!   https://github.com/WebAssembly/wasi-libc/blob/master/libc-bottom-half/headers/public/wasi/api.h
//!
//! - See also https://github.com/WebAssembly/WASI/blob/master/design/application-abi.md for how
//!   WASI functions access callers' memories. Quoting here:
//!
//!   > ... all modules accessing WASI APIs also export a linear memory with the name memory. Data
//!   > pointers in WASI API calls are relative to this memory's index space.
//!
//!   This part is currently implementing by having a special kind of `Fun` for WASI functions. At
//!   call sites we pass the memory address of the calling function to WASI functions implemented
//!   below.

use crate::exec::Runtime;
use crate::export::Export;
use crate::module::Module;
use crate::store::{MemAddr, ModuleAddr, Store};
use crate::value::Value;
use crate::{ExecError, Result};

use parity_wasm::elements as wasm;

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
        let proc_exit_addr = store.allocate_wasi_fun(module_addr, ty_i32x1, &wasi_proc_exit);
        let proc_exit_idx = module.add_fun(proc_exit_addr);
        module.add_export(Export::new_fun("proc_exit".to_owned(), proc_exit_idx));
    }

    {
        let fd_write_addr = store.allocate_wasi_fun(module_addr, ty_i32x4_i32x1, &wasi_fd_write);
        let fd_write_idx = module.add_fun(fd_write_addr);
        module.add_export(Export::new_fun("fd_write".to_owned(), fd_write_idx));
    }

    {
        let fd_prestat_get_addr =
            store.allocate_wasi_fun(module_addr, ty_i32x2_i32x1, &wasi_fd_prestat_get);
        let fd_prestat_get_idx = module.add_fun(fd_prestat_get_addr);
        module.add_export(Export::new_fun(
            "fd_prestat_get".to_owned(),
            fd_prestat_get_idx,
        ));
    }

    {
        let fd_prestat_dir_name_addr =
            store.allocate_wasi_fun(module_addr, ty_i32x3_i32x1, &wasi_fd_prestat_dir_name);
        let fd_prestat_dir_name_idx = module.add_fun(fd_prestat_dir_name_addr);
        module.add_export(Export::new_fun(
            "fd_prestat_dir_name".to_owned(),
            fd_prestat_dir_name_idx,
        ));
    }

    {
        let environ_sizes_get_addr =
            store.allocate_wasi_fun(module_addr, ty_i32x2_i32x1, &wasi_environ_sizes_get);
        let environ_sizes_get_idx = module.add_fun(environ_sizes_get_addr);
        module.add_export(Export::new_fun(
            "environ_sizes_get".to_owned(),
            environ_sizes_get_idx,
        ));
    }

    {
        let environ_get_addr =
            store.allocate_wasi_fun(module_addr, ty_i32x2_i32x1, &wasi_environ_get);
        let environ_get_idx = module.add_fun(environ_get_addr);
        module.add_export(Export::new_fun("environ_get".to_owned(), environ_get_idx));
    }

    let module_addr_ = store.allocate_module(module);
    assert_eq!(module_addr, module_addr_);
    module_addr
}

// [i32] -> []
fn wasi_proc_exit(rt: &mut Runtime, _mem_addr: MemAddr) -> Result<Value> {
    let exit_code = rt.frames.current()?.get_local(0)?.expect_i32();
    ::std::process::exit(exit_code);
}

// [i32, i32, i32, i32] -> [i32]
fn wasi_fd_write(_rt: &mut Runtime, _mem_addr: MemAddr) -> Result<Value> {
    Err(ExecError::Panic("wasi_fd_write".to_string()))
}

// [i32, i32] -> [i32]
// Return a description of the given preopened file descriptor.
// https://github.com/bytecodealliance/wasmtime/blob/5799fd3cc0b4d51f58532b19397d5c3a4677a010/crates/wasi-common/src/old/snapshot_0/hostcalls_impl/fs.rs#L958
fn wasi_fd_prestat_get(rt: &mut Runtime, _mem_addr: MemAddr) -> Result<Value> {
    // __wasi_fd_t fd
    let fd = rt.frames.current()?.get_local(0)?.expect_i32();

    // The buffer where the description is stored
    // __wasi_prestat_t *buf
    let prestat_t_ptr = rt.frames.current()?.get_local(1)?.expect_i32() as u32;

    // Err(ExecError::Panic(format!("wasi_fd_prestat_get({}, {})", fd, prestat_t_ptr)))
    println!("wasi_fd_prestat_get({}, {})", fd, prestat_t_ptr);
    Ok(Value::I32(54)) // ERRNO_NOTDIR

    // // Currently only stdout, stdin, and stderr are allowed: stdin=0, stdout=1, stderr=2
    // match fd {
    //     0 => {
    //         // stdin
    //         Ok(Value::I32(0))
    //     }
    //     1 => {
    //         // stdout
    //         Ok(Value::I32(0))
    //     }
    //     2 => {
    //         // stderr
    //         Ok(Value::I32(0))
    //     }
    //     _ => {
    //         // TODO
    //         Err(ExecError::Panic(format!("wasi_fd_prestat_get({})", fd)))
    //     }
    // }
}

// [i32, i32, i32] -> [i32]
fn wasi_fd_prestat_dir_name(_rt: &mut Runtime, _mem_addr: MemAddr) -> Result<Value> {
    Err(ExecError::Panic("wasi_fd_prestat_dir_name".to_string()))
}

// [i32, i32] -> [i32]
// TODO: This function currently returns 0
fn wasi_environ_sizes_get(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // The size of the environment variable data.

    // The number of environment variable arguments.
    // __wasi_size_t *environc
    let environc_ptr = rt.frames.current()?.get_local(0)?.expect_i32() as u32;

    // __wasi_size_t *environ_buf_size
    let environ_buf_size = rt.frames.current()?.get_local(1)?.expect_i32() as u32;

    let mem = rt.store.get_mem_mut(mem_addr);
    mem.store_32(environc_ptr, 0)?;
    mem.store_32(environ_buf_size, 0)?;

    Ok(Value::I32(0)) // errno success

    // Err(ExecError::Panic(format!(
    //     "wasi_environ_sizes_get (environ_buf_size={:#x}, environc_ptr={:#x})",
    //     environ_buf_size, environc_ptr
    // )))
}

// [i32, i32] -> [i32]
fn wasi_environ_get(_rt: &mut Runtime, _mem_addr: MemAddr) -> Result<Value> {
    Err(ExecError::Panic("wasi_environ_get".to_string()))
}
