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

use std::ffi::CString;
use std::io::Write;

use parity_wasm::elements as wasm;

#[derive(Debug)]
pub(crate) struct WASICtx {
    pub(crate) args: Vec<CString>,
}

impl WASICtx {
    pub(crate) fn new() -> Self {
        WASICtx { args: vec![] }
    }

    pub(crate) fn new_with_args(args: Vec<CString>) -> Self {
        WASICtx { args }
    }
}

/// Initializes the 'wasi_snapshot_preview1' module.
pub fn allocate_wasi(store: &mut Store) -> ModuleAddr {
    let module_addr = store.next_module_addr();
    let mut module: Module = Default::default();

    use wasm::ValueType::I32;

    allocate_fn(
        &mut module,
        module_addr,
        store,
        vec![I32],
        I32,
        "proc_exit",
        wasi_proc_exit,
    );
    allocate_fn(
        &mut module,
        module_addr,
        store,
        vec![I32, I32, I32, I32],
        I32,
        "fd_write",
        wasi_fd_write,
    );
    allocate_fn(
        &mut module,
        module_addr,
        store,
        vec![I32, I32],
        I32,
        "fd_prestat_get",
        wasi_fd_prestat_get,
    );
    allocate_fn(
        &mut module,
        module_addr,
        store,
        vec![I32, I32, I32],
        I32,
        "fd_prestat_dir_name",
        wasi_fd_prestat_dir_name,
    );
    allocate_fn(
        &mut module,
        module_addr,
        store,
        vec![I32, I32],
        I32,
        "environ_sizes_get",
        wasi_environ_sizes_get,
    );
    allocate_fn(
        &mut module,
        module_addr,
        store,
        vec![I32, I32],
        I32,
        "environ_get",
        wasi_environ_get,
    );
    allocate_fn(
        &mut module,
        module_addr,
        store,
        vec![I32, I32],
        I32,
        "args_sizes_get",
        wasi_args_sizes_get,
    );
    allocate_fn(
        &mut module,
        module_addr,
        store,
        vec![I32, I32],
        I32,
        "args_get",
        wasi_args_get,
    );

    let module_addr_ = store.allocate_module(module);
    assert_eq!(module_addr, module_addr_);
    module_addr
}

fn allocate_fn(
    module: &mut Module,
    module_addr: ModuleAddr,
    store: &mut Store,
    arg_tys: Vec<wasm::ValueType>,
    ret_ty: wasm::ValueType,
    name: &str,
    fun: fn(&mut Runtime, MemAddr) -> Result<Value>,
) {
    // TODO: This allocates same types multiple times
    let ty_idx = module.add_type(wasm::FunctionType::new(arg_tys, vec![ret_ty]));
    let addr = store.allocate_wasi_fun(module_addr, ty_idx, fun);
    let idx = module.add_fun(addr);
    module.add_export(Export::new_fun(name.to_owned(), idx));
}

// [i32] -> []
fn wasi_proc_exit(rt: &mut Runtime, _mem_addr: MemAddr) -> Result<Value> {
    let exit_code = rt.frames.current()?.get_local(0)?.expect_i32();
    ::std::process::exit(exit_code);
}

// [i32, i32, i32, i32] -> [i32]
fn wasi_fd_write(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    let fd = rt.frames.current()?.get_local(0)?.expect_i32();

    // List of scatter/gather vectors from which to retrieve data.
    // __wasi_ciovec_t *iovs
    let iovs_ptr = rt.frames.current()?.get_local(1)?.expect_i32() as u32;

    // The length of the array pointed to by `iovs`
    let iovs_len = rt.frames.current()?.get_local(2)?.expect_i32() as u32;

    // The number of bytes written
    let nwritten_ptr = rt.frames.current()?.get_local(3)?.expect_i32() as u32;

    // println!(
    //     "wasi_fd_write(fd={}, iovs_ptr={:#x}, iovs_len={}, nwritten_ptr={:#x})",
    //     fd, iovs_ptr, iovs_len, nwritten_ptr
    // );

    let mut bytes: Vec<u8> = vec![];

    for vec_i in 0..iovs_len {
        // Each element is two words
        let offset = vec_i * 8;
        let vec_buf_addr = iovs_ptr + offset;
        let vec_len_addr = vec_buf_addr + 4;
        let vec_buf = rt.store.get_mem(mem_addr).load_32(vec_buf_addr)?;
        let vec_len = rt.store.get_mem(mem_addr).load_32(vec_len_addr)?;
        bytes.reserve(vec_len as usize);
        for byte_i in 0..vec_len {
            bytes.push(rt.store.get_mem(mem_addr)[vec_buf + byte_i]);
        }
    }

    match fd {
        1 => {
            ::std::io::stdout().write(&bytes).unwrap();
        }
        2 => {
            ::std::io::stderr().write(&bytes).unwrap();
        }
        _ => {
            return Err(ExecError::Panic(format!("wasi_fd_write fd={}", fd)));
        }
    }

    rt.store
        .get_mem_mut(mem_addr)
        .store_32(nwritten_ptr, bytes.len() as u32)?;

    Ok(Value::I32(0))
}

// [i32, i32] -> [i32]
// Return a description of the given preopened file descriptor.
// https://github.com/bytecodealliance/wasmtime/blob/5799fd3cc0b4d51f58532b19397d5c3a4677a010/crates/wasi-common/src/old/snapshot_0/hostcalls_impl/fs.rs#L958
fn wasi_fd_prestat_get(_rt: &mut Runtime, _mem_addr: MemAddr) -> Result<Value> {
    // __wasi_fd_t fd
    // let fd = rt.frames.current()?.get_local(0)?.expect_i32();

    // The buffer where the description is stored
    // __wasi_prestat_t *buf
    // let prestat_t_ptr = rt.frames.current()?.get_local(1)?.expect_i32() as u32;

    // Err(ExecError::Panic(format!("wasi_fd_prestat_get({}, {})", fd, prestat_t_ptr)))
    // println!("wasi_fd_prestat_get({}, {})", fd, prestat_t_ptr);
    Ok(Value::I32(8)) // ERRNO_BADF

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

// Return command-line argument data sizes
fn wasi_args_sizes_get(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // Number of arguments
    let argc_addr = rt.frames.current()?.get_local(0)?.expect_i32() as u32;
    // The size of the argument string data
    let argv_buf_size_addr = rt.frames.current()?.get_local(1)?.expect_i32() as u32;

    let argc = rt.wasi_ctx.args.len();
    let argv_size: usize = rt
        .wasi_ctx
        .args
        .iter()
        .map(|arg| arg.as_bytes_with_nul().len())
        .sum();

    let mem = rt.store.get_mem_mut(mem_addr);
    mem.store_32(argc_addr, argc as u32)?;
    mem.store_32(argv_buf_size_addr, argv_size as u32)?;

    Ok(Value::I32(0))
}

// Read command-line argument data. The size of the array should match that returned by
// `args_sizes_get`.
fn wasi_args_get(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // uint8_t **
    let argv_addr = rt.frames.current()?.get_local(0)?.expect_i32() as u32;
    // uint8_t *
    let argv_buf_addr = rt.frames.current()?.get_local(1)?.expect_i32() as u32;

    // Current offset in argv_buf array
    let mut argv_buf_offset = 0;
    // Addresses of arguments, to be written to argv_addr
    let mut arg_ptrs = vec![];

    let mem = rt.store.get_mem_mut(mem_addr);

    for arg in &rt.wasi_ctx.args {
        let arg_bytes = arg.as_bytes_with_nul();
        let arg_ptr = argv_buf_addr + argv_buf_offset;

        // Push address of the argument to argv_addr vector
        arg_ptrs.push(arg_ptr);

        // Copy argument
        for (offset, byte) in arg_bytes.iter().copied().enumerate() {
            mem.store_8(arg_ptr + offset as u32, byte)?;
        }

        // Update offset in argv_buf array
        argv_buf_offset = argv_buf_offset + arg_bytes.len() as u32;
    }

    // Write argv
    for (i, arg_ptr) in arg_ptrs.into_iter().enumerate() {
        mem.store_32(argv_addr + (i * 4) as u32, arg_ptr)?;
    }

    // Err(ExecError::Panic(format!(
    //     "args_get({:#x}, {:#x})",
    //     argv_addr, argv_buf_addr
    // )))

    Ok(Value::I32(0))
}
