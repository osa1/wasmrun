//! Implements a Wasm module that provides the WASI API
//!
//! References:
//!
//! - API documentation: https://github.com/WebAssembly/WASI/blob/master/phases/snapshot/docs.md
//!   This is currently not too useful, see https://github.com/WebAssembly/WASI/issues/326
//!
//! - C header file for the API:
//!   https://github.com/WebAssembly/wasi-libc/blob/master/libc-bottom-half/headers/public/wasi/api.h
//!
//!   (Most of the comments below are copied from this header file)
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
use wasi_common::wasi::wasi_snapshot_preview1;

/// Initializes the 'wasi_snapshot_preview1' module.
pub(crate) fn allocate_wasi(store: &mut Store) -> ModuleAddr {
    let module_addr = store.next_module_addr();
    let mut module: Module = Default::default();

    use wasm::ValueType::{I32, I64};

    let mut allocate = |arg_tys, ret_ty, name, fun| {
        allocate_fn(&mut module, module_addr, store, arg_tys, ret_ty, name, fun);
    };

    allocate(vec![I32], I32, "proc_exit", wasi_proc_exit);
    allocate(vec![I32, I32, I32, I32], I32, "fd_write", wasi_fd_write);
    allocate(vec![I32, I32], I32, "fd_prestat_get", wasi_fd_prestat_get);
    allocate(
        vec![I32, I32, I32],
        I32,
        "fd_prestat_dir_name",
        wasi_fd_prestat_dir_name,
    );
    allocate(
        vec![I32, I32],
        I32,
        "environ_sizes_get",
        wasi_environ_sizes_get,
    );
    allocate(vec![I32, I32], I32, "environ_get", wasi_environ_get);
    allocate(vec![I32, I32], I32, "args_sizes_get", wasi_args_sizes_get);
    allocate(vec![I32, I32], I32, "args_get", wasi_args_get);
    allocate(vec![I32], I32, "fd_close", wasi_fd_close);
    allocate(vec![I32, I32], I32, "fd_filestat_get", wasi_fd_filestat_get);
    allocate(vec![I32, I32, I32, I32], I32, "fd_read", wasi_fd_read);
    allocate(
        vec![I32, I32, I32, I32, I32, I64, I64, I32, I32],
        I32,
        "path_open",
        wasi_path_open,
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
fn wasi_proc_exit(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    let exit_code = rt.get_local(0)?.expect_i32();

    trace!("proc_exit({})", exit_code);

    // TODO: No idea what this does
    wasi_snapshot_preview1::proc_exit(&mut rt.wasi_ctx, rt.store.get_mem_mut(mem_addr), exit_code);

    Err(ExecError::Exit(exit_code))
}

// [i32, i32, i32, i32] -> [i32]
fn wasi_fd_write(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    let fd = rt.get_local(0)?.expect_i32();

    // List of scatter/gather vectors from which to retrieve data.
    // __wasi_ciovec_t *iovs
    let iovs_ptr = rt.get_local(1)?.expect_i32();

    // The length of the array pointed to by `iovs`
    let iovs_len = rt.get_local(2)?.expect_i32();

    // The number of bytes written
    let nwritten_ptr = rt.get_local(3)?.expect_i32();

    trace!(
        "fd_write(fd={}, iovs_ptr={:#x}, iovs_len={}, nwritten_ptr={:#x})",
        fd,
        iovs_ptr,
        iovs_len,
        nwritten_ptr
    );

    let ret = wasi_snapshot_preview1::fd_write(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        fd,
        iovs_ptr,
        iovs_len,
        nwritten_ptr,
    );

    Ok(Value::I32(ret))
}

// [i32, i32, i32, i32] -> [i32]
fn wasi_fd_read(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    let fd = rt.get_local(0)?.expect_i32();

    // List of scatter/gather vectors to which to store data
    // const __wasi_iovec_t *iovs,
    let iovs = rt.get_local(1)?.expect_i32();

    // The length of the array pointed to by `iovs`.
    // size_t iovs_len
    let iovs_len = rt.get_local(2)?.expect_i32();

    // The number of bytes read.
    // __wasi_size_t *nread
    let nread = rt.get_local(3)?.expect_i32();

    trace!("fd_read({}, {:#x}, {}, {:#x})", fd, iovs, iovs_len, nread);

    let ret = wasi_snapshot_preview1::fd_read(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        fd,
        iovs,
        iovs_len,
        nread,
    );

    Ok(Value::I32(ret))
}

// [i32, i32] -> [i32]
// Return a description of the given preopened file descriptor.
// https://github.com/bytecodealliance/wasmtime/blob/5799fd3cc0b4d51f58532b19397d5c3a4677a010/crates/wasi-common/src/old/snapshot_0/hostcalls_impl/fs.rs#L958
fn wasi_fd_prestat_get(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // __wasi_fd_t fd
    let fd = rt.get_local(0)?.expect_i32();

    // The buffer where the description is stored
    // __wasi_prestat_t *buf
    let prestat_t_ptr = rt.get_local(1)?.expect_i32();

    trace!("fd_prestat_get({}, {:#x})", fd, prestat_t_ptr);

    let ret = wasi_snapshot_preview1::fd_prestat_get(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        fd,
        prestat_t_ptr,
    );

    Ok(Value::I32(ret)) // ERRNO_BADF
}

// [i32, i32, i32] -> [i32]
// Return a description of the given preopened file descriptor
fn wasi_fd_prestat_dir_name(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // __wasi_fd_t fd
    let fd = rt.get_local(0)?.expect_i32();

    // A buffer into which to write the preopened directory name.
    // uint8_t * path
    let buf_ptr = rt.get_local(1)?.expect_i32();

    // __wasi_size_t path_len
    let len = rt.get_local(2)?.expect_i32();

    trace!("fd_prestat_dir_name({}, {:#x}, {})", fd, buf_ptr, len);

    let ret = wasi_snapshot_preview1::fd_prestat_dir_name(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        fd,
        buf_ptr,
        len,
    );

    Ok(Value::I32(ret))
}

// [i32, i32] -> [i32]
fn wasi_environ_sizes_get(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // The size of the environment variable data.

    // The number of environment variable arguments.
    // __wasi_size_t *environc
    let environc_ptr = rt.get_local(0)?.expect_i32();

    // __wasi_size_t *environ_buf_size
    let environ_buf_size = rt.get_local(1)?.expect_i32();

    trace!(
        "environ_sizes_get({:#x}, {})",
        environc_ptr,
        environ_buf_size
    );

    let ret = wasi_snapshot_preview1::environ_sizes_get(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        environc_ptr,
        environ_buf_size,
    );

    Ok(Value::I32(ret))
}

// [i32, i32] -> [i32]
// Read environment variable data. The sizes of the buffers should match that returned by
// `environ_sizes_get`.
fn wasi_environ_get(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // uint8_t * * environ
    let environ = rt.get_local(0)?.expect_i32();

    // uint8_t * environ_buf
    let environ_buf = rt.get_local(1)?.expect_i32();

    trace!("environ_get({:#x}, {:#x})", environ, environ_buf);

    let ret = wasi_snapshot_preview1::environ_get(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        environ,
        environ_buf,
    );

    Ok(Value::I32(ret))
}

// Return command-line argument data sizes
fn wasi_args_sizes_get(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // Number of arguments
    let argc_addr = rt.get_local(0)?.expect_i32();
    // The size of the argument string data
    let argv_buf_size_addr = rt.get_local(1)?.expect_i32();

    trace!(
        "args_sizes_get({:#x}, {:#x})",
        argc_addr,
        argv_buf_size_addr
    );

    let ret = wasi_snapshot_preview1::args_sizes_get(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        argc_addr,
        argv_buf_size_addr,
    );

    Ok(Value::I32(ret))
}

// Read command-line argument data. The size of the array should match that returned by
// `args_sizes_get`.
fn wasi_args_get(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // uint8_t **
    let argv = rt.get_local(0)?.expect_i32();
    // uint8_t *
    let argv_buf = rt.get_local(1)?.expect_i32();

    trace!("args_get({:#x}, {:#x})", argv, argv_buf);

    let ret = wasi_snapshot_preview1::args_get(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        argv,
        argv_buf,
    );

    Ok(Value::I32(ret))
}

// [i32] -> [i32]
fn wasi_fd_close(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    let fd = rt.get_local(0)?.expect_i32();

    trace!("fd_close({})", fd);

    let ret =
        wasi_snapshot_preview1::fd_close(&mut rt.wasi_ctx, rt.store.get_mem_mut(mem_addr), fd);

    Ok(Value::I32(ret))
}

// [i32, i32] -> [i32]
// Return the attributes of an open file.
fn wasi_fd_filestat_get(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // __wasi_fd_t fd
    let fd = rt.get_local(0)?.expect_i32();

    // The buffer where the file's attributes are stored.
    // __wasi_filestat_t *buf
    let buf = rt.get_local(1)?.expect_i32();

    trace!("fd_filestat_get({}, {:#x})", fd, buf);

    let ret = wasi_snapshot_preview1::fd_filestat_get(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        fd,
        buf,
    );

    Ok(Value::I32(ret))
}

// [I32, I32, I32, I32, I32, I64, I64, I32, I32] -> [I32]
// Open a file or directory. The returned file descriptor is not guaranteed to be the
// lowest-numbered file descriptor not currently open; it is randomized to prevent applications
// from depending on making assumptions about indexes, since this is error-prone in multi-threaded
// contexts. The returned file descriptor is guaranteed to be less than 2**31. Note: This is
// similar to `openat` in POSIX.
fn wasi_path_open(rt: &mut Runtime, mem_addr: MemAddr) -> Result<Value> {
    // __wasi_fd_t fd
    let fd = rt.get_local(0)?.expect_i32();

    // Flags determining the method of how the path is resolved.
    // __wasi_lookupflags_t dirflags,
    let flags = rt.get_local(1)?.expect_i32();

    // The relative path of the file or directory to open, relative to the `path_open::fd`
    // directory.
    // const char *path
    let path = rt.get_local(2)?.expect_i32();

    // The length of the buffer pointed to by `path`.
    // size_t path_len
    let path_len = rt.get_local(3)?.expect_i32();

    // The method by which to open the file.
    // __wasi_oflags_t oflags
    let oflags = rt.get_local(4)?.expect_i32();

    // The initial rights of the newly created file descriptor. The
    // implementation is allowed to return a file descriptor with fewer rights
    // than specified, if and only if those rights do not apply to the type of
    // file being opened.
    // The *base* rights are rights that will apply to operations using the file
    // descriptor itself, while the *inheriting* rights are rights that apply to
    // file descriptors derived from it.
    // __wasi_rights_t fs_rights_base
    let fs_rights_base = rt.get_local(5)?.expect_i64();

    // __wasi_rights_t fs_rights_inherting
    let fs_rights_inheriting = rt.get_local(6)?.expect_i64();

    // __wasi_fdflags_t fdflags
    let fdflags = rt.get_local(7)?.expect_i32();

    // The file descriptor of the file that has been opened.
    // __wasi_fd_t *opened_fd
    let opened_fd = rt.get_local(8)?.expect_i32();

    trace!(
        "path_open(\
        fd={}, flags={}, path={:#x}, path_len={}, oflags={}, \
        fs_rights_base={}, fs_rights_inheriting={}, fdflags={}, \
        opened_fd={:#x})",
        fd,
        flags,
        path,
        path_len,
        oflags,
        fs_rights_base,
        fs_rights_inheriting,
        fdflags,
        opened_fd
    );

    let ret = wasi_snapshot_preview1::path_open(
        &mut rt.wasi_ctx,
        rt.store.get_mem_mut(mem_addr),
        fd,
        flags,
        path,
        path_len,
        oflags,
        fs_rights_base,
        fs_rights_inheriting,
        fdflags,
        opened_fd,
    );

    Ok(Value::I32(ret))
}
