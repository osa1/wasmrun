fn main() {
    unsafe {
        main_();
    }
}

unsafe fn main_() {
    let (n_vars, buf_size) = wasi::environ_sizes_get().unwrap();
    println!("n_vars={}, buf_size={}", n_vars, buf_size);
}
