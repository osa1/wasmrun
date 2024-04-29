use libwasmrun::{exec, syntax, HostFunDecl, Mem, Runtime, ValueType};

use std::rc::Rc;

fn main() {
    let module_path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| panic!("USAGE: w4wr <program>"));

    let module = syntax::deserialize_file(module_path).unwrap();

    let mut rt = Runtime::new();

    let mem_addr = rt.allocate_mem(Mem::new(1, Some(1)));

    let _env_module_addr = rt.allocate_host_module(
        "env".to_string(),
        vec![
            (
                "tone".to_string(),
                HostFunDecl {
                    arg_tys: vec![
                        ValueType::I32, // frequency
                        ValueType::I32, // duration
                        ValueType::I32, // volume
                        ValueType::I32, // flags
                    ],
                    ret_tys: vec![],
                    fun: Rc::new(|_rt, _mem_addr| {
                        println!("tone");
                        Ok(vec![])
                    }),
                },
            ),
            (
                "blit".to_string(),
                HostFunDecl {
                    arg_tys: vec![
                        ValueType::I32, // spritePtr
                        ValueType::I32, // x
                        ValueType::I32, // y
                        ValueType::I32, // width
                        ValueType::I32, // height
                        ValueType::I32, // flags
                    ],
                    ret_tys: vec![],
                    fun: Rc::new(|_rt, _mem_addr| {
                        println!("blit");
                        Ok(vec![])
                    }),
                },
            ),
            (
                "textUtf16".to_string(),
                HostFunDecl {
                    arg_tys: vec![
                        ValueType::I32, // str
                        ValueType::I32, // byteLength
                        ValueType::I32, // x
                        ValueType::I32, // y
                    ],
                    ret_tys: vec![],
                    fun: Rc::new(|_rt, _mem_addr| {
                        println!("textUtf16");
                        Ok(vec![])
                    }),
                },
            ),
            (
                "rect".to_string(),
                HostFunDecl {
                    arg_tys: vec![
                        ValueType::I32, // x
                        ValueType::I32, // y
                        ValueType::I32, // width
                        ValueType::I32, // height
                    ],
                    ret_tys: vec![],
                    fun: Rc::new(|_rt, _mem_addr| {
                        println!("rect");
                        Ok(vec![])
                    }),
                },
            ),
        ],
        vec![("memory".to_string(), mem_addr)],
    );

    let module_addr = exec::instantiate(&mut rt, module).unwrap();

    let start_fun_idx = rt.get_module(module_addr).get_start().unwrap();
    let update_fun_idx = rt
        .get_module(module_addr)
        .get_exported_fun_idx("update")
        .unwrap();

    exec::invoke(&mut rt, module_addr, start_fun_idx).unwrap();
    exec::finish(&mut rt).unwrap();

    loop {
        exec::invoke(&mut rt, module_addr, update_fun_idx).unwrap();
        exec::finish(&mut rt).unwrap();
    }
}
