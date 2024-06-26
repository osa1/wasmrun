use libwasmrun::{exec, HostFunDecl, MemAddr, Result, Runtime, Value, ValueType};
use libwasmrun_syntax as wasm;

use std::rc::Rc;

fn host_add(rt: &mut Runtime, _: Option<MemAddr>) -> Result<Vec<Value>> {
    let arg1 = match rt.get_local(0)? {
        Value::I32(a) => a,
        _ => panic!(),
    };
    let arg2 = match rt.get_local(1)? {
        Value::I32(a) => a,
        _ => panic!(),
    };
    Ok(vec![Value::I32(arg1 + arg2)])
}

static TEST_WAST: &str = r#"
    (module
      (func $i (import "host" "add") (param i32 i32) (result i32))
      (func (export "test") (param $a i32) (param $b i32) (result i32)
         (call $i (local.get $a) (local.get $b)))
      (memory 1)
      (export "mem" (memory 0)))
"#;

#[test]
fn test_importing_host_fn() {
    let wasm = wabt::wat2wasm(TEST_WAST).unwrap();
    let module: wasm::Module = wasm::deserialize_buffer(&wasm).unwrap();

    let mut rt = Runtime::new();

    rt.allocate_host_module(
        "host".to_owned(),
        vec![(
            "add".to_owned(),
            HostFunDecl {
                arg_tys: vec![ValueType::I32, ValueType::I32],
                ret_tys: vec![ValueType::I32],
                fun: Rc::new(host_add),
            },
        )],
        vec![],
    );

    let module_addr = exec::instantiate(&mut rt, module).unwrap();

    rt.push_value(Value::I32(1));
    rt.push_value(Value::I32(2));
    exec::invoke_by_name(&mut rt, module_addr, "test").unwrap();
    exec::finish(&mut rt).unwrap();

    assert!(matches!(rt.pop_value().unwrap(), Value::I32(3)));
}
