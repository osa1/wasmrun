mod frame;
mod stack;
mod value;

use frame::FrameStack;
use stack::Stack;
use value::Value;

use crate::parser;
use crate::parser::{Export, Fun, FuncType, Instruction, MemArg};

type Addr = u32;

type ModuleIdx = usize;

struct Module {
    types: Vec<FuncType>,
    func_addrs: Vec<Addr>,
    table_addrs: Vec<Addr>,
    // No need for mem_addrs as there can be at most one memory instance currently in Wasm
    global_addrs: Vec<Addr>,
    exports: Vec<Export>,
}

struct Store {
    funcs: Vec<Func>,
    tables: Vec<Vec<Option<u32>>>, // indexed by table address (table_addrs), returns function address (index into Store.funcs)
    mems: Vec<Vec<u8>>,            // indexed by module idx
    globals: Vec<Global>,
}

struct Global {
    value: Value,
    mutable: bool, // Only needed for validation
}

pub struct Runtime {
    store: Store,
    stack: Stack,
    frames: FrameStack,
    modules: Vec<Module>,
}

struct Func {
    module: ModuleIdx,
    fun: Fun,
}

/*
impl Module {
    fn new(module: parser::Module) -> Module {

    }
}
*/

pub fn exec(runtime: &mut Runtime, instr: &[Instruction], mut ip: usize) {
    loop {
        use Instruction::*;
        match &instr[ip] {
            I32Store(MemArg { align: _, offset }) => {
                let value = runtime.stack.pop_i32();
                let addr = runtime.stack.pop_i32() as u32;
                let addr = (addr + offset) as usize;
                let end_addr = addr + 4;

                let current_module = runtime.frames.current().module();
                let mem = &mut runtime.store.mems[current_module];
                if end_addr as usize > mem.len() {
                    panic!("OOB I32Store (mem size={}, addr={})", mem.len(), addr);
                }

                let [b1, b2, b3, b4] = value.to_le_bytes();
                mem[addr] = b1;
                mem[addr + 1] = b2;
                mem[addr + 2] = b3;
                mem[addr + 4] = b4;

                ip += 1;
            }

            I32Load(MemArg { align: _, offset }) => {
                let addr = runtime.stack.pop_i32() as u32;
                let addr = (addr + offset) as usize;
                let end_addr = addr + 4;

                let current_module = runtime.frames.current().module();
                let mem = &runtime.store.mems[current_module];
                if end_addr as usize > mem.len() {
                    panic!("OOB I32Load (mem size={}, addr={})", mem.len(), addr);
                }

                let b1 = mem[addr];
                let b2 = mem[addr + 1];
                let b3 = mem[addr + 2];
                let b4 = mem[addr + 3];
                runtime.stack.push_i32(i32::from_le_bytes([b1, b2, b3, b4]));

                ip += 1;
            }

            LocalGet(idx) => {
                let val = runtime.frames.current().get_local(*idx);
                runtime.stack.push(val);
                ip += 1;
            }

            LocalSet(idx) => {
                let val = runtime.stack.pop();
                runtime.frames.current_mut().set_local(*idx, val);
                ip += 1;
            }

            LocalTee(idx) => {
                let val = runtime.stack.pop();
                runtime.frames.current_mut().set_local(*idx, val);
                runtime.stack.push(val);
                ip += 1;
            }

            GlobalGet(idx) => {
                let current_module = runtime.frames.current().module();
                let global_idx = runtime.modules[current_module].global_addrs[*idx as usize];
                let value = runtime.store.globals[global_idx as usize].value;
                runtime.stack.push(value);
                ip += 1;
            }

            GlobalSet(idx) => {
                let current_module = runtime.frames.current().module();
                let global_idx = runtime.modules[current_module].global_addrs[*idx as usize];
                let value = runtime.stack.pop();
                runtime.store.globals[global_idx as usize].value = value;
                ip += 1;
            }

            I32Const(i) => {
                runtime.stack.push_i32(*i);
                ip += 1;
            }

            I64Const(i) => {
                runtime.stack.push_i64(*i);
                ip += 1;
            }

            F32Const(f) => {
                runtime.stack.push_f32(*f);
                ip += 1;
            }

            F64Const(f) => {
                runtime.stack.push_f64(*f);
                ip += 1;
            }

            I32Le_u => {
                let val2 = runtime.stack.pop_i32();
                let val1 = runtime.stack.pop_i32();
                runtime.stack.push_bool(val1 <= val2);
                ip += 1;
            }

            Call(x) => {
                let module_idx = runtime.frames.current().module();
                let module = &runtime.modules[module_idx];
                let func_addr = module.func_addrs[*x as usize];
                let fun = &runtime.store.funcs[func_addr as usize];
                runtime.frames.push(fun);
                let instrs = fun.fun.expr.instrs.clone();
                exec(runtime, &*instrs, 0);
                runtime.frames.pop();
                ip += 1;
            }

            CallIndirect(type_idx) => {
                let module_idx = runtime.frames.current().module();
                let table_idx = runtime.modules[module_idx].table_addrs[0];
                let table = &runtime.store.tables[table_idx as usize];
                let fun_idx = runtime.stack.pop_i32();
                match table.get(fun_idx as usize) {
                    None => {
                        panic!("call_indirect: OOB function index (function idx={}, table idx={}, table size={})",
                               fun_idx, table_idx, table.len());
                    }
                    Some(None) => {
                        panic!("call_indirect: function index not initialized (function idx={}, table idx={})",
                               fun_idx, table_idx);
                    }
                    Some(Some(fun_addr)) => {
                        let fun = &runtime.store.funcs[*fun_addr as usize];

                        let fun_ty = fun.fun.ty;
                        if fun_ty != *type_idx {
                            panic!("call_indirect: function type doesn't match expected type (fun ty={}, expected={})",
                                   fun_ty, type_idx);
                        }

                        runtime.frames.push(fun);
                        let instrs = fun.fun.expr.instrs.clone();
                        exec(runtime, &*instrs, 0);
                        runtime.frames.pop();
                        ip += 1;
                    }
                }
            }

            Return => {
                return;
            }

            Block(parser::types::Block { ty: _, instrs }) => {
                // TODO: I think type is not useful for execution?
                let instrs = instrs.clone();
                exec(runtime, &*instrs, 0);
            }

            _ => todo!("unhandled instruction: {:?}", instr),
        }
    }
}
