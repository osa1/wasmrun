use crate::parser::FuncType;

type Addr = u32;

struct Module {
    types: Vec<FuncType>,
    func_addrs: Vec<Addr>,
    table_addrs: Vec<Addr>,
    // No need for mem_addrs as there can be at most one memory instance currently in Wasm
    global_addrs: Vec<Addr>,
    // exports:  TODO
}

impl Module {
    // fn new(
}
