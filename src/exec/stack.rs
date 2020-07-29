use super::store::ModuleIdx;
use super::value::Value;

#[derive(Debug, Default)]
pub struct Stack(Vec<StackValue>);

#[derive(Debug)]
pub enum StackValue {
    Value(Value),
    ReturnAddr(ReturnAddr),
}

#[derive(Debug)]
pub struct ReturnAddr {
    /// Module index of the caller: the function we return to lives in this module.
    module_idx: ModuleIdx,
    /// Index of the caller function in the caller's module.
    fun_id: u32,
    /// Instruction index in the caller function. One index per block. For example, if this is `[5,
    /// 10]`, then instruction 5 in the caller is a `block` or `loop`, and the call instruction is
    /// 10th instruction in the block or loop.
    instr_idx: Vec<u32>,
}

impl Stack {
    pub fn pop_value(&mut self) -> Value {
        match self.0.pop() {
            Some(StackValue::Value(val)) => val,
            Some(StackValue::ReturnAddr(_)) => panic!("Stack::pop: call frame empty"),
            None => panic!("Stack::pop: empty stack"),
        }
    }

    pub fn pop_i32(&mut self) -> i32 {
        match self.0.pop() {
            Some(StackValue::Value(Value::I32(val))) => val,
            Some(StackValue::Value(other)) => panic!("Stack::pop_i32: {:#?}", other),
            Some(StackValue::ReturnAddr(_)) => panic!("Stack::pop_i32: call frame empty"),
            None => panic!("Stack::pop_i32: empty stack"),
        }
    }

    pub fn push_value(&mut self, val: Value) {
        self.0.push(StackValue::Value(val))
    }

    pub fn push_i32(&mut self, i: i32) {
        self.0.push(StackValue::Value(Value::I32(i)))
    }

    pub fn push_u32(&mut self, i: u32) {
        self.0.push(StackValue::Value(Value::I32(i as i32)))
    }

    pub fn push_i64(&mut self, i: i64) {
        self.0.push(StackValue::Value(Value::I64(i)))
    }

    pub fn push_f32(&mut self, f: f32) {
        self.0.push(StackValue::Value(Value::F32(f)))
    }

    pub fn push_f64(&mut self, f: f64) {
        self.0.push(StackValue::Value(Value::F64(f)))
    }

    pub fn push_bool(&mut self, bool: bool) {
        self.push_u32(if bool { 1 } else { 0 })
    }
}
