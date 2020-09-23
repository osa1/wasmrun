use super::value::Value;
use crate::{ExecError, Result};

#[derive(Debug, Default)]
pub struct Stack(Vec<Value>);

impl Stack {
    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn pop_value_opt(&mut self) -> Option<Value> {
        match self.0.pop() {
            Some(val) => Some(val),
            None => None,
        }
    }

    pub fn pop_value(&mut self) -> Result<Value> {
        self.pop_value_opt()
            .ok_or_else(|| ExecError::Panic("Stack::pop: empty stack".to_string()))
    }

    pub fn pop_i32(&mut self) -> Result<i32> {
        match self.0.pop() {
            Some(Value::I32(val)) => Ok(val),
            Some(other) => Err(ExecError::Panic(format!("Stack::pop_i32: {:#?}", other))),
            None => Err(ExecError::Panic("Stack::pop_i32: empty stack".to_string())),
        }
    }

    pub fn pop_i64(&mut self) -> Result<i64> {
        match self.0.pop() {
            Some(Value::I64(val)) => Ok(val),
            Some(other) => Err(ExecError::Panic(format!("Stack::pop_i64: {:#?}", other))),
            None => Err(ExecError::Panic("Stack::pop_i64: empty stack".to_string())),
        }
    }

    pub fn push_value(&mut self, val: Value) {
        self.0.push(val)
    }

    pub fn push_i32(&mut self, i: i32) {
        self.0.push(Value::I32(i))
    }

    pub fn push_u32(&mut self, i: u32) {
        self.0.push(Value::I32(i as i32))
    }

    pub fn push_i64(&mut self, i: i64) {
        self.0.push(Value::I64(i))
    }

    pub fn push_f32(&mut self, f: f32) {
        self.0.push(Value::F32(f))
    }

    pub fn push_f64(&mut self, f: f64) {
        self.0.push(Value::F64(f))
    }

    pub fn push_bool(&mut self, bool: bool) {
        self.push_u32(if bool { 1 } else { 0 })
    }
}

pub trait StackValue: Sized {
    fn pop(stack: &mut Stack) -> Result<Self>;
    fn push(&self, stack: &mut Stack);
}

impl StackValue for u32 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        Ok(stack.pop_i32()? as u32)
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i32(*self as i32);
    }
}

impl StackValue for i32 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        stack.pop_i32()
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i32(*self);
    }
}

impl StackValue for i64 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        stack.pop_i64()
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i64(*self);
    }
}

impl StackValue for u64 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        Ok(stack.pop_i64()? as u64)
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i64(*self as i64);
    }
}

impl StackValue for bool {
    fn pop(stack: &mut Stack) -> Result<Self> {
        Ok(stack.pop_i32()? == 1)
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i32(if *self { 1 } else { 0 });
    }
}
