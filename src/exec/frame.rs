use super::store::Func;
use super::value::Value;

use std::iter::repeat;

#[derive(Default, Debug)]
pub struct FrameStack(Vec<Frame>);

impl FrameStack {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Debug)]
pub struct Frame {
    pub fun_idx: u32,
    pub locals: Vec<Value>,
}

impl FrameStack {
    pub fn current(&self) -> &Frame {
        match self.0.last() {
            None => panic!("FrameStack::current: call stack empty"),
            Some(frame) => frame,
        }
    }

    pub fn current_mut(&mut self) -> &mut Frame {
        match self.0.last_mut() {
            None => panic!("FrameStack::current_mut: call stack empty"),
            Some(frame) => frame,
        }
    }

    pub(super) fn push(&mut self, fun: &Func) {
        self.0.push(Frame {
            fun_idx: fun.fun_idx as u32,
            locals: fun
                .fun
                .locals()
                .iter()
                .flat_map(|local| repeat(Value::Uninitialized).take(local.count() as usize))
                .collect(),
        });
    }

    pub(super) fn pop(&mut self) {
        self.0.pop().unwrap();
    }
}

impl Frame {
    pub fn get_local(&self, idx: u32) -> Value {
        match self.locals.get(idx as usize) {
            Some(value) => *value,
            None => panic!(
                "Frame::get_local: local index OOB (n locals={}, local idx={})",
                self.locals.len(),
                idx
            ),
        }
    }

    pub fn set_local(&mut self, idx: u32, value: Value) {
        match self.locals.get_mut(idx as usize) {
            Some(slot) => {
                *slot = value;
            }
            None => panic!(
                "Frame::set_local: local index OOB (n locals={}, local idx={})",
                self.locals.len(),
                idx
            ),
        }
    }
}
