use crate::fun::Fun;
use crate::store::FunAddr;
use crate::value::Value;
use crate::{ExecError, Result};

use std::iter::repeat;

#[derive(Default, Debug)]
pub struct FrameStack {
    pub frames: Vec<Frame>,
}

impl FrameStack {
    pub(crate) fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }
}

#[derive(Debug)]
pub struct Frame {
    pub fun_addr: FunAddr,
    pub locals: Vec<Value>, // includes args
}

impl FrameStack {
    pub(crate) fn clear(&mut self) {
        self.frames.clear();
    }

    pub(crate) fn current(&self) -> Result<&Frame> {
        match self.frames.last() {
            None => exec_panic!("FrameStack::current: call stack empty"),
            Some(frame) => Ok(frame),
        }
    }

    pub(crate) fn current_mut(&mut self) -> Result<&mut Frame> {
        match self.frames.last_mut() {
            None => exec_panic!("FrameStack::current_mut: call stack empty"),
            Some(frame) => Ok(frame),
        }
    }

    pub(crate) fn push(&mut self, fun: &Fun, args: Vec<Value>) {
        self.frames.push(Frame {
            fun_addr: fun.fun_addr(),
            locals: args
                .into_iter()
                .chain(fun.locals().iter().flat_map(|local| {
                    repeat(
                        Value::default(local.value_type())
                            .unwrap_or_else(|| todo!("Uninitialized local")),
                    )
                    .take(local.count() as usize)
                }))
                .collect(),
        });
    }

    pub(crate) fn pop(&mut self) {
        self.frames.pop().unwrap();
    }
}

impl Frame {
    pub fn get_local(&self, idx: u32) -> Result<Value> {
        match self.locals.get(idx as usize) {
            Some(value) => Ok(*value),
            None => exec_panic!(
                "Frame::get_local: local index OOB (n locals={}, local idx={})",
                self.locals.len(),
                idx
            ),
        }
    }

    pub(crate) fn set_local(&mut self, idx: u32, value: Value) -> Result<()> {
        match self.locals.get_mut(idx as usize) {
            Some(slot) => {
                *slot = value;
                Ok(())
            }
            None => Err(ExecError::Panic(format!(
                "Frame::set_local: local index OOB (n locals={}, local idx={})",
                self.locals.len(),
                idx
            ))),
        }
    }
}
