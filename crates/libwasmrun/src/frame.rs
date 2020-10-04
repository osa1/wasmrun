use crate::fun::WasmFun;
use crate::store::FunAddr;
use crate::value::Value;
use crate::{ExecError, Result};

use parity_wasm::elements as wasm;

use std::iter::repeat;

#[derive(Default, Debug)]
pub(crate) struct FrameStack(Vec<Frame>);

impl FrameStack {
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Debug)]
pub(crate) struct Frame {
    pub(crate) fun_addr: FunAddr,
    pub(crate) locals: Vec<Value>, // includes args
}

impl FrameStack {
    pub(crate) fn clear(&mut self) {
        self.0.clear();
    }

    pub(crate) fn current(&self) -> Result<&Frame> {
        match self.0.last() {
            None => Err(ExecError::Panic(
                "FrameStack::current: call stack empty".to_string(),
            )),
            Some(frame) => Ok(frame),
        }
    }

    pub(crate) fn current_mut(&mut self) -> Result<&mut Frame> {
        match self.0.last_mut() {
            None => Err(ExecError::Panic(
                "FrameStack::current_mut: call stack empty".to_string(),
            )),
            Some(frame) => Ok(frame),
        }
    }

    pub(crate) fn push(&mut self, fun: &WasmFun, arg_tys: &[wasm::ValueType]) {
        self.0.push(Frame {
            fun_addr: fun.fun_addr,
            locals: arg_tys
                .iter()
                .map(|ty| Value::default(*ty))
                .chain(fun.fun.locals().iter().flat_map(|local| {
                    repeat(Value::default(local.value_type())).take(local.count() as usize)
                }))
                .collect(),
        });
    }

    pub(crate) fn pop(&mut self) {
        self.0.pop().unwrap();
    }
}

impl Frame {
    pub(crate) fn get_local(&self, idx: u32) -> Result<Value> {
        match self.locals.get(idx as usize) {
            Some(value) => Ok(*value),
            None => Err(ExecError::Panic(format!(
                "Frame::get_local: local index OOB (n locals={}, local idx={})",
                self.locals.len(),
                idx
            ))),
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
