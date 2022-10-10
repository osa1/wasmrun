use crate::value::{Ref, Value};
use crate::{ExecError, Result};

#[derive(Debug)]
pub(crate) struct Stack(Vec<Block>);

#[derive(Debug)]
pub(crate) struct Block {
    /// Value stack of the block
    values: Vec<Value>,

    /// `br` continuation of the block. For `block` and `if` this is generated by
    /// `store::gen_block_bounds`. For `loop` it's the `loop` instruction itself.
    pub(crate) cont: u32,

    /// Number of arguments of this block
    n_args: u32,

    /// Number of return values of this block
    n_rets: u32,

    /// Block or function. The distinction is important as we pop the call frame when returning
    /// from a function block.
    pub(crate) kind: BlockKind,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum BlockKind {
    /// Special block which is not supposed to be popped. Used in entry points in tests and
    /// elsewhere to push function arguments and get return values.
    Top,
    Block,
    Loop,
    Fun,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum EndOrBreak {
    End,
    Break,
}

impl Default for Stack {
    fn default() -> Self {
        Stack(vec![Block {
            values: vec![],
            cont: 0,
            n_args: 0,
            n_rets: 0,
            kind: BlockKind::Top,
        }])
    }
}

impl Stack {
    pub(crate) fn clear(&mut self) {
        *self = Default::default();
    }

    fn current_block_mut(&mut self) -> Result<&mut Block> {
        match self.0.last_mut() {
            None => Err(ExecError::Panic(
                "Stack::current_block_mut: empty stack".to_string(),
            )),
            Some(block) => Ok(block),
        }
    }

    pub(crate) fn pop_value_opt(&mut self) -> Result<Option<Value>> {
        Ok(self.current_block_mut()?.pop_value_opt())
    }

    pub(crate) fn pop_value(&mut self) -> Result<Value> {
        self.pop_value_opt()?
            .ok_or_else(|| ExecError::Panic("Stack::pop: empty stack".to_string()))
    }

    pub(crate) fn pop_i32(&mut self) -> Result<i32> {
        match self.pop_value()? {
            Value::I32(val) => Ok(val),
            other => Err(ExecError::Panic(format!("Stack::pop_i32: {:?}", other))),
        }
    }

    pub(crate) fn pop_i64(&mut self) -> Result<i64> {
        match self.pop_value()? {
            Value::I64(val) => Ok(val),
            other => Err(ExecError::Panic(format!("Stack::pop_i64: {:?}", other))),
        }
    }

    pub(crate) fn pop_i128(&mut self) -> Result<i128> {
        match self.pop_value()? {
            Value::I128(val) => Ok(val),
            other => Err(ExecError::Panic(format!("Stack::pop_i128: {:?}", other))),
        }
    }

    pub(crate) fn pop_f32(&mut self) -> Result<f32> {
        match self.pop_value()? {
            Value::F32(val) => Ok(val),
            other => Err(ExecError::Panic(format!("Stack::pop_f32: {:?}", other))),
        }
    }

    pub(crate) fn pop_f64(&mut self) -> Result<f64> {
        match self.pop_value()? {
            Value::F64(val) => Ok(val),
            other => Err(ExecError::Panic(format!("Stack::pop_f64: {:?}", other))),
        }
    }

    pub(crate) fn pop_ref(&mut self) -> Result<Ref> {
        match self.pop_value()? {
            Value::Ref(val) => Ok(val),
            other => Err(ExecError::Panic(format!("Stack::pop_ref: {:?}", other))),
        }
    }

    pub(crate) fn push_value(&mut self, val: Value) -> Result<()> {
        self.current_block_mut()?.values.push(val);
        Ok(())
    }

    pub(crate) fn push_i32(&mut self, i: i32) -> Result<()> {
        self.push_value(Value::I32(i))
    }

    pub(crate) fn push_i64(&mut self, i: i64) -> Result<()> {
        self.push_value(Value::I64(i))
    }

    pub(crate) fn push_i128(&mut self, i: i128) -> Result<()> {
        self.push_value(Value::I128(i))
    }

    pub(crate) fn push_f32(&mut self, f: f32) -> Result<()> {
        self.push_value(Value::F32(f))
    }

    pub(crate) fn push_f64(&mut self, f: f64) -> Result<()> {
        self.push_value(Value::F64(f))
    }

    pub(crate) fn push_ref(&mut self, r: Ref) -> Result<()> {
        self.push_value(Value::Ref(r))
    }

    pub(crate) fn push_block(&mut self, cont: u32, n_args: u32, n_rets: u32) {
        self.0.push(Block {
            values: vec![],
            cont,
            n_args,
            n_rets,
            kind: BlockKind::Block,
        })
    }

    pub(crate) fn push_loop(&mut self, cont: u32, n_args: u32, n_rets: u32) {
        self.0.push(Block {
            values: vec![],
            cont,
            n_args,
            n_rets,
            kind: BlockKind::Loop,
        })
    }

    pub(crate) fn push_fun_block(&mut self, cont: u32, n_args: u32, n_rets: u32) {
        self.0.push(Block {
            values: vec![],
            cont,
            n_args,
            n_rets,
            kind: BlockKind::Fun,
        })
    }

    pub(crate) fn pop_block(&mut self) -> Result<Block> {
        let block = self
            .0
            .pop()
            .ok_or_else(|| ExecError::Panic("Stack::pop_block: empty".to_string()))?;

        if block.kind == BlockKind::Top {
            return Err(ExecError::Panic(format!(
                "Stack::pop_block: popped {:?}",
                block.kind
            )));
        }

        Ok(block)
    }

    pub(crate) fn pop_fun_block(&mut self) -> Result<Block> {
        loop {
            let block = self.pop_block()?;
            if block.kind == BlockKind::Fun {
                return Ok(block);
            }
        }
    }

    pub(crate) fn return_arity(&mut self, depth: u32, end_or_break: EndOrBreak) -> Result<u32> {
        match self.0.get(self.0.len() - 1 - depth as usize) {
            None => Err(ExecError::Panic(format!(
                "Stack::return_arity: depth {} is greater than stack depth {}",
                depth,
                self.0.len()
            ))),
            Some(Block {
                kind,
                n_args,
                n_rets,
                ..
            }) => match kind {
                BlockKind::Top => Err(ExecError::Panic(
                    "Stack::return_arity of top frame".to_string(),
                )),
                BlockKind::Block | BlockKind::Fun => Ok(*n_rets),
                BlockKind::Loop => match end_or_break {
                    EndOrBreak::End => Ok(*n_rets),
                    EndOrBreak::Break => Ok(*n_args),
                },
            },
        }
    }
}

impl Block {
    fn pop_value_opt(&mut self) -> Option<Value> {
        self.values.pop()
    }
}

pub(crate) trait StackValue: Sized {
    fn pop(stack: &mut Stack) -> Result<Self>;
    fn push(&self, stack: &mut Stack);
}

impl StackValue for u32 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        Ok(stack.pop_i32()? as u32)
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i32(*self as i32).unwrap();
    }
}

impl StackValue for i32 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        stack.pop_i32()
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i32(*self).unwrap();
    }
}

impl StackValue for i64 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        stack.pop_i64()
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i64(*self).unwrap();
    }
}

impl StackValue for u64 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        Ok(stack.pop_i64()? as u64)
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i64(*self as i64).unwrap();
    }
}

impl StackValue for i128 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        stack.pop_i128()
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i128(*self as i128).unwrap();
    }
}

impl StackValue for f32 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        stack.pop_f32()
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_f32(*self).unwrap();
    }
}

impl StackValue for f64 {
    fn pop(stack: &mut Stack) -> Result<Self> {
        stack.pop_f64()
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_f64(*self).unwrap();
    }
}

impl StackValue for bool {
    fn pop(stack: &mut Stack) -> Result<Self> {
        Ok(stack.pop_i32()? == 1)
    }

    fn push(&self, stack: &mut Stack) {
        stack.push_i32(if *self { 1 } else { 0 }).unwrap();
    }
}
