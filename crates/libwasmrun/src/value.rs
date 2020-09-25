use parity_wasm::elements as wasm;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Value {
    pub fn default_i32() -> Self {
        Value::I32(0)
    }

    pub fn default_i64() -> Self {
        Value::I64(0)
    }

    pub fn default_f32() -> Self {
        Value::F32(0f32)
    }

    pub fn default_f64() -> Self {
        Value::F64(0f64)
    }

    pub fn default(ty: wasm::ValueType) -> Self {
        match ty {
            wasm::ValueType::I32 => Value::default_i32(),
            wasm::ValueType::I64 => Value::default_i64(),
            wasm::ValueType::F32 => Value::default_f32(),
            wasm::ValueType::F64 => Value::default_f64(),
            wasm::ValueType::V128 => todo!("Unsupported value type: V128"),
        }
    }
}
