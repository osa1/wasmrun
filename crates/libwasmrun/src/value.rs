#![allow(clippy::inconsistent_digit_grouping)]

use std::fmt;

use parity_wasm::elements as wasm;

#[derive(Clone, Copy)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

// TODO: Make this a const once from_bits is a const fn
pub(crate) fn canonical_f32_nan() -> f32 {
    f32::from_bits(0b0_11111111_10000000000000000000000)
}

pub(crate) fn canonical_f64_nan() -> f64 {
    f64::from_bits(0b0_11111111111_1000000000000000000000000000000000000000000000000000)
}

pub(crate) fn canonicalize_f32_nan(f: f32) -> f32 {
    if f.is_nan() {
        canonical_f32_nan()
    } else {
        f
    }
}

pub(crate) fn canonicalize_f64_nan(f: f64) -> f64 {
    if f.is_nan() {
        canonical_f64_nan()
    } else {
        f
    }
}

impl Value {
    pub(crate) fn default_i32() -> Self {
        Value::I32(0)
    }

    pub(crate) fn default_i64() -> Self {
        Value::I64(0)
    }

    pub(crate) fn default_f32() -> Self {
        Value::F32(0f32)
    }

    pub(crate) fn default_f64() -> Self {
        Value::F64(0f64)
    }

    pub(crate) fn default(ty: wasm::ValueType) -> Self {
        match ty {
            wasm::ValueType::I32 => Value::default_i32(),
            wasm::ValueType::I64 => Value::default_i64(),
            wasm::ValueType::F32 => Value::default_f32(),
            wasm::ValueType::F64 => Value::default_f64(),
            wasm::ValueType::V128 => todo!("Unsupported value type: V128"),
        }
    }

    pub fn expect_i32(&self) -> i32 {
        match self {
            Value::I32(i) => *i,
            Value::I64(_) | Value::F32(_) | Value::F64(_) => panic!(),
        }
    }

    pub fn expect_i64(&self) -> i64 {
        match self {
            Value::I64(i) => *i,
            Value::I32(_) | Value::F32(_) | Value::F64(_) => panic!(),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::I32(i) => write!(fmt, "{}i32", i),
            Value::I64(i) => write!(fmt, "{}i64", i),
            Value::F32(f) => write!(fmt, "{}f32 ({:?})", f, F32Debug(*f)),
            Value::F64(f) => write!(fmt, "{}f64 ({:?})", f, F64Debug(*f)),
        }
    }
}

struct F32Debug(f32);

impl fmt::Debug for F32Debug {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bits = self.0.to_bits();
        let sign = bits >> 31 == 1;
        let exp = (bits >> 23) & ((1 << 8) - 1);
        let signi = bits & ((1 << 23) - 1);
        fmt_float(sign, u64::from(exp), 8, u64::from(signi), 23, f)
    }
}

struct F64Debug(f64);

impl fmt::Debug for F64Debug {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bits = self.0.to_bits();
        let sign = bits >> 63 == 1;
        let exp = (bits >> 52) & ((1 << 11) - 1);
        let signi = bits & ((1 << 52) - 1);
        fmt_float(sign, exp, 11, signi, 52, f)
    }
}

fn fmt_float(
    sign: bool,
    mut exp: u64,
    exp_digits: u8,
    mut signi: u64,
    signi_digits: u8,
    f: &mut fmt::Formatter,
) -> fmt::Result {
    let mut s = String::new();

    for _ in 0..signi_digits {
        s.push(char::from(b'0' + (signi & 0b1) as u8));
        signi >>= 1;
    }

    s.push('_');

    for _ in 0..exp_digits {
        s.push(char::from(b'0' + (exp & 0b1) as u8));
        exp >>= 1;
    }

    s.push('_');

    if sign {
        s.push('1');
    } else {
        s.push('0');
    }

    unsafe {
        s.as_mut_vec().reverse();
    }
    std::fmt::Display::fmt(&s, f)
}
