#![allow(clippy::unusual_byte_groupings)]

use crate::store::{ArrayAddr, ExnAddr, ExternAddr, FunAddr, ModuleAddr, Store, StructAddr};

use libwasmrun_syntax as wasm;

use std::fmt;

#[derive(Clone, Copy)]
pub enum Value {
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
    Ref(Ref),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ref {
    Null(ModuleAddr, wasm::HeapType),
    Func(FunAddr),
    Exn(ExnAddr),
    Struct(StructAddr),
    Array(ArrayAddr),

    /// A host reference.
    ///
    /// When internalized (with `any.convert_extern`), this becomes an `any` that is not `eq`.
    Host {
        value: u32,
        internal: bool,
    },

    /// A Wasm heap reference.
    Extern(ExternAddr),

    /// A 31-bit integer disguised as a pointer.
    I31(i32),
}

impl Ref {
    pub fn is_null(&self) -> bool {
        matches!(self, Ref::Null(_, _))
    }

    pub fn to_i31(&self) -> Option<i32> {
        match self {
            Ref::I31(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_struct_addr(&self) -> Option<StructAddr> {
        match self {
            Ref::Struct(addr) => Some(*addr),
            _ => None,
        }
    }

    pub fn as_array_addr(&self) -> Option<ArrayAddr> {
        match self {
            Ref::Array(addr) => Some(*addr),
            _ => None,
        }
    }

    pub fn rtt(&self, store: &Store) -> (ModuleAddr, wasm::HeapType) {
        match self {
            Ref::Null(module_addr, heap_ty) => (*module_addr, *heap_ty),

            Ref::Func(fun_addr) => store.get_fun(*fun_addr).rtt(),

            Ref::Exn(exn_addr) => {
                let exn = store.get_exn(*exn_addr);
                let tag = store.get_tag(exn.tag_addr);
                tag.rtt()
            }

            Ref::Host { value: _, internal } => (
                ModuleAddr(0),
                if *internal {
                    wasm::HeapType::Any
                } else {
                    wasm::HeapType::Extern
                },
            ),

            Ref::Extern { .. } => (ModuleAddr(0), wasm::HeapType::Extern),

            Ref::Struct(struct_addr) => store.get_struct(*struct_addr).rtt(),

            Ref::Array(array_addr) => store.get_array(*array_addr).rtt(),

            Ref::I31(_) => (ModuleAddr(0), wasm::HeapType::I31),
        }
    }
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
    pub fn new_host_ref(value: u32) -> Self {
        Value::Ref(Ref::Host {
            value,
            internal: true,
        })
    }

    pub(crate) fn default_from_storage_type(
        module_addr: ModuleAddr,
        storage_ty: &wasm::StorageType,
    ) -> Self {
        match storage_ty {
            wasm::StorageType::Val(val_ty) => Self::default_from_value_type(module_addr, val_ty),
            wasm::StorageType::Packed(_) => Value::I32(0),
        }
    }

    pub(crate) fn default_from_value_type(
        module_addr: ModuleAddr,
        value_ty: &wasm::ValueType,
    ) -> Self {
        match value_ty {
            wasm::ValueType::I32 => Self::default_i32(),
            wasm::ValueType::I64 => Self::default_i64(),
            wasm::ValueType::F32 => Self::default_f32(),
            wasm::ValueType::F64 => Self::default_f64(),
            wasm::ValueType::V128 => Self::default_i128(),
            wasm::ValueType::Reference(ref_ty) => {
                Self::default_from_reference_type(module_addr, ref_ty)
            }
        }
    }

    pub(crate) fn default_from_reference_type(
        module_addr: ModuleAddr,
        ref_ty: &wasm::ReferenceType,
    ) -> Self {
        Value::Ref(Ref::Null(module_addr, ref_ty.heap_ty))
    }

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

    pub(crate) fn default_i128() -> Self {
        Value::I128(0i128)
    }

    pub(crate) fn expect_i32(&self) -> i32 {
        match self {
            Value::I32(i) => *i,
            _ => panic!("expect_i32: found {:?}", self),
        }
    }

    pub(crate) fn expect_i64(&self) -> i64 {
        match self {
            Value::I64(i) => *i,
            _ => panic!("expect_i64: found {:?}", self),
        }
    }

    pub(crate) fn expect_ref(&self) -> Ref {
        match self {
            Value::Ref(ref_) => *ref_,
            _ => panic!("expect_ref: found {:?}", self),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::I32(i) => write!(fmt, "{}i32", i),
            Value::I64(i) => write!(fmt, "{}i64", i),
            Value::I128(i) => write!(fmt, "{}i128", i),
            Value::F32(f) => write!(fmt, "{}f32 ({:?})", f, F32Debug(*f)),
            Value::F64(f) => write!(fmt, "{}f64 ({:?})", f, F64Debug(*f)),
            Value::Ref(r) => r.fmt(fmt),
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
