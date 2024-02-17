use libwasmrun::exec::{self, Runtime, Trap};
use libwasmrun::store::{ExternAddr, ModuleAddr};
use libwasmrun::{ExecError, Ref, Value};
use libwasmrun_syntax as wasm;

use fxhash::FxHashMap;
use wast::core::{HeapType, WastArgCore, WastRetCore};
use wast::token::{Id, Index, Span};
use wast::{QuoteWat, WastArg, WastDirective, WastExecute, WastInvoke, Wat};

pub struct TestFileRunner<'a> {
    pub file_contents: &'a str,
    pub rt: Runtime,
    pub module_addr: Option<ModuleAddr>,
    pub modules: FxHashMap<String, ModuleAddr>,
    pub failing_lines: Vec<usize>,

    /// Line number of the current test. First line is 0.
    line_num: usize,
}

#[derive(Debug)]
enum Error {
    /// A wasmrun execution error.
    Exec(ExecError),

    /// Deserialization of a module failed.
    Deserialize(wasm::Error),

    /// A used was missing. This probably means that a module registration before the test failed.
    MissingModule,

    /// A global was missing.
    MissingGlobal,

    /// An "assert trap" test succeeded without trapping.
    AssertTrapWorked,

    /// An "assert trap" test failed with unexpected error.
    #[allow(clippy::enum_variant_names)]
    AssertTrapUnexpectedError(Box<Error>),

    /// Test used stuff we don't support yet, such as components and threads.
    UnsupportedStuff,

    /// Can't pop return value of an invocation. Execution probably pushed less number of values
    /// than expected.
    CantPopReturnValue,

    /// Execution returned incorrect result.
    IncorrectResult,
}

type Result<A> = std::result::Result<A, Error>;

impl From<ExecError> for Error {
    fn from(err: ExecError) -> Self {
        Error::Exec(err)
    }
}

impl From<wasm::Error> for Error {
    fn from(err: wasm::Error) -> Self {
        Error::Deserialize(err)
    }
}

impl<'a> TestFileRunner<'a> {
    pub fn new(file_contents: &'a str) -> TestFileRunner<'a> {
        Self {
            file_contents,
            rt: Runtime::new_test(),
            module_addr: None,
            modules: Default::default(),
            failing_lines: Default::default(),
            line_num: 0,
        }
    }

    /// Run a single test directive. Reports failures to stdout.
    pub fn run_directive(&mut self, directive: WastDirective) {
        self.line_num = directive_line_number(&directive, self.file_contents);
        if let Err(err) = self.run_directive_(directive) {
            println!("{}: {:?}", self.line_num + 1, err);
            self.failing_lines.push(self.line_num + 1);
        }
    }

    fn run_directive_(&mut self, directive: WastDirective) -> Result<()> {
        match directive {
            WastDirective::AssertMalformed { .. }
            | WastDirective::AssertInvalid { .. }
            | WastDirective::AssertExhaustion { .. }
            | WastDirective::AssertUnlinkable { .. } => {
                // We don't test these yet, skip
                Ok(())
            }

            WastDirective::Wat(mut module) => self.run_quote_wat_directive(&mut module),

            WastDirective::Register {
                span: _,
                name,
                module,
            } => {
                let module_addr = self.get_module_addr(&module)?;
                self.rt.register_module(name.to_owned(), module_addr);
                Ok(())
            }

            WastDirective::Invoke(invoke) => self.run_invoke_directive(&invoke),

            WastDirective::AssertTrap {
                span: _,
                exec,
                message,
            } => {
                let ret = match exec {
                    WastExecute::Invoke(invoke) => self.run_invoke_directive(&invoke),
                    WastExecute::Wat(mut wat) => self.run_wat_directive(&mut wat),
                    WastExecute::Get { module, global } => {
                        self.run_global_get_directive(&module, global).map(|_| ())
                    }
                };

                match ret {
                    Ok(()) => Err(Error::AssertTrapWorked),
                    Err(err) => {
                        if let Error::Exec(ExecError::Trap(trap)) = err {
                            let trap_msg = trap_expected_msg(trap);
                            if message.starts_with(trap_msg) {
                                Ok(())
                            } else {
                                Err(Error::AssertTrapUnexpectedError(Box::new(Error::Exec(
                                    ExecError::Trap(trap),
                                ))))
                            }
                        } else {
                            Err(Error::AssertTrapUnexpectedError(Box::new(err)))
                        }
                    }
                }
            }

            WastDirective::AssertReturn {
                span: _,
                exec,
                results,
            } => match exec {
                WastExecute::Invoke(invoke) => {
                    self.run_invoke_directive(&invoke)?;

                    let mut expected_vals: Vec<WastRetCore> = Vec::with_capacity(results.len());

                    for result in results {
                        match result {
                            wast::WastRet::Core(value) => expected_vals.push(value),
                            wast::WastRet::Component(_) => return Err(Error::UnsupportedStuff),
                        }
                    }

                    let mut found_vals: Vec<Value> = Vec::with_capacity(expected_vals.len());

                    for _ in 0..expected_vals.len() {
                        match self.rt.pop_value() {
                            Some(value) => found_vals.push(value),
                            None => return Err(Error::CantPopReturnValue),
                        }
                    }

                    expected_vals.reverse();

                    if found_vals.len() != expected_vals.len() {
                        return Err(Error::IncorrectResult);
                    }

                    let module_addr = self.get_module_addr(&None)?;
                    if !test_vals(&self.rt, module_addr, &expected_vals, &found_vals) {
                        return Err(Error::IncorrectResult);
                    }

                    Ok(())
                }
                WastExecute::Wat(_) => panic!("Module in assert_return test"),

                WastExecute::Get { module, global } => {
                    let module_addr = self.get_module_addr(&module)?;
                    let expected_val = match &results[0] {
                        wast::WastRet::Core(val) => val,
                        wast::WastRet::Component(_) => return Err(Error::UnsupportedStuff),
                    };
                    match self.rt.get_global(module_addr, global) {
                        Some(value) => {
                            if !test_val(&self.rt, module_addr, expected_val, &value) {
                                return Err(Error::IncorrectResult);
                            }
                        }
                        None => return Err(Error::IncorrectResult),
                    }
                    Ok(())
                }
            },

            WastDirective::AssertException { span: _, exec } => {
                match exec {
                    WastExecute::Invoke(invoke) => {
                        self.run_invoke_directive(&invoke)?;
                        if self.rt.unhandled_exception.is_none() {
                            return Err(Error::IncorrectResult);
                        }
                    }

                    WastExecute::Wat(_) => todo!(),

                    WastExecute::Get {
                        module: _,
                        global: _,
                    } => todo!(),
                }

                Ok(())
            }

            WastDirective::Thread(_) | WastDirective::Wait { .. } => Err(Error::UnsupportedStuff),
        }
    }

    fn run_quote_wat_directive(&mut self, module: &mut QuoteWat) -> Result<()> {
        let (_span, name, module): (Span, Option<String>, Vec<u8>) = match module {
            QuoteWat::Wat(Wat::Module(m)) => (
                m.span,
                m.id.map(|id| id.name().to_owned()),
                m.encode().unwrap(),
            ),
            QuoteWat::QuoteModule(span, _) => (*span, None, module.encode().unwrap()),
            QuoteWat::QuoteComponent(_, _) | QuoteWat::Wat(Wat::Component(_)) => {
                return Err(Error::UnsupportedStuff);
            }
        };

        let module = wasm::deserialize_buffer(&module).map_err(Error::from)?;
        let module_addr = exec::instantiate(&mut self.rt, module)?;
        self.module_addr = Some(module_addr);
        if let Some(name) = name {
            self.modules.insert(name, module_addr);
        }
        Ok(())
    }

    fn run_wat_directive(&mut self, module: &mut Wat) -> Result<()> {
        let module = match module {
            Wat::Module(module) => module,
            Wat::Component(_) => return Err(Error::UnsupportedStuff),
        };

        let _span = module.span;
        let name = module.id.map(|id| id.name().to_owned());
        let module = module.encode().unwrap();

        let module = wasm::deserialize_buffer(&module).map_err(Error::from)?;
        let module_addr = exec::instantiate(&mut self.rt, module)?;
        self.module_addr = Some(module_addr);
        if let Some(name) = name {
            self.modules.insert(name, module_addr);
        }
        Ok(())
    }

    fn run_global_get_directive(&mut self, module: &Option<Id>, global: &str) -> Result<Value> {
        let module_addr = self.get_module_addr(module)?;
        self.rt
            .get_global(module_addr, global)
            .ok_or(Error::MissingGlobal)
    }

    fn run_invoke_directive(&mut self, invoke: &WastInvoke) -> Result<()> {
        let WastInvoke {
            span: _,
            module,
            name,
            args,
        } = invoke;

        let module_addr = self.get_module_addr(module)?;

        let args = self.eval_values(args)?;

        self.rt.clear_stack();
        for arg in args {
            self.rt.push_value(arg);
        }

        exec::invoke_by_name(&mut self.rt, module_addr, name)?;

        Ok(exec::finish(&mut self.rt)?)
    }

    fn get_module_addr(&mut self, module_name: &Option<Id>) -> Result<ModuleAddr> {
        match module_name {
            Some(module_name) => match self.modules.get(module_name.name()) {
                Some(module_addr) => Ok(*module_addr),
                None => Err(Error::MissingModule),
            },
            None => match &self.module_addr {
                Some(module_addr) => Ok(*module_addr),
                None => Err(Error::MissingModule),
            },
        }
    }

    fn eval_values(&mut self, values: &[WastArg]) -> Result<Vec<Value>> {
        let mut wasm_values = Vec::with_capacity(values.len());
        for value in values {
            wasm_values.push(self.eval_value(value)?);
        }
        Ok(wasm_values)
    }

    fn eval_value(&mut self, value: &WastArg) -> Result<Value> {
        match value {
            WastArg::Core(value) => self.eval_value_(value),
            WastArg::Component(_) => Err(Error::UnsupportedStuff),
        }
    }

    fn eval_value_(&mut self, value: &WastArgCore) -> Result<Value> {
        Ok(match value {
            WastArgCore::I32(i) => Value::I32(*i),

            WastArgCore::I64(i) => Value::I64(*i),

            WastArgCore::F32(f) => Value::F32(f32::from_bits(f.bits)),

            WastArgCore::F64(f) => Value::F64(f64::from_bits(f.bits)),

            WastArgCore::V128(i) => {
                let mut bytes = [0u8; 16];
                match i {
                    wast::core::V128Const::I8x16(lanes) => {
                        for i in 0..16 {
                            bytes[i] = lanes[i] as u8;
                        }
                    }

                    wast::core::V128Const::I16x8(lanes) => {
                        for i in 0..8 {
                            bytes[i * 2..i * 2 + 2].copy_from_slice(&lanes[i].to_le_bytes());
                        }
                    }

                    wast::core::V128Const::I32x4(lanes) => {
                        for i in 0..4 {
                            bytes[i * 4..i * 4 + 4].copy_from_slice(&lanes[i].to_le_bytes());
                        }
                    }

                    wast::core::V128Const::I64x2(lanes) => {
                        for i in 0..2 {
                            bytes[i * 8..i * 8 + 8].copy_from_slice(&lanes[i].to_le_bytes());
                        }
                    }

                    wast::core::V128Const::F32x4(lanes) => {
                        for i in 0..4 {
                            bytes[i * 4..i * 4 + 4].copy_from_slice(&lanes[i].bits.to_le_bytes());
                        }
                    }

                    wast::core::V128Const::F64x2(lanes) => {
                        for i in 0..2 {
                            bytes[i * 8..i * 8 + 8].copy_from_slice(&lanes[i].bits.to_le_bytes());
                        }
                    }
                }
                Value::I128(i128::from_le_bytes(bytes))
            }

            WastArgCore::RefNull(ty) => Value::Ref(Ref::Null(match ty {
                wast::core::HeapType::Func => wasm::HeapType::Func,
                wast::core::HeapType::Extern => wasm::HeapType::Extern,
                wast::core::HeapType::Any => wasm::HeapType::Any,
                wast::core::HeapType::Eq => wasm::HeapType::Eq,
                wast::core::HeapType::Struct => wasm::HeapType::Struct,
                wast::core::HeapType::Array => wasm::HeapType::Array,
                wast::core::HeapType::I31 => wasm::HeapType::I31,
                wast::core::HeapType::NoFunc => wasm::HeapType::NoFunc,
                wast::core::HeapType::NoExtern => wasm::HeapType::NoExtern,
                wast::core::HeapType::Exn => wasm::HeapType::Exn,
                wast::core::HeapType::None => wasm::HeapType::None,
                wast::core::HeapType::Concrete(wast::token::Index::Num(idx, _)) => {
                    wasm::HeapType::TypeIdx(*idx)
                }
                wast::core::HeapType::Concrete(wast::token::Index::Id(_)) => todo!(),
            })),

            WastArgCore::RefHost(addr) | WastArgCore::RefExtern(addr) => {
                Value::Ref(Ref::Extern(ExternAddr(*addr)))
            }
        })
    }
}

/// Check whether a list of wasmrun values match the expected spec test values
fn test_vals(
    rt: &Runtime,
    module_addr: ModuleAddr,
    expected_vals: &[WastRetCore],
    found_vals: &[Value],
) -> bool {
    expected_vals.len() == found_vals.len()
        && expected_vals
            .iter()
            .zip(found_vals.iter())
            .all(|(a, b)| test_val(rt, module_addr, a, b))
}

/// Check whether a wasmrun value matches the expected spec test value
fn test_val(rt: &Runtime, module_addr: ModuleAddr, expected: &WastRetCore, found: &Value) -> bool {
    match (expected, found) {
        (WastRetCore::I32(i1), Value::I32(i2)) => i1 == i2,

        (WastRetCore::I64(i1), Value::I64(i2)) => i1 == i2,

        (WastRetCore::F32(pat), Value::F32(f2)) => match pat {
            wast::core::NanPattern::CanonicalNan => is_f32_canonical_nan(*f2),
            wast::core::NanPattern::ArithmeticNan => is_f32_arithmetic_nan(*f2),
            wast::core::NanPattern::Value(f1) => f1.bits == f2.to_bits(),
        },

        (WastRetCore::F64(pat), Value::F64(f2)) => match pat {
            wast::core::NanPattern::CanonicalNan => is_f64_canonical_nan(*f2),
            wast::core::NanPattern::ArithmeticNan => is_f64_arithmetic_nan(*f2),
            wast::core::NanPattern::Value(f1) => f1.bits == f2.to_bits(),
        },

        (WastRetCore::V128(v1), Value::I128(v2)) => {
            let v2 = v2.to_le_bytes();
            match v1 {
                wast::core::V128Pattern::I8x16(lanes) => {
                    for i in 0..16 {
                        if v2[i] != lanes[i] as u8 {
                            return false;
                        }
                    }
                }

                wast::core::V128Pattern::I16x8(lanes) => {
                    for i in 0..8 {
                        if v2[i * 2..i * 2 + 2] != lanes[i].to_le_bytes() {
                            return false;
                        }
                    }
                }

                wast::core::V128Pattern::I32x4(lanes) => {
                    for i in 0..4 {
                        if v2[i * 4..i * 4 + 4] != lanes[i].to_le_bytes() {
                            return false;
                        }
                    }
                }

                wast::core::V128Pattern::I64x2(lanes) => {
                    for i in 0..2 {
                        if v2[i * 8..i * 8 + 8] != lanes[i].to_le_bytes() {
                            return false;
                        }
                    }
                }

                wast::core::V128Pattern::F32x4(lanes) => {
                    for i in 0..4 {
                        let value = u32::from_le_bytes(v2[i * 4..i * 4 + 4].try_into().unwrap());
                        let value_matches = match lanes[i] {
                            wast::core::NanPattern::CanonicalNan => {
                                is_f32_canonical_nan(f32::from_bits(value))
                            }
                            wast::core::NanPattern::ArithmeticNan => {
                                is_f32_arithmetic_nan(f32::from_bits(value))
                            }
                            wast::core::NanPattern::Value(f1) => f1.bits == value,
                        };
                        if !value_matches {
                            return false;
                        }
                    }
                }

                wast::core::V128Pattern::F64x2(lanes) => {
                    for i in 0..2 {
                        let value = u64::from_le_bytes(v2[i * 8..i * 8 + 8].try_into().unwrap());
                        let value_matches = match lanes[i] {
                            wast::core::NanPattern::CanonicalNan => {
                                is_f64_canonical_nan(f64::from_bits(value))
                            }
                            wast::core::NanPattern::ArithmeticNan => {
                                is_f64_arithmetic_nan(f64::from_bits(value))
                            }
                            wast::core::NanPattern::Value(f1) => f1.bits == value,
                        };
                        if !value_matches {
                            return false;
                        }
                    }
                }
            }
            true
        }

        (WastRetCore::RefNull(heap_ty), Value::Ref(Ref::Null(ref_ty))) => match (heap_ty, ref_ty) {
            (None, _) => true,

            (Some(HeapType::Func), wasm::HeapType::Func | wasm::HeapType::NoFunc) => true,

            (Some(HeapType::NoFunc), wasm::HeapType::NoFunc) => true,

            (Some(HeapType::Extern), wasm::HeapType::Extern | wasm::HeapType::NoExtern) => true,

            (Some(HeapType::NoExtern), wasm::HeapType::NoExtern) => true,

            (Some(HeapType::Exn), wasm::HeapType::Exn) => true,

            (Some(HeapType::Any), _) => true,

            (Some(HeapType::None), wasm::HeapType::None) => true,

            (_, _) => todo!("test_val(expected={:?}, found={:?})", expected, found),
        },

        (WastRetCore::RefFunc(Some(Index::Num(idx, _span))), Value::Ref(Ref::Func(addr))) => {
            *addr == rt.get_module_fun_addr(module_addr, *idx)
        }

        (WastRetCore::RefFunc(None), Value::Ref(Ref::Func(_))) => true,

        (WastRetCore::RefExtern(None), Value::Ref(Ref::Extern(_))) => true,

        (WastRetCore::RefExtern(Some(extern1)), Value::Ref(Ref::Extern(extern2))) => {
            ExternAddr(*extern1) == *extern2
        }

        (WastRetCore::RefStruct, Value::Ref(Ref::Struct(_))) => true,

        (WastRetCore::RefI31, Value::Ref(Ref::I31(_))) => true,

        (_, _) => false,
    }
}

fn is_f32_canonical_nan(f: f32) -> bool {
    let payload_bits = 23;
    let payload_mask = (1 << payload_bits) - 1;
    let canonical_nan_payload = 1 << (payload_bits - 1);
    let f_payload = f.to_bits() & payload_mask;
    f.is_nan() && f_payload == canonical_nan_payload
}

fn is_f32_arithmetic_nan(f: f32) -> bool {
    // Positive or negative nan
    f.is_nan()
}

fn is_f64_canonical_nan(f: f64) -> bool {
    let payload_bits = 52;
    let payload_mask = (1 << payload_bits) - 1;
    let canonical_nan_payload = 1 << (payload_bits - 1);
    let f_payload = f.to_bits() & payload_mask;
    f.is_nan() && f_payload == canonical_nan_payload
}

fn is_f64_arithmetic_nan(f: f64) -> bool {
    // Positive or negative nan
    f.is_nan()
}

/// Convert a wasmrun trap to the error message for that trap used in Wasm test suite
fn trap_expected_msg(trap: Trap) -> &'static str {
    match trap {
        Trap::UndefinedElement => "undefined",
        Trap::UninitializedElement => "uninitialized",
        Trap::IndirectCallTypeMismatch => "indirect call",
        Trap::ElementOOB => "element out of bounds",
        Trap::OOBMemoryAccess => "out of bounds memory access",
        Trap::OOBTableAccess => "out of bounds table access",
        Trap::IntDivideByZero => "integer divide by zero",
        Trap::IntOverflow => "integer overflow",
        Trap::InvalidConvToInt => "invalid conversion to integer",
        Trap::Unreachable => "unreachable",
        Trap::CallIndirectOnExternRef => "TODO", // TODO
        Trap::NullFunction => "null function",
        Trap::NullReference => "null reference",
        Trap::NullStructReference => "null structure reference",
        Trap::NullArrayReference => "null array reference",
        Trap::NullI31Reference => "null i31 reference",
    }
}

fn directive_line_number(directive: &WastDirective, file_contents: &str) -> usize {
    directive_span(directive).linecol_in(file_contents).0
}

fn directive_span(directive: &WastDirective) -> Span {
    match directive {
        WastDirective::Wat(module) => match module {
            QuoteWat::Wat(Wat::Module(m)) => m.span,
            QuoteWat::Wat(Wat::Component(c)) => c.span,
            QuoteWat::QuoteModule(span, _) => *span,
            QuoteWat::QuoteComponent(span, _) => *span,
        },
        WastDirective::AssertMalformed { span, .. } => *span,
        WastDirective::AssertInvalid { span, .. } => *span,
        WastDirective::Register { span, .. } => *span,
        WastDirective::Invoke(i) => i.span,
        WastDirective::AssertTrap { span, .. } => *span,
        WastDirective::AssertReturn { span, .. } => *span,
        WastDirective::AssertExhaustion { span, .. } => *span,
        WastDirective::AssertUnlinkable { span, .. } => *span,
        WastDirective::AssertException { span, .. } => *span,
        WastDirective::Thread(wast::WastThread { span, .. }) => *span,
        WastDirective::Wait { span, .. } => *span,
    }
}
