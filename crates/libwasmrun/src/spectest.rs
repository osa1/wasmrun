// https://github.com/WebAssembly/spec/blob/7526564b56c30250b66504fe795e9c1e88a938af/interpreter/host/spectest.ml

use crate::exec::Runtime;

// [] -> []
pub(crate) fn print(_rt: &mut Runtime) {}

// [i32] -> []
pub(crate) fn print_i32(_rt: &mut Runtime) {}

// [i32, f32] -> []
pub(crate) fn print_i32_f32(_rt: &mut Runtime) {}

// [f64, f64] -> []
pub(crate) fn print_f64_f64(_rt: &mut Runtime) {}

// [f32] -> []
pub(crate) fn print_f32(_rt: &mut Runtime) {}

// [f64] -> []
pub(crate) fn print_f64(_rt: &mut Runtime) {}
