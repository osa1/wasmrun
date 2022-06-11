use crate::exec::{ExecError, Result};
use crate::store::ModuleAddr;
use crate::Runtime;

use libwasmrun_syntax::SimdInstruction;

pub fn exec_simd_instr(
    _rt: &mut Runtime,
    _module_addr: ModuleAddr,
    instr: SimdInstruction,
) -> Result<()> {
    Err(ExecError::Panic(format!(
        "SIMD instruction not implemented: {:?}",
        instr
    )))
}
