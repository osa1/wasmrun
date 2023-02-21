macro_rules! exec_panic {
    ($($arg:tt)*) => {{
        return Err(crate::ExecError::Panic(format!($($arg)*)))
    }};
}
