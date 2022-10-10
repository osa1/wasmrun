macro_rules! exec_panic {
    ($($arg:tt)*) => {{
        return Err(ExecError::Panic(format!($($arg)*)))
    }};
}
