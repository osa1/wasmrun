use std::fmt::Display;
use std::fs::read_to_string;
use std::num::ParseIntError;
use std::str::FromStr;

use libwasmrun::Value;

use serde::Deserialize;

#[derive(Debug)]
pub struct TestSpec {
    pub source_filename: String,
    pub commands: Vec<Command>,
}

#[derive(Debug)]
pub enum Command {
    Module {
        line: usize,
        name: Option<String>,
        filename: String,
    },

    AssertReturn {
        line: usize,
        kind: ActionKind,
        module: Option<String>,
        func: String,
        args: Vec<Value>,
        expected: Vec<Value>,
        /// Expected error message. Only available when kind is `ActionKind::Trap`.
        err_msg: Option<String>,
    },

    AssertUninstantiable {
        line: usize,
        filename: String,
        text: String,
    },

    Register {
        line: usize,
        name: Option<String>,
        register_as: String,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ActionKind {
    /// Call a function
    Invoke,
    /// Call a function, expect it to trap
    Trap,
    /// Get a global
    GetGlobal,
}

pub fn parse_test_spec(file: &str) -> Result<TestSpec, Vec<usize>> {
    let mut failing_lines: Vec<usize> = vec![];

    let file_contents = read_to_string(file).unwrap();
    let TestSpecDe {
        source_filename,
        commands,
    } = serde_json::from_str(&file_contents).unwrap();

    let mut commands_ = vec![];
    'command_loop: for command_de in commands {
        match command_de.typ.as_ref() {
            "module" => commands_.push(Command::Module {
                line: command_de.line,
                name: command_de.name,
                filename: command_de.filename.unwrap(),
            }),
            "assert_return" | "assert_trap" => {
                let action = command_de.action.unwrap();
                let action_kind = match action.typ.as_str() {
                    "invoke" => {
                        if command_de.typ == "assert_return" {
                            ActionKind::Invoke
                        } else {
                            assert_eq!(command_de.typ, "assert_trap");
                            ActionKind::Trap
                        }
                    }
                    "get" => ActionKind::GetGlobal,
                    other => panic!("Unknown action type: {}", other),
                };
                let mut args = Vec::with_capacity(action.args.len());
                for value in action.args.into_iter() {
                    match parse_value(value) {
                        Ok(value) => {
                            args.push(value);
                        }
                        Err(err) => {
                            println!("{}", err);
                            failing_lines.push(command_de.line);
                            continue 'command_loop;
                        }
                    }
                }
                commands_.push(Command::AssertReturn {
                    line: command_de.line,
                    kind: action_kind,
                    module: action.module,
                    func: action.field,
                    args,
                    expected: if action_kind != ActionKind::Trap {
                        let expected = command_de.expected.unwrap();
                        let mut values = Vec::with_capacity(expected.len());
                        for value in expected {
                            match parse_value(value) {
                                Ok(value) => values.push(value),
                                Err(err) => {
                                    println!("{}", err);
                                    failing_lines.push(command_de.line);
                                    continue 'command_loop;
                                }
                            }
                        }
                        values
                    } else {
                        vec![]
                    },
                    err_msg: command_de.text,
                });
            }
            "assert_uninstantiable" => {
                commands_.push(Command::AssertUninstantiable {
                    line: command_de.line,
                    filename: command_de.filename.unwrap(),
                    text: command_de.text.unwrap(),
                });
            }
            "action" => {
                let action = command_de.action.unwrap();
                if action.typ != "invoke" {
                    todo!("Unknown action type: {}", action.typ);
                }
                // Basically assert_return, we the function doesn't return anything
                let mut args = Vec::with_capacity(action.args.len());
                for arg in action.args {
                    match parse_value(arg) {
                        Ok(value) => args.push(value),
                        Err(err) => {
                            println!("{}", err);
                            failing_lines.push(command_de.line);
                            continue 'command_loop;
                        }
                    }
                }
                commands_.push(Command::AssertReturn {
                    line: command_de.line,
                    kind: ActionKind::Invoke,
                    module: action.module,
                    func: action.field,
                    args,
                    expected: vec![],
                    err_msg: command_de.text,
                });
            }
            "register" => {
                commands_.push(Command::Register {
                    line: command_de.line,
                    name: command_de.name,
                    register_as: command_de.as_.unwrap(),
                });
            }
            "assert_exhaustion" | "assert_unlinkable" => {
                // TODO We probably want to test this
            }
            "assert_invalid" | "assert_malformed" => {
                // We don't want to test this stuff, skip
            }
            other => {
                println!("Unknown command type: {}", other);
                failing_lines.push(command_de.line);
            }
        }
    }

    Ok(TestSpec {
        source_filename,
        commands: commands_,
    })
}

fn parse_value(value_de: ValueDe) -> Result<Value, String> {
    let str = match &value_de.value {
        Some(str) => str,
        None => panic!("{:?}", value_de),
    };
    Ok(match value_de.typ.as_ref() {
        "i32" => Value::I32(parse_str::<ParseIntError, u32>(str) as i32),
        "i64" => Value::I64(parse_str::<ParseIntError, u64>(str) as i64),
        "f32" => {
            // f32::NAN = 0b0_11111111_10000000000000000000000
            // If I'm reading the spec right, nan:canonical and nan:arithmetic can be the same
            if str == "nan:canonical" {
                Value::F32(f32::NAN)
            } else if str == "nan:arithmetic" {
                Value::F32(f32::NAN)
            } else {
                let i_32 = parse_str::<ParseIntError, u32>(str) as i32;
                let f_32: f32 = unsafe { ::std::mem::transmute(i_32) };
                Value::F32(f_32)
            }
        }
        "f64" => {
            if str == "nan:canonical" {
                Value::F64(f64::NAN)
            } else if str == "nan:arithmetic" {
                Value::F64(f64::NAN)
            } else {
                let i_64 = parse_str::<ParseIntError, u64>(str) as i64;
                let f_64: f64 = unsafe { ::std::mem::transmute(i_64) };
                Value::F64(f_64)
            }
        }
        other => return Err(format!("Unknown value type: {}", other)),
    })
}

fn parse_str<E: Display, A: FromStr<Err = E>>(s: &str) -> A {
    match s.parse() {
        Ok(val) => val,
        Err(err) => panic!("Error while parsing {:?}: {}", s, err),
    }
}

#[derive(Debug, Deserialize)]
struct TestSpecDe {
    source_filename: String,
    commands: Vec<CommandDe>,
}

#[derive(Debug, Deserialize)]
struct CommandDe {
    #[serde(rename = "type")]
    typ: String,
    line: usize,
    filename: Option<String>,
    action: Option<ActionDe>,
    expected: Option<Vec<ValueDe>>,
    name: Option<String>,
    #[serde(rename = "as")]
    as_: Option<String>,
    text: Option<String>, // error message in assert_trap
}

#[derive(Debug, Deserialize)]
struct ActionDe {
    #[serde(rename = "type")]
    typ: String,
    module: Option<String>,
    field: String,
    #[serde(default)]
    args: Vec<ValueDe>,
}

#[derive(Debug, Deserialize)]
struct ValueDe {
    #[serde(rename = "type")]
    typ: String,
    value: Option<String>,
}
