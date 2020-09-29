use std::fmt::Display;
use std::fs::read_to_string;
use std::num::ParseIntError;
use std::str::FromStr;

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
    },

    Register {
        line: usize,
        name: Option<String>,
        register_as: String,
    },
}

#[derive(Debug)]
pub enum ActionKind {
    /// Call a function
    Invoke,
    /// Get a global
    GetGlobal,
}

#[derive(Debug)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

pub fn parse_test_spec(file: &str) -> TestSpec {
    let file_contents = read_to_string(file).unwrap();
    let TestSpecDe {
        source_filename,
        commands,
    } = serde_json::from_str(&file_contents).unwrap();

    let mut commands_ = vec![];
    for command_de in commands {
        match command_de.typ.as_ref() {
            "module" => commands_.push(Command::Module {
                line: command_de.line,
                name: command_de.name,
                filename: command_de.filename.unwrap(),
            }),
            "assert_return" => {
                let action = command_de.action.unwrap();
                let action_kind = match action.typ.as_str() {
                    "invoke" => ActionKind::Invoke,
                    "get" => ActionKind::GetGlobal,
                    other => panic!("Unknown action type: {}", other),
                };
                commands_.push(Command::AssertReturn {
                    line: command_de.line,
                    kind: action_kind,
                    module: action.module,
                    func: action.field,
                    args: action.args.into_iter().map(parse_value).collect(),
                    expected: command_de
                        .expected
                        .unwrap()
                        .into_iter()
                        .map(parse_value)
                        .collect(),
                });
            }
            "action" => {
                let action = command_de.action.unwrap();
                if action.typ != "invoke" {
                    todo!("Unknown action type: {}", action.typ);
                }
                // Basically assert_return, we the function doesn't return anything
                commands_.push(Command::AssertReturn {
                    line: command_de.line,
                    kind: ActionKind::Invoke,
                    module: action.module,
                    func: action.field,
                    args: action.args.into_iter().map(parse_value).collect(),
                    expected: vec![],
                });
            }
            "register" => {
                commands_.push(Command::Register {
                    line: command_de.line,
                    name: command_de.name,
                    register_as: command_de.as_.unwrap(),
                });
            }
            "assert_trap" | "assert_exhaustion" => {
                // TODO We probably want to test these
            }
            "assert_invalid"
            | "assert_malformed"
            | "assert_unlinkable"
            | "assert_uninstantiable" => {
                // We don't want to test this stuff, skip
            }
            other => todo!("Unknown command type: {}", other),
        }
    }

    TestSpec {
        source_filename,
        commands: commands_,
    }
}

fn parse_value(value_de: ValueDe) -> Value {
    let str = &value_de.value.unwrap();
    match value_de.typ.as_ref() {
        "i32" => Value::I32(parse_str::<ParseIntError, u32>(str) as i32),
        "i64" => Value::I64(parse_str::<ParseIntError, u64>(str) as i64),
        "f32" => {
            if str == "nan:canonical" {
                Value::F32(f32::NAN)
            } else if str == "nan:arithmetic" {
                Value::F32(f32::NAN) // FIXME
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
                Value::F64(f64::NAN) // FIXME
            } else {
                let i_64 = parse_str::<ParseIntError, u64>(str) as i64;
                let f_64: f64 = unsafe { ::std::mem::transmute(i_64) };
                Value::F64(f_64)
            }
        }
        other => todo!("Unknown value type: {}", other),
    }
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
