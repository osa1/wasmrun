use crate::module::{Module, TypeIdx};

use libwasmrun_syntax::{CompType, FunctionType, HeapType, ValueType};

pub(super) fn is_value_subtype_of(
    sub_ty: &ValueType,
    super_ty: &ValueType,
    sub_ty_module: &Module,
    super_ty_module: &Module,
) -> bool {
    let sub_ref_ty = match sub_ty {
        ValueType::I32 | ValueType::I64 | ValueType::F32 | ValueType::F64 | ValueType::V128 => {
            return sub_ty == super_ty;
        }
        ValueType::Reference(ref_ty) => ref_ty,
    };

    let super_ref_ty = match super_ty {
        ValueType::I32 | ValueType::I64 | ValueType::F32 | ValueType::F64 | ValueType::V128 => {
            return false
        }
        ValueType::Reference(ref_ty) => ref_ty,
    };

    if sub_ref_ty.nullable && !super_ref_ty.nullable {
        return false;
    }

    is_heap_subtype_of(
        &sub_ref_ty.heap_ty,
        &super_ref_ty.heap_ty,
        sub_ty_module,
        super_ty_module,
    )
}

pub(crate) fn is_heap_subtype_of(
    sub_ty: &HeapType,
    super_ty: &HeapType,
    sub_ty_module: &Module,
    super_ty_module: &Module,
) -> bool {
    match super_ty {
        // Bottom types are only subtypes of the bottom types of same kind.
        HeapType::None
        | HeapType::NoExtern
        | HeapType::NoFunc
        | HeapType::Extern
        | HeapType::Exn
        | HeapType::I31 => sub_ty == super_ty,

        // `Any` is a supertype of all internal types.
        HeapType::Any => matches!(
            sub_ty,
            HeapType::Any
                | HeapType::Exn
                | HeapType::Func
                | HeapType::Eq
                | HeapType::Struct
                | HeapType::Array
                | HeapType::I31
                | HeapType::TypeIdx(_), // struct, array, func
        ),

        // Structs, arrays, and i31s are `eq`, functions are not.
        // TODO: What about `Exn`?
        // TODO: Shouldn't externs be `eq` as well?
        HeapType::Eq => match sub_ty {
            HeapType::Array | HeapType::Struct | HeapType::I31 => true,
            HeapType::TypeIdx(sub_ty_idx) => !matches!(
                &sub_ty_module.get_type(TypeIdx(*sub_ty_idx)).comp_ty,
                CompType::Func(_)
            ),
            _ => false,
        },

        HeapType::Func => match sub_ty {
            HeapType::Func => true,
            HeapType::TypeIdx(sub_ty_idx) => {
                matches!(
                    &sub_ty_module.get_type(TypeIdx(*sub_ty_idx)).comp_ty,
                    CompType::Func(_)
                )
            }
            _ => false,
        },

        HeapType::Struct => match sub_ty {
            HeapType::Func => true,
            HeapType::TypeIdx(sub_ty_idx) => {
                matches!(
                    &sub_ty_module.get_type(TypeIdx(*sub_ty_idx)).comp_ty,
                    CompType::Struct(_)
                )
            }
            _ => false,
        },

        HeapType::Array => match sub_ty {
            HeapType::Func => true,
            HeapType::TypeIdx(sub_ty_idx) => {
                matches!(
                    &sub_ty_module.get_type(TypeIdx(*sub_ty_idx)).comp_ty,
                    CompType::Array(_)
                )
            }
            _ => false,
        },

        HeapType::TypeIdx(super_ty_idx) => {
            // Super type is a struct, array, or function type.
            match sub_ty {
                HeapType::Any => false,

                HeapType::None => true,

                HeapType::Exn => false,

                HeapType::Extern => false,

                HeapType::NoExtern => false,

                HeapType::Func => false,

                HeapType::NoFunc => matches!(
                    &super_ty_module.get_type(TypeIdx(*super_ty_idx)).comp_ty,
                    CompType::Func(_)
                ),

                HeapType::Eq => false,

                HeapType::Struct => matches!(
                    &super_ty_module.get_type(TypeIdx(*super_ty_idx)).comp_ty,
                    CompType::Struct(_)
                ),

                HeapType::Array => matches!(
                    &super_ty_module.get_type(TypeIdx(*super_ty_idx)).comp_ty,
                    CompType::Array(_)
                ),

                HeapType::I31 => false,

                HeapType::TypeIdx(mut sub_ty_idx) => {
                    let super_canonical_ty_idx =
                        super_ty_module.canonical_type_ids[*super_ty_idx as usize];

                    let mut sub_canonical_ty_idx =
                        sub_ty_module.canonical_type_ids[sub_ty_idx as usize];

                    if super_canonical_ty_idx == sub_canonical_ty_idx {
                        return true;
                    }

                    if let Some(sub_func_ty) = sub_ty_module
                        .get_type(TypeIdx(sub_ty_idx))
                        .as_function_type()
                    {
                        let super_func_ty = match super_ty_module
                            .get_type(TypeIdx(*super_ty_idx))
                            .as_function_type()
                        {
                            Some(func_ty) => func_ty,
                            None => return false,
                        };

                        return is_function_subtype_of(
                            sub_func_ty,
                            super_func_ty,
                            sub_ty_module,
                            super_ty_module,
                        );
                    }

                    let mut sub_ty_supers = &sub_ty_module.get_type(TypeIdx(sub_ty_idx)).supers;
                    debug_assert!(sub_ty_supers.len() <= 1);

                    while !sub_ty_supers.is_empty() {
                        sub_ty_idx = sub_ty_supers[0];
                        sub_ty_supers = &sub_ty_module.get_type(TypeIdx(sub_ty_idx)).supers;

                        sub_canonical_ty_idx =
                            sub_ty_module.canonical_type_ids[sub_ty_idx as usize];

                        if super_canonical_ty_idx == sub_canonical_ty_idx {
                            return true;
                        }
                    }

                    false
                }
            }
        }
    }
}

pub(crate) fn is_function_subtype_of(
    sub_ty: &FunctionType,
    super_ty: &FunctionType,
    sub_ty_module: &Module,
    super_ty_module: &Module,
) -> bool {
    if sub_ty.params.len() != super_ty.params.len()
        || sub_ty.results().len() != super_ty.results().len()
    {
        return false;
    }

    for (sub_ty_arg, super_ty_arg) in sub_ty.params().iter().zip(super_ty.params().iter()) {
        if !is_value_subtype_of(super_ty_arg, sub_ty_arg, super_ty_module, sub_ty_module) {
            return false;
        }
    }

    for (sub_ty_ret, super_ty_ret) in sub_ty.results().iter().zip(super_ty.results().iter()) {
        if !is_value_subtype_of(sub_ty_ret, super_ty_ret, sub_ty_module, super_ty_module) {
            return false;
        }
    }

    true
}
