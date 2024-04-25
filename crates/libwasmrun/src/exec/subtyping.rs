use crate::module::{Module, TypeIdx};

use libwasmrun_syntax::{CompType, HeapType};

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
