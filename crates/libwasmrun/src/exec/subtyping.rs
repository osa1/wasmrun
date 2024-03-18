use libwasmrun_syntax::{CompType, HeapType, ValueType};

#[allow(unused)]
pub(super) fn is_subtype_of(
    sub_ty: &ValueType,
    super_ty: &ValueType,
    sub_ty_module_tys: &[CompType],
    super_ty_module_tys: &[CompType],
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
        sub_ty_module_tys,
        super_ty_module_tys,
    )
}

fn is_heap_subtype_of(
    sub_ty: &HeapType,
    super_ty: &HeapType,
    sub_ty_module_tys: &[CompType],
    _super_ty_module_tys: &[CompType],
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
            HeapType::TypeIdx(sub_ty_idx) => {
                !matches!(sub_ty_module_tys[*sub_ty_idx as usize], CompType::Func(_))
            }
            _ => false,
        },

        HeapType::Func => match sub_ty {
            HeapType::Func => true,
            HeapType::TypeIdx(sub_ty_idx) => {
                matches!(sub_ty_module_tys[*sub_ty_idx as usize], CompType::Func(_))
            }
            _ => false,
        },

        HeapType::Struct => match sub_ty {
            HeapType::Func => true,
            HeapType::TypeIdx(sub_ty_idx) => {
                matches!(sub_ty_module_tys[*sub_ty_idx as usize], CompType::Struct(_))
            }
            _ => false,
        },

        HeapType::Array => match sub_ty {
            HeapType::Func => true,
            HeapType::TypeIdx(sub_ty_idx) => {
                matches!(sub_ty_module_tys[*sub_ty_idx as usize], CompType::Array(_))
            }
            _ => false,
        },

        HeapType::TypeIdx(_super_ty_idx) => todo!(),
    }
}
