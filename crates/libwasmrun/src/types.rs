use crate::collections::Map;
use crate::module::Module;

use libwasmrun_syntax as wasm;

#[derive(Debug, Default)]
pub(crate) struct TypeCanonicalizer {
    /// Maps groups to the canonical id of the first type in the group.
    canonical_groups: Map<CanonicalGroup, u32>,
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub(crate) struct CanonicalTypeIdx(u32);

#[derive(Debug, PartialEq, Eq, Hash)]
pub(crate) struct CanonicalGroup {
    types: Vec<wasm::SubType>,
}

impl TypeCanonicalizer {
    #[allow(unused)]
    pub(crate) fn add_recursive_group(
        &mut self,
        module: &mut Module,
        ty: &wasm::RecType,
        recursive_group_start: u32,
    ) -> u32 {
        // NB. By design, same types in different order generates different groups.
        let group_types: Vec<wasm::SubType> = ty
            .tys
            .iter()
            .map(|ty| canonicalize_sub_type(module, ty, recursive_group_start))
            .collect();

        let new_group_idx = self.canonical_groups.len() as u32;

        *self
            .canonical_groups
            .entry(CanonicalGroup { types: group_types })
            .or_insert(new_group_idx)
    }
}

fn canonicalize_sub_type(
    module: &Module,
    ty: &wasm::SubType,
    recursive_group_start: u32,
) -> wasm::SubType {
    let wasm::SubType {
        final_,
        supers,
        comp_ty,
    } = ty;

    let canonical_super_tys: Vec<u32> = supers
        .iter()
        .map(|super_idx| module.isorecursive_canonical_type_ids[*super_idx as usize])
        .collect();

    let comp_ty = match comp_ty {
        wasm::CompType::Func(wasm::FunctionType { params, results }) => {
            wasm::CompType::Func(wasm::FunctionType {
                params: params
                    .iter()
                    .map(|param| canonicalize_value_type(module, param, recursive_group_start))
                    .collect(),
                results: results
                    .iter()
                    .map(|result| canonicalize_value_type(module, result, recursive_group_start))
                    .collect(),
            })
        }

        wasm::CompType::Struct(wasm::StructType { fields }) => {
            wasm::CompType::Struct(wasm::StructType {
                fields: fields
                    .iter()
                    .map(|field_ty| {
                        canonicalize_field_type(module, field_ty, recursive_group_start)
                    })
                    .collect(),
            })
        }

        wasm::CompType::Array(wasm::ArrayType { field }) => {
            wasm::CompType::Array(wasm::ArrayType {
                field: canonicalize_field_type(module, field, recursive_group_start),
            })
        }
    };

    wasm::SubType {
        final_: *final_,
        supers: canonical_super_tys,
        comp_ty,
    }
}

fn canonicalize_field_type(
    module: &Module,
    wasm::FieldType {
        mutability,
        storage_ty,
    }: &wasm::FieldType,
    recursive_group_start: u32,
) -> wasm::FieldType {
    wasm::FieldType {
        mutability: *mutability,
        storage_ty: canonicalize_storage_type(module, storage_ty, recursive_group_start),
    }
}

fn canonicalize_storage_type(
    module: &Module,
    ty: &wasm::StorageType,
    recursive_group_start: u32,
) -> wasm::StorageType {
    match ty {
        wasm::StorageType::Val(val_ty) => wasm::StorageType::Val(canonicalize_value_type(
            module,
            val_ty,
            recursive_group_start,
        )),
        wasm::StorageType::Packed(_) => *ty,
    }
}

fn canonicalize_value_type(
    module: &Module,
    ty: &wasm::ValueType,
    recursive_group_start: u32,
) -> wasm::ValueType {
    match ty {
        wasm::ValueType::I32
        | wasm::ValueType::I64
        | wasm::ValueType::F32
        | wasm::ValueType::F64
        | wasm::ValueType::V128 => *ty,

        wasm::ValueType::Reference(wasm::ReferenceType { nullable, heap_ty }) => match heap_ty {
            wasm::HeapType::Any
            | wasm::HeapType::None
            | wasm::HeapType::Exn
            | wasm::HeapType::Extern
            | wasm::HeapType::NoExtern
            | wasm::HeapType::Func
            | wasm::HeapType::NoFunc
            | wasm::HeapType::Eq
            | wasm::HeapType::Struct
            | wasm::HeapType::Array
            | wasm::HeapType::I31 => *ty,

            wasm::HeapType::TypeIdx(idx) => {
                if *idx >= recursive_group_start {
                    wasm::ValueType::Reference(wasm::ReferenceType {
                        nullable: *nullable,
                        heap_ty: wasm::HeapType::TypeIdx(idx - recursive_group_start),
                    })
                } else {
                    wasm::ValueType::Reference(wasm::ReferenceType {
                        nullable: *nullable,
                        heap_ty: wasm::HeapType::TypeIdx(
                            module.isorecursive_canonical_type_ids[*idx as usize],
                        ),
                    })
                }
            }
        },
    }
}
