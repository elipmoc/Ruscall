use std::collections::HashSet;
use super::super::types::*;

//型に関する操作をまとめた型クラス
pub trait Types {
    //型が持っている型変数一覧を列挙する
    fn tv_list(&self) -> HashSet<TypeId>;
}

impl<'a, T: Types> Types for &'a T {
    fn tv_list(&self) -> HashSet<TypeId> {
        (*self).tv_list()
    }
}

impl<Item: Types> Types for Vec<Item> {
    fn tv_list(&self) -> HashSet<TypeId> {
        self.iter().fold(HashSet::new(), |mut acc, ty| {
            acc.extend(ty.tv_list());
            acc
        })
    }
}

impl Types for Type {
    fn tv_list(&self) -> HashSet<TypeId> {
        use self::Type::*;
        match self {
            TCon { .. } | TGen(_) => HashSet::new(),
            TyVar(ty_id) => {
                let mut tv = HashSet::new();
                tv.insert(ty_id.clone());
                tv
            }
            TupleType(tuple_ty) => tuple_ty.element_tys.tv_list(),
            StructType(struct_ty) => {
                match &struct_ty.ty {
                    StructInternalType::TupleType(tuple_ty) => tuple_ty.element_tys.tv_list(),
                    StructInternalType::RecordType(record_ty) => record_ty.element_tys.iter().map(|(_, ty)| ty).collect::<Vec<_>>().tv_list()
                }
            }
            LambdaType(lambda_ty) => {
                let mut tv = match &lambda_ty.env_ty {
                    Some(tuple_ty) => tuple_ty.element_tys.tv_list(),
                    None => HashSet::new()
                };
                tv.extend(lambda_ty.func_ty.param_types.tv_list());
                tv.extend(lambda_ty.func_ty.ret_type.tv_list());
                tv
            }
        }
    }
}