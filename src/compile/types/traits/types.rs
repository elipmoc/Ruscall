use std::collections::{HashSet, HashMap};
use super::super::*;
use super::super::super::semantic_analysis::type_inference::type_substitute::TypeSubstitute;

//型に関する操作をまとめた型クラス
pub trait Types {
    //型が持っている型変数一覧を列挙する
    fn tv_list(&self) -> HashSet<TypeId>;
    //型代入を行う
    fn apply(self, &TypeSubstitute, inst_flag: bool) -> Self;
}

impl<Item: Types> Types for Vec<Item> {
    fn tv_list(&self) -> HashSet<TypeId> {
        self.iter().fold(HashSet::new(), |mut acc, ty| {
            acc.extend(ty.tv_list());
            acc
        })
    }
    fn apply(self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        self.into_iter().map(|ty| { ty.apply(ty_sub, inst_flag) }).collect()
    }
}

impl<Item: Types> Types for Vec<(String, Item)> {
    fn tv_list(&self) -> HashSet<TypeId> {
        self.iter().fold(HashSet::new(), |mut acc, (_, ty)| {
            acc.extend(ty.tv_list());
            acc
        })
    }
    fn apply(self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        self.into_iter().map(|(str, ty)| { (str, ty.apply(ty_sub, inst_flag)) }).collect()
    }
}

use std::{cmp::Eq, hash::Hash};

impl<K: Eq + Hash, Item: Types> Types for HashMap<K, Item> {
    fn tv_list(&self) -> HashSet<TypeId> {
        self.iter().fold(HashSet::new(), |mut acc, (_, ty)| {
            acc.extend(ty.tv_list());
            acc
        })
    }
    fn apply(self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        self.into_iter().map(|(k, ty)| { (k, ty.apply(ty_sub, inst_flag)) }).collect()
    }
}

impl Types for Type {
    fn tv_list(&self) -> HashSet<TypeId> {
        use self::Type::*;
        match self {
            TCon { .. } | TGen(_, _) => HashSet::new(),
            TyVar(ty_id) => {
                let mut tv = HashSet::new();
                tv.insert(ty_id.clone());
                tv
            }
            TupleType(tuple_ty) => tuple_ty.element_tys.tv_list(),
            StructType(struct_ty) => {
                match &struct_ty.ty {
                    StructInternalType::TupleType(tuple_ty) => tuple_ty.element_tys.tv_list(),
                    StructInternalType::RecordType(record_ty) => record_ty.element_tys.tv_list()
                }
            }
            LambdaType(lambda_ty) => {
                let mut tv = match &lambda_ty.env_ty {
                    Some(tuple_ty) => tuple_ty.element_tys.tv_list(),
                    None => HashSet::new()
                };
                tv.extend(lambda_ty.func_ty.tv_list());
                tv
            }
        }
    }

    fn apply(self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        ty_sub.type_look_up(&self, inst_flag)
    }
}

impl Types for FuncType {
    fn tv_list(&self) -> HashSet<TypeId> {
        let mut tv = self.param_types.tv_list();
        tv.extend(self.ret_type.tv_list());
        tv
    }
    fn apply(self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        ty_sub.func_look_up(&self, inst_flag)
    }
}

impl<T: Types> Types for Qual<T> {
    fn tv_list(&self) -> HashSet<TypeId> {
        let mut tv = self.t.tv_list();
        tv.extend(self.ps.tv_list());
        tv
    }
    fn apply(mut self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        self.t = self.t.apply(ty_sub, inst_flag);
        self.ps = self.ps.apply(ty_sub, inst_flag);
        self
    }
}

impl Types for Preds {
    fn tv_list(&self) -> HashSet<TypeId> {
        self.0.tv_list()
    }
    fn apply(self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        Preds(self.0.into_iter().map(|(ty, p)| {
            (ty.apply(ty_sub, inst_flag), p.apply(ty_sub, inst_flag))
        }).collect())
    }
}

impl Types for Pred {
    fn tv_list(&self) -> HashSet<TypeId> {
        let mut tv = self.ty.tv_list();
        tv.extend(self.cond.tv_list());
        tv
    }
    fn apply(mut self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        self.ty = self.ty.apply(ty_sub, inst_flag);
        self.cond = self.cond.apply(ty_sub, inst_flag);
        self
    }
}


impl Types for Condition {
    fn tv_list(&self) -> HashSet<TypeId> {
        use self::Condition::*;
        match self {
            Call(func_ty) => func_ty.tv_list(),
            Items(impls) => impls.tv_list(),
            Empty => HashSet::new()
        }
    }
    fn apply(self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        use self::Condition::*;
        match self {
            Call(func_ty) => Call(Box::new(func_ty.apply(ty_sub, inst_flag))),
            Items(impls) => Items(Box::new(impls.apply(ty_sub, inst_flag))),
            e @ Empty => e
        }
    }
}

impl Types for ImplItems {
    fn tv_list(&self) -> HashSet<TypeId> {
        let mut tv = self.name_properties.tv_list();
        tv.extend(self.index_properties.tv_list());
        tv
    }
    fn apply(mut self, ty_sub: &TypeSubstitute, inst_flag: bool) -> Self {
        self.name_properties = self.name_properties.apply(ty_sub, inst_flag);
        self.index_properties = self.index_properties.apply(ty_sub, inst_flag);
        self
    }
}
