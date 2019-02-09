use super::super::*;

//量化された型を実体化できる性質を持つ型クラス
pub trait Instantiate {
    fn inst(self, fresh_types: &Vec<Type>) -> Self;
}

impl<Item: Instantiate> Instantiate for Vec<Item> {
    fn inst(self, fresh_types: &Vec<Type>) -> Vec<Item> {
        self.into_iter().map(|x| x.inst(fresh_types)).collect()
    }
}

impl<Item: Instantiate> Instantiate for Vec<(String, Item)> {
    fn inst(self, fresh_types: &Vec<Type>) -> Vec<(String, Item)> {
        self.into_iter().map(|(s, i)| (s, i.inst(fresh_types))).collect()
    }
}

use std::{cmp::Eq, hash::Hash, collections::HashMap};

impl<K: Eq + Hash, Item: Instantiate> Instantiate for HashMap<K, Item> {
    fn inst(self, fresh_types: &Vec<Type>) -> Self {
        self.into_iter().map(|(k, ty)| { (k, ty.inst(fresh_types)) }).collect()
    }
}

impl Instantiate for Type {
    fn inst(self, fresh_types: &Vec<Type>) -> Type {
        use self::Type::*;
        match self {
            ty @ TCon { .. } => ty,
            TGen(n,_) => fresh_types[n].clone(),
            ty @ TyVar(_) => ty,
            TupleType(tuple_ty) => {
                let mut tuple_ty = *tuple_ty;
                tuple_ty.element_tys = tuple_ty.element_tys.inst(fresh_types);
                TupleType(Box::new(tuple_ty))
            }
            StructType(struct_ty) => {
                let mut struct_ty = *struct_ty;
                struct_ty.ty = match struct_ty.ty {
                    StructInternalType::TupleType(mut tuple_ty) => {
                        tuple_ty.element_tys = tuple_ty.element_tys.inst(fresh_types);
                        StructInternalType::TupleType(tuple_ty)
                    }
                    StructInternalType::RecordType(mut record_ty) => {
                        record_ty.element_tys = record_ty.element_tys.inst(fresh_types);
                        StructInternalType::RecordType(record_ty)
                    }
                };
                StructType(Box::new(struct_ty))
            }
            LambdaType(lambda_ty) => {
                let mut lambda_ty = *lambda_ty;
                lambda_ty.env_ty = match lambda_ty.env_ty {
                    Some(mut tuple_ty) => {
                        tuple_ty.element_tys = tuple_ty.element_tys.inst(fresh_types);
                        Some(tuple_ty)
                    }
                    None => None
                };
                lambda_ty.func_ty.param_types = lambda_ty.func_ty.param_types.inst(fresh_types);
                lambda_ty.func_ty.ret_type = lambda_ty.func_ty.ret_type.inst(fresh_types);
                LambdaType(Box::new(lambda_ty))
            }
        }
    }
}

impl Instantiate for FuncType {
    fn inst(mut self, fresh_types: &Vec<Type>) -> Self {
        self.param_types = self.param_types.inst(fresh_types);
        self.ret_type = self.ret_type.inst(fresh_types);
        self
    }
}

impl<T: Instantiate> Instantiate for Qual<T> {
    fn inst(mut self, fresh_types: &Vec<Type>) -> Self {
        self.t = self.t.inst(fresh_types);
        self.ps = self.ps.inst(fresh_types);
        self
    }
}

impl Instantiate for Preds {
    fn inst(self, fresh_types: &Vec<Type>) -> Self {
        Preds(self.0.into_iter().map(|(ty, p)| (ty.inst(fresh_types), p.inst(fresh_types))).collect())
    }
}

impl Instantiate for Pred {
    fn inst(mut self, fresh_types: &Vec<Type>) -> Self {
        self.ty = self.ty.inst(fresh_types);
        self.cond = self.cond.inst(fresh_types);
        self
    }
}

impl Instantiate for Condition {
    fn inst(self, fresh_types: &Vec<Type>) -> Self {
        use self::Condition::*;
        match self {
            Call(func_ty) => Call(Box::new(func_ty.inst(fresh_types))),
            Items(items) => Items(Box::new(items.inst(fresh_types))),
            c @ Empty => c
        }
    }
}

impl Instantiate for ImplItems {
    fn inst(mut self, fresh_types: &Vec<Type>) -> Self {
        self.name_properties = self.name_properties.inst(fresh_types);
        self.index_properties = self.index_properties.inst(fresh_types);
        self
    }
}