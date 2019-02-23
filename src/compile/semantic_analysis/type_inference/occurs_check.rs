use super::super::super::types::types::*;
use std::collections::HashMap;

type TypeSubstituteHashMap = HashMap<TypeId, Type>;

//TypeにTypeIdが出現するか検査
pub fn occurs_check(hash_map: &TypeSubstituteHashMap, ty: &Type, ty_id: &TypeId) -> bool {
    match ty {
        Type::TyVar(id) => {
            if id == ty_id { true } else {
                if hash_map.contains_key(&id) {
                    occurs_check(hash_map, &hash_map[id], &ty_id)
                } else {
                    false
                }
            }
        }
        Type::TCon { .. } | Type::TGen(_, _) => false,
        Type::TupleType(x) => x.occurs_check(hash_map, ty_id),
        Type::TApp(x) => x.occurs_check(hash_map, ty_id),
        Type::StructType(x) => {
            match x.ty {
                StructInternalType::TupleType(ref x) => x.occurs_check(hash_map, ty_id),
                StructInternalType::RecordType(ref x) => x.element_tys.iter().any(|(_, e)| occurs_check(hash_map, e, ty_id))
            }
        }
    }
}

impl TupleType {
    fn occurs_check(&self, hash_map: &TypeSubstituteHashMap, ty_id: &TypeId) -> bool {
        self.element_tys.iter().any(|e| occurs_check(hash_map, e, ty_id))
    }
}

impl TApp {
    fn occurs_check(&self, hash_map: &TypeSubstituteHashMap, ty_id: &TypeId) -> bool {
        occurs_check(hash_map, &self.0, ty_id) ||
            occurs_check(hash_map, &self.1, ty_id)
    }
}