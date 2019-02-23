use crate::compile::types::*;
use super::type_substitute::TypeSubstitute;

impl TypeSubstitute {
    // 型変数に対応する単相型を見つけて返す。見つからなかったら空タプルの型を返す
    pub fn look_up(&self, ty_id: &TypeId, inst_flag: bool) -> Type {
        match self.ty_sub.get(ty_id) {
            Some(ty) => self.type_look_up(ty, inst_flag),
            None => {
                Type::TyVar(ty_id.clone())
            }
        }
    }

    pub fn type_look_up(&self, ty: &Type, inst_flag: bool) -> Type {
        match ty {
            Type::TyVar(ty_id) => self.look_up(ty_id, inst_flag),
            Type::TupleType(x) => Type::TupleType(Box::new(self.tuple_look_up(x, inst_flag))),
            Type::TApp(x) => Type::TApp(Box::new(self.func_look_up(x, inst_flag))),
            Type::StructType(x) => self.struct_look_up(x, inst_flag),
            Type::TCon { .. } => ty.clone(),
            Type::TGen(_, ty_id) => if inst_flag { self.look_up(&ty_id, true) } else { ty.clone() },
        }
    }
    pub fn func_look_up(&self, ty: &TApp, inst_flag: bool) -> TApp {
        TApp(self.type_look_up(&ty.0, inst_flag), self.type_look_up(&ty.1, inst_flag))
    }

    fn tuple_look_up(&self, ty: &TupleType, inst_flag: bool) -> TupleType {
        TupleType {
            element_tys:
            ty.element_tys
                .iter()
                .map(|ty| self.type_look_up(ty, inst_flag))
                .collect(),
        }
    }

    fn record_look_up(&self, ty: &RecordType, inst_flag: bool) -> RecordType {
        RecordType {
            element_tys:
            ty.element_tys
                .iter()
                .map(|(name, ty)| (name.clone(), self.type_look_up(ty, inst_flag)))
                .collect(),
        }
    }

    fn struct_look_up(&self, ty: &StructType, inst_flag: bool) -> Type {
        let mut ty = ty.clone();
        ty.ty =
            match ty.ty {
                StructInternalType::TupleType(x) => StructInternalType::TupleType(self.tuple_look_up(&x, inst_flag)),
                StructInternalType::RecordType(x) => StructInternalType::RecordType(self.record_look_up(&x, inst_flag)),
            };
        Type::StructType(Box::new(ty))
    }
}