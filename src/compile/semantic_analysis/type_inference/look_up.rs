use super::super::super::types::types::*;
use super::type_env::TypeSubstitute;

impl TypeSubstitute {
    // 型変数に対応する単相型を見つけて返す。見つからなかったら空タプルの型を返す
    pub fn look_up(&self, ty_id: &TypeId) -> Type {
        match self.ty_sub.get(ty_id) {
            Some(ty) => self.type_look_up(ty),
            None => {
                Type::TyVar(ty_id.clone())
            }
        }
    }

    pub fn type_look_up(&self, ty: &Type) -> Type {
        match ty {
            Type::TyVar(ty_id) => self.look_up(ty_id),
            Type::TupleType(x) => Type::TupleType(Box::new(self.tuple_look_up(x))),
            Type::LambdaType(x) => self.lambda_look_up(x),
            Type::StructType(x) => self.struct_look_up(x),
            Type::TCon { .. } => ty.clone(),
        }
    }
    fn func_look_up(&self, ty: &FuncType) -> FuncType {
        FuncType {
            param_types:
            ty.param_types
                .iter()
                .map(|ty| self.type_look_up(ty))
                .collect(),
            ret_type: self.type_look_up(&ty.ret_type),
        }
    }

    fn tuple_look_up(&self, ty: &TupleType) -> TupleType {
        TupleType {
            element_tys:
            ty.element_tys
                .iter()
                .map(|ty| self.type_look_up(ty))
                .collect(),
        }
    }

    fn lambda_look_up(&self, ty: &LambdaType) -> Type {
        Type::LambdaType(Box::new(LambdaType {
            env_ty: ty.env_ty.clone().map(|x| self.tuple_look_up(&x)),
            func_ty: self.func_look_up(&ty.func_ty),
        }))
    }

    fn record_look_up(&self, ty: &RecordType) -> RecordType {
        RecordType {
            element_tys:
            ty.element_tys
                .iter()
                .map(|(name, ty)| (name.clone(), self.type_look_up(ty)))
                .collect(),
        }
    }

    fn struct_look_up(&self, ty: &StructType) -> Type {
        let mut ty = ty.clone();
        ty.ty =
            match ty.ty {
                StructInternalType::TupleType(x) => StructInternalType::TupleType(self.tuple_look_up(&x)),
                StructInternalType::RecordType(x) => StructInternalType::RecordType(self.record_look_up(&x)),
            };
        Type::StructType(Box::new(ty))
    }
}