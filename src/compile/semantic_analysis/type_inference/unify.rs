use super::super::super::types::types::*;
use super::type_env::{TypeEnv, TypeSubstitute};
use super::occurs_check::occurs_check;

impl TypeSubstitute {
    //二つの型の型代入情報の末端を取得して単一化する
    pub fn start_unify(self, ty_env: TypeEnv, ty1: Type, ty2: Type) -> Result<(Self, Type, TypeEnv), String> {
        if ty1 == ty2 {
            return Ok((self, ty1, ty_env));
        }
        match (&ty1, &ty2) {
            (Type::TyVar(ref ty_id1, _), Type::TyVar(ref ty_id2, _)) => {
                match (self.0.get(ty_id1).map(Clone::clone), self.0.get(ty_id2).map(Clone::clone)) {
                    (Some(ty1), Some(ty2)) => self.start_unify(ty_env, ty1.clone(), ty2.clone()),
                    (Some(ty1), None) => self.start_unify(ty_env, ty1.clone(), ty2.clone()),
                    (None, Some(ty2)) => self.start_unify(ty_env, ty1.clone(), ty2.clone()),
                    (None, None) => {
                        let (mut ty_sub, ty, ty_env) = self.unify(ty_env, ty1.clone(), ty2.clone())?;
                        ty_sub.safe_insert(ty_id1.clone(), ty.clone());
                        ty_sub.safe_insert(ty_id2.clone(), ty.clone());
                        Ok((ty_sub, ty, ty_env))
                    }
                }
            }

            (Type::TyVar(ref ty_id1, _), _) => {
                match self.0.get(ty_id1).map(Clone::clone) {
                    Some(ty1) => self.start_unify(ty_env, ty1.clone(), ty2.clone()),
                    None => {
                        let (mut ty_sub, ty, ty_env) = self.unify(ty_env, ty1.clone(), ty2.clone())?;
                        ty_sub.safe_insert(ty_id1.clone(), ty.clone());
                        Ok((ty_sub, ty, ty_env))
                    }
                }
            }

            (_, Type::TyVar(ref ty_id2, _)) => {
                match self.0.get(ty_id2).map(Clone::clone) {
                    Some(ty2) => self.start_unify(ty_env, ty1.clone(), ty2.clone()),
                    None => {
                        let (mut ty_sub, ty, ty_env) = self.unify(ty_env, ty1.clone(), ty2.clone())?;
                        ty_sub.safe_insert(ty_id2.clone(), ty.clone());
                        Ok((ty_sub, ty, ty_env))
                    }
                }
            }
            (ty1, ty2) => self.unify(ty_env, ty1.clone(), ty2.clone())
        }
    }

    //型変数に型を代入する
    fn safe_insert(&mut self, ty_id: TypeId, insert_ty: Type) {
        if occurs_check(&self.0, &insert_ty, &ty_id) == false {
            println!("{:?}=>{:?}", ty_id, insert_ty);
            self.0.insert(ty_id, insert_ty);
        } else {
            println!("occurs! {:?}=>{:?}", ty_id, insert_ty);
        }
    }

    //単一化処理
    fn unify(self, ty_env: TypeEnv, ty1: Type, ty2: Type) -> Result<(Self, Type, TypeEnv), String> {
        match (ty1, ty2) {
            (Type::TyVar(id, conds), ty) => self.ty_var_unify(ty_env, id, conds, ty),
            (ty, Type::TyVar(id, conds)) => self.ty_var_unify(ty_env, id, conds, ty),
            (Type::LambdaType(ty1), Type::LambdaType(ty2)) => self.lambda_unify(ty_env, *ty1, *ty2),
            (Type::TupleType(ty1), Type::TupleType(ty2)) => self.tuple_unify(ty_env, *ty1, *ty2),
            (ty1, ty2) => {
                if ty1 == ty2 {
                    return Ok((self, ty1, ty_env));
                }
                Err(format!(
                    "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                    ty1, ty2
                ))
            }
        }
    }

    //型変数の単一化処理
    fn ty_var_unify(mut self, mut ty_env: TypeEnv, ty_id: TypeId, cond: TypeCondition, ty: Type) -> Result<(Self, Type, TypeEnv), String> {
        let result_ty =
            match ty {
                Type::TyVar(id, mut cond2) => {
                    match (cond.call, cond2.call) {
                        (Some(fn_ty1), Some(fn_ty2)) => {
                            let (ty_sub, new_fn_ty, new_ty_env) = self.fn_unify(ty_env, *fn_ty1, *fn_ty2)?;
                            self = ty_sub;
                            ty_env = new_ty_env;
                            Type::TyVar(ty_env.no_name_get(), TypeCondition { call: Some(Box::new(new_fn_ty)) })
                        }
                        (fn_ty1, None) => {
                            Type::TyVar(ty_id, TypeCondition { call: fn_ty1 })
                        }
                        (None, fn_ty2) => {
                            Type::TyVar(id, TypeCondition { call: fn_ty2 })
                        }
                    }
                }
                Type::LambdaType(x) => {
                    match cond.call {
                        Some(c) => {
                            let env_len = x.env_ty.clone().map(|t| t.element_tys.len()).unwrap_or(0);
                            let mut func_ty = x.func_ty.clone();
                            func_ty.param_types = func_ty.param_types.clone().into_iter().skip(env_len).collect::<Vec<_>>();
                            let (ty_sub, _, new_ty_env) = self.fn_unify(ty_env, func_ty, *c)?;
                            self = ty_sub;
                            ty_env = new_ty_env;
                        }
                        _ => ()
                    };
                    Type::LambdaType(x.clone())
                }
                ty => {
                    if cond.call.is_some() {
                        return Err(format!(
                            "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                            Type::TyVar(ty_id, cond), ty
                        ));
                    } else {
                        ty
                    }
                }
            };
        Ok((self, result_ty, ty_env))
    }

    //関数の単一化処理
    fn fn_unify(mut self, mut ty_env: TypeEnv, ty1: FuncType, ty2: FuncType) -> Result<(Self, FuncType, TypeEnv), String> {
        if ty1.param_types.len() != ty2.param_types.len() {
            return Err(format!(
                "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                ty1, ty2
            ));
        }
        let mut new_param_types: Vec<Type> = Vec::with_capacity(ty1.param_types.len());
        for (x, y) in ty1.param_types.into_iter().zip(ty2.param_types) {
            let (ty_sub, ty, new_ty_env) = self.start_unify(ty_env, x, y)?;
            self = ty_sub;
            ty_env = new_ty_env;
            new_param_types.push(ty);
        }
        let (ty_sub, new_ret_type, ty_env) = self.start_unify(ty_env, ty1.ret_type, ty2.ret_type)?;
        Ok((ty_sub, FuncType { param_types: new_param_types, ret_type: new_ret_type }, ty_env))
    }

    //タプルの単一化処理
    fn tuple_unify(mut self, mut ty_env: TypeEnv, ty1: TupleType, ty2: TupleType) -> Result<(Self, Type, TypeEnv), String> {
        if ty1.element_tys.len() != ty2.element_tys.len() {
            return Err(format!(
                "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                ty1, ty2
            ));
        }
        for (x, y) in ty1.element_tys.clone().into_iter().zip(ty2.element_tys) {
            let (ty_sub, _, new_ty_env) = self.start_unify(ty_env, x, y)?;
            self = ty_sub;
            ty_env = new_ty_env;
        }
        Ok((self, Type::TupleType(Box::new(ty1)), ty_env))
    }

    //ラムダの単一化処理
    fn lambda_unify(mut self, mut ty_env: TypeEnv, ty1: LambdaType, ty2: LambdaType) -> Result<(Self, Type, TypeEnv), String> {
        match (ty1.env_ty.clone(), ty2.env_ty) {
            (Some(x), Some(y)) => {
                let (ty_sub, _, new_ty_env) = self.tuple_unify(ty_env, x, y)?;
                self = ty_sub;
                ty_env = new_ty_env;
            }
            (None, None) => (),
            (None, x) | (x, None) => {
                return Err(format!(
                    "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                    "void", x
                ));
            }
        }
        let (ty_sub, _, ty_env) = self.fn_unify(ty_env, ty1.func_ty.clone(), ty2.func_ty)?;
        Ok((ty_sub, Type::LambdaType(Box::new(ty1)), ty_env))
    }
}