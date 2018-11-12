use super::super::error::Error;
use super::super::types::types::*;
use super::type_env::*;
use super::mir::*;

type TyCheckResult<T> = Result<T, Error>;

trait TypeGet {
    fn ty_get(&self, TypeInfo) -> TyCheckResult<(TypeInfo, Type)>;
}

fn ty_get_all<Item: TypeGet, T: Iterator<Item=Item>>(iter: T, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Vec<Type>)>
{
    let (ty_info, ty_list) =
        iter.fold(Ok((ty_info, vec![])), |acc, x| {
            let (ty_info, mut ty_list) = acc?;
            let (ty_info, ty) = x.ty_get(ty_info)?;
            ty_list.push(ty);
            Ok((ty_info, ty_list))
        })?;
    Ok((ty_info, ty_list))
}

impl ProgramMir {
    pub fn ty_get(mut self) -> TyCheckResult<ProgramMir> {
        //外部関数宣言の型チェック
        let ty_info =
            ty_get_all(
                self.ex_dec_func_list.iter(), self.ty_info,
            )?.0;
        //関数宣言の型チェック
        let ty_info =
            ty_get_all(self.dec_func_list.iter(), ty_info)?.0;
        //関数定義の型チェック
        let ty_info =
            ty_get_all(self.func_list.iter().map(|x| x), ty_info)?.0;
        self.ty_info = ty_info;
        Ok(self)
    }
}

impl<'a> TypeGet for &'a DecFuncMir {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let func_ty_id = ty_info.get(self.name.clone());

        let ty_var = ty_info.no_name_get();
        let ty_info = ty_info.unify(
            Type::TyVar(func_ty_id.clone(), vec![]),
            Type::TyVar(ty_var, vec![TypeCondition::Call(self.ty.clone())]),
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, Type::TyVar(func_ty_id, vec![])))
    }
}

impl<'a> TypeGet for &'a FuncMir {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        ty_info.in_nest();
        let params_ty: Vec<Type>
        = (0..self.params_len)
            .map(|id| {
                let id = self.params_len - id - 1;
                let ty_id = ty_info.get(id.to_string());
                Type::TyVar(ty_id, vec![])
            }).collect();
        let (mut ty_info, ret_ty) = (&self.body).ty_get(ty_info)?;
        let func_ty = Type::TyVar(ty_info.global_get(self.name.clone()), vec![]);
        let mut ty_info = ty_info.unify(
            func_ty.clone(),
            Type::create_func_type(
                params_ty,
                ret_ty,
            ),
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        ty_info.out_nest();
        Ok((ty_info, func_ty))
    }
}


impl<'a> TypeGet for &'a ExprMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        match self {
            ExprMir::NumMir(x) => x.ty_get(ty_info),
            ExprMir::BoolMir(x) => x.ty_get(ty_info),
            ExprMir::CallMir(x) => x.ty_get(ty_info),
            ExprMir::OpMir(x) => x.ty_get(ty_info),
            ExprMir::VariableMir(x) => x.ty_get(ty_info),
            ExprMir::GlobalVariableMir(x) => x.ty_get(ty_info),
            ExprMir::TupleMir(x) => x.ty_get(ty_info),
            ExprMir::LambdaMir(x) => x.ty_get(ty_info)
        }
    }
}

impl TypeGet for NumMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        Ok((ty_info, Type::Int32))
    }
}

impl TypeGet for BoolMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        Ok((ty_info, Type::Bool))
    }
}

impl TypeGet for CallMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, params_ty) =
            ty_get_all(self.params.iter(), ty_info)?;
        let ret_ty_id = self.ty_id.clone();
        let ret_ty = Type::TyVar(ret_ty_id, vec![]);
        let (mut ty_info, func_ty) = (&self.func).ty_get(ty_info)?;
        let ty_id = ty_info.no_name_get();
        let ty_info =
            ty_info.unify(
                func_ty,
                Type::TyVar(ty_id, vec![TypeCondition::Call(FuncType { param_types: params_ty, ret_type: ret_ty.clone() })]),
            ).map_err(|msg| Error::new(self.func.get_pos(), &msg))?;
        Ok((ty_info, ret_ty))
    }
}

impl TypeGet for OpMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, l_expr_ty) = (&self.l_expr)
            .ty_get(ty_info)?;
        let ty_info = ty_info.unify(l_expr_ty, Type::Int32)
            .map_err(|msg| Error::new(self.l_expr.get_pos(), &msg))?;

        let (ty_info, r_expr_ty) = (&self.r_expr)
            .ty_get(ty_info)?;
        let ty_info = ty_info.unify(r_expr_ty, Type::Int32)
            .map_err(|msg| Error::new(self.r_expr.get_pos(), &msg))?;
        Ok((ty_info, Type::Int32))
    }
}

impl<'a> TypeGet for &'a VariableMir {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let ty_id = ty_info.get(self.id.to_string());
        let ty_info = ty_info.unify(Type::TyVar(ty_id, vec![]), Type::TyVar(self.ty_id.clone(), vec![]))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, Type::TyVar(self.ty_id.clone(), vec![])))
    }
}

impl TypeGet for GlobalVariableMir {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let ty_var_id = ty_info.global_get(self.id.clone());
        Ok((ty_info, Type::TyVar(ty_var_id, vec![])))
    }
}

impl TypeGet for TupleMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, elements_ty) =
            ty_get_all(self.elements.iter(), ty_info)?;
        let tuple_ty = Type::TupleType(Box::new(TupleType { element_tys: elements_ty }));
        let ty_info = ty_info.unify(tuple_ty.clone(), Type::TyVar(self.ty_id.clone(), vec![]))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, tuple_ty))
    }
}

impl TypeGet for LambdaMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (mut ty_info, envs_ty) =
            ty_get_all(self.env.iter(), ty_info)?;
        ty_info.in_nest();
        let params_ty: Vec<Type> =
            (0..self.params_len)
                .map(|id| {
                    let id = self.params_len - id - 1;
                    let ty_id = ty_info.get((id).to_string());
                    Type::TyVar(ty_id, vec![])
                }).collect();
        let func_ty = Type::TyVar(ty_info.global_get(self.func_name.clone()), vec![]);
        ty_info.out_nest();
        let ret_id = ty_info.no_name_get();
        let ty_info =
            envs_ty.iter().zip(&params_ty)
                .fold(Ok(ty_info),
                      |acc, (x, y)| acc?.unify(x.clone(), y.clone()))
                .map_err(|msg| Error::new(self.pos, &msg))?;

        let func_ty2 = FuncType { param_types: params_ty, ret_type: Type::TyVar(ret_id, vec![]) };

        let lambda_ty = if envs_ty.len() == 0 {
            Type::LambdaType(Box::new(LambdaType {
                func_ty: func_ty2.clone(),
                env_ty: None,
            }))
        } else {
            Type::create_lambda_type(envs_ty, func_ty2.clone())
        };
        let ty_info = ty_info.unify(func_ty, Type::create_func_type(func_ty2.param_types, func_ty2.ret_type))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let ty_info = ty_info.unify(lambda_ty.clone(), Type::TyVar(self.ty_id.clone(), vec![]))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, lambda_ty))
    }
}