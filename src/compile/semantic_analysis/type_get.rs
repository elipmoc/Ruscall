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
        let ty_info = ty_info.unify(
            Type::TyVar(func_ty_id.clone(), TypeCondition::new()),
            Type::LambdaType(Box::new(LambdaType { env_ty: None, func_ty: self.ty.clone() })),
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, Type::TyVar(func_ty_id, TypeCondition::new())))
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
                Type::TyVar(ty_id, TypeCondition::new())
            }).collect();
        let (mut ty_info, ret_ty) = (&self.body).ty_get(ty_info)?;
        let func_ty = Type::TyVar(ty_info.global_get(self.name.clone()), TypeCondition::new());
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
            ExprMir::IfMir(x) => x.ty_get(ty_info),
            ExprMir::CallMir(x) => x.ty_get(ty_info),
            ExprMir::OpMir(x) => x.ty_get(ty_info),
            ExprMir::VariableMir(x) => x.ty_get(ty_info),
            ExprMir::GlobalVariableMir(x) => x.ty_get(ty_info),
            ExprMir::TupleMir(x) => x.ty_get(ty_info),
            ExprMir::TupleStructMir(x) => x.ty_get(ty_info),
            ExprMir::TuplePropertyMir(x) => x.ty_get(ty_info),
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

impl TypeGet for IfMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, cond_ty) = (&self.cond).ty_get(ty_info)?;
        let (ty_info, t_expr_ty) = (&self.t_expr).ty_get(ty_info)?;
        let (ty_info, f_expr_ty) = (&self.f_expr).ty_get(ty_info)?;
        let ty_info = ty_info.unify(cond_ty, Type::Bool)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let ty_info = ty_info.unify(t_expr_ty, f_expr_ty.clone())
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let ret_ty = Type::TyVar(self.ty_id.clone(), TypeCondition::new());
        let ty_info = ty_info.unify(ret_ty.clone(), f_expr_ty)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, ret_ty))
    }
}

impl TypeGet for CallMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, params_ty) =
            ty_get_all(self.params.iter(), ty_info)?;
        let ret_ty_id = self.ty_id.clone();
        let ret_ty = Type::TyVar(ret_ty_id, TypeCondition::new());
        let (mut ty_info, func_ty) = (&self.func).ty_get(ty_info)?;
        let ty_id = ty_info.no_name_get();
        let ty_info =
            ty_info.unify(
                func_ty,
                Type::TyVar(ty_id, TypeCondition::with_call(FuncType { param_types: params_ty, ret_type: ret_ty.clone() })),
            ).map_err(|msg| Error::new(self.func.get_pos(), &msg))?;
        Ok((ty_info, ret_ty))
    }
}

impl TypeGet for OpMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let ret_ty = match &(self.op) as &str {
            "==" => Type::Bool,
            _ => Type::Int32,
        };

        let (ty_info, l_expr_ty) = (&self.l_expr)
            .ty_get(ty_info)?;
        let ty_info = ty_info.unify(l_expr_ty, Type::Int32)
            .map_err(|msg| Error::new(self.l_expr.get_pos(), &msg))?;

        let (ty_info, r_expr_ty) = (&self.r_expr)
            .ty_get(ty_info)?;
        let ty_info = ty_info.unify(r_expr_ty, Type::Int32)
            .map_err(|msg| Error::new(self.r_expr.get_pos(), &msg))?;
        Ok((ty_info, ret_ty))
    }
}

impl<'a> TypeGet for &'a VariableMir {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let ty_id = ty_info.get(self.id.to_string());
        let ty_info = ty_info.unify(Type::TyVar(ty_id, TypeCondition::new()), Type::TyVar(self.ty_id.clone(), TypeCondition::new()))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, Type::TyVar(self.ty_id.clone(), TypeCondition::new())))
    }
}

impl TypeGet for GlobalVariableMir {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let ty_var_id = ty_info.global_get(self.id.clone());
        Ok((ty_info, Type::TyVar(ty_var_id, TypeCondition::new())))
    }
}

impl TypeGet for TupleMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, elements_ty) =
            ty_get_all(self.elements.iter(), ty_info)?;
        let tuple_ty = Type::TupleType(Box::new(TupleType { element_tys: elements_ty }));
        let ty_info = ty_info.unify(tuple_ty.clone(), Type::TyVar(self.ty_id.clone(), TypeCondition::new()))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, tuple_ty))
    }
}

impl TypeGet for TupleStructMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, tuple_ty) = self.tuple.ty_get(ty_info)?;
        let internal_ty = match self.ty.ty {
            StructInternalType::RecordType(ref x) => Type::TupleType(Box::new(TupleType {
                element_tys: x.element_tys.iter()
                    .map(|(_, x)| x.clone())
                    .collect()
            })),
            StructInternalType::TupleType(ref x) => Type::TupleType(Box::new(x.clone()))
        };
        let ty_info = ty_info.unify(tuple_ty.clone(), internal_ty)
            .map_err(|msg| Error::new(self.tuple.pos, &msg))?;
        Ok((ty_info, Type::StructType(Box::new(self.ty.clone()))))
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
                    Type::TyVar(ty_id, TypeCondition::new())
                }).collect();
        let func_ty = Type::TyVar(ty_info.global_get(self.func_name.clone()), TypeCondition::new());
        ty_info.out_nest();
        let ret_id = ty_info.no_name_get();
        let ty_info =
            envs_ty.iter().zip(&params_ty)
                .fold(Ok(ty_info),
                      |acc, (x, y)| acc?.unify(x.clone(), y.clone()))
                .map_err(|msg| Error::new(self.pos, &msg))?;

        let func_ty2 = FuncType { param_types: params_ty, ret_type: Type::TyVar(ret_id, TypeCondition::new()) };

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
        let ty_info = ty_info.unify(lambda_ty.clone(), Type::TyVar(self.ty_id.clone(), TypeCondition::new()))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, lambda_ty))
    }
}


impl TypeGet for TuplePropertyMir {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (mut ty_info, expr_ty) = (&self.expr).ty_get(ty_info)?;
        let property_ty =
            Type::TyVar(
                self.ty_id.clone(), TypeCondition::new(),
            );
        let temp_var_ty = ty_info.no_name_get();
        let ty_info = ty_info.unify(
            Type::TyVar(temp_var_ty, TypeCondition::with_impl_tuple_property(self.index, property_ty.clone())),
            expr_ty,
        ).map_err(|msg| Error::new(self.pos, &msg))?;

        Ok((ty_info, property_ty))
    }
}