use super::super::error::Error;
use super::super::types::*;
use super::type_env::*;
use super::ir::*;

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

impl ProgramIr {
    pub fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<TypeInfo> {
        //外部関数宣言の型チェック
        let ty_info =
            ty_get_all(
                self.ex_dec_func_list.iter(), ty_info,
            )?.0;
        //関数宣言の型チェック
        let ty_info =
            ty_get_all(self.dec_func_list.iter(), ty_info)?.0;
        //関数定義の型チェック
        let ty_info =
            ty_get_all(self.func_list.iter().map(|x| x), ty_info)?.0;

        Ok(ty_info)
    }
}

use super::super::ast::DecFuncAST;

impl<'a> TypeGet for &'a DecFuncAST {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let func_ty_id = ty_info.get(self.name.clone());

        let ty_info = ty_info.unify(
            Type::TyVar(func_ty_id.clone(), vec![]),
            Type::create_func_type(self.ty.param_types.clone(), self.ty.ret_type.clone() ),
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, Type::TyVar(func_ty_id, vec![])))
    }
}

impl<'a> TypeGet for &'a FuncIr {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let params_ty: Vec<Type>
        = (0..self.params_len)
            .map(|id| {
                let id = self.params_len - id - 1;
                let ty_id = ty_info.get(id.to_string());
                Type::TyVar(ty_id, vec![])
            }).collect();
        let (mut ty_info, ret_ty) = (&self.body).ty_get(ty_info)?;
        let func_ty =Type::TyVar( ty_info.global_get(self.name.clone()),vec![]);
        let mut ty_info = ty_info.unify(
            func_ty.clone(),
            Type::create_func_type(
                params_ty,
                ret_ty,
            ),
        ).map_err(|msg| Error::new(self.pos, &msg))?;

        (0..self.params_len)
            .for_each(|x| ty_info.remove(&x.to_string()));
        Ok((ty_info, func_ty))
    }
}


impl<'a> TypeGet for &'a ExprIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        match self {
            ExprIr::NumIr(x) => x.ty_get(ty_info),
            ExprIr::CallIr(x) => x.ty_get(ty_info),
            ExprIr::OpIr(x) => x.ty_get(ty_info),
            ExprIr::VariableIr(x) => x.ty_get(ty_info),
            ExprIr::GlobalVariableIr(x) => x.ty_get(ty_info),
            ExprIr::TupleIr(x) => x.ty_get(ty_info),
            ExprIr::LambdaIr(x) => x.ty_get(ty_info)
        }
    }
}

impl TypeGet for NumIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        Ok((ty_info, Type::Int32))
    }
}

impl TypeGet for CallIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        use combine::stream::state::SourcePosition;
        let (mut ty_info, params_ty) =
            ty_get_all(self.params.iter(), ty_info)?;
        let ret_ty_id = self.ty_id.clone();
        let ret_ty = Type::TyVar(ret_ty_id, vec![]);
        let (mut ty_info, func_ty) = (&self.func).ty_get(ty_info)?;
        let ty_id = ty_info.no_name_get();
        let ty_info =
            ty_info.unify(
                func_ty,
                Type::TyVar(ty_id, vec![TypeCondition::Call(FuncType { param_types: params_ty, ret_type: ret_ty.clone() })]),
            ).map_err(|msg| Error::new(SourcePosition::new(), &msg))?;
        Ok((ty_info, ret_ty))
    }
}

impl TypeGet for OpIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        use combine::stream::state::SourcePosition;
        let (ty_info, l_expr_ty) = (&self.l_expr)
            .ty_get(ty_info)?;
        let ty_info = ty_info.unify(l_expr_ty, Type::Int32)
            .map_err(|msg| Error::new(SourcePosition::new(), &msg))?;

        let (ty_info, r_expr_ty) = (&self.r_expr)
            .ty_get(ty_info)?;
        let ty_info = ty_info.unify(r_expr_ty, Type::Int32)
            .map_err(|msg| Error::new(SourcePosition::new(), &msg))?;
        Ok((ty_info, Type::Int32))
    }
}

impl<'a> TypeGet for &'a VariableIr {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let ty_id = ty_info.get(self.id.to_string());
        let ty_info = ty_info.unify(Type::TyVar(ty_id, vec![]), Type::TyVar(self.ty_id.clone(), vec![]))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, Type::TyVar(self.ty_id.clone(), vec![])))
    }
}

impl TypeGet for GlobalVariableIr {
    fn ty_get(&self, mut ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let ty_var_id = ty_info.global_get(self.id.clone());
        Ok((ty_info, Type::TyVar(ty_var_id, vec![])))
    }
}

impl TypeGet for TupleIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, elements_ty) =
            ty_get_all(self.elements.iter(), ty_info)?;
        let tuple_ty = Type::TupleType(Box::new(TupleType { element_tys: elements_ty }));
        let ty_info = ty_info.unify(tuple_ty.clone(), Type::TyVar(self.ty_id.clone(), vec![]))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, tuple_ty))
    }
}

impl TypeGet for LambdaIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (mut ty_info, envs_ty) =
            ty_get_all(self.env.iter(), ty_info)?;
        ty_info.in_nest();
        let mut params_ty: Vec<Type> =
            (0..self.params_len)
                .map(|id| {
                    let id = self.params_len - id - 1;
                    let ty_id = ty_info.get((id).to_string());
                    Type::TyVar(ty_id, vec![])
                }).collect();
        let func_ty = Type::TyVar(ty_info.global_get(self.func_name.clone()), vec![]);
        (0..self.params_len).for_each(|x| ty_info.remove(&(x).to_string()));
        ty_info.out_nest();
        let ret_id = ty_info.no_name_get();
        let mut ty_info =
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