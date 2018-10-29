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
    pub fn ty_get(&self) -> TyCheckResult<TypeInfo> {
        let ty_info = TypeInfo::new();
        //外部関数宣言の型チェック
        let ty_info =
            ty_get_all(
                self.ex_dec_func_list.iter().map(|x| x.1), ty_info,
            )?.0;
        //関数宣言の型チェック
        let ty_info =
            ty_get_all(self.dec_func_list.iter(), ty_info)?.0;
        //関数定義の型チェック
        let ty_info =
            ty_get_all(self.func_list.iter().map(|x| x.1), ty_info)?.0;

        Ok(ty_info)
    }
}

use super::super::ast::DecFuncAST;

impl<'a> TypeGet for &'a DecFuncAST {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_id, ty_info) = ty_info.get(self.name.clone());

        let ty_info = ty_info.unify(
            Type::TyVar(ty_id.clone()),
            Type::FuncType(Box::new(self.ty.clone())),
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, Type::TyVar(ty_id)))
    }
}

impl<'a> TypeGet for &'a FuncIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, params_ty): (_, Vec<Type>)
        = (0..self.params_len)
            .fold((ty_info, vec![]), move |mut acc, id| {
                let id = self.params_len - id - 1;
                let (ty_id, ty_info) = acc.0.get(id.to_string());
                acc.1.push(Type::TyVar(ty_id));
                acc.0 = ty_info;
                acc
            });
        let (ty_info, ret_ty) = (&self.body).ty_get(ty_info)?;
        let (func_ty_id, ty_info) = ty_info.global_get(self.name.clone());
        let ty_info = ty_info.unify(
            Type::TyVar(func_ty_id.clone()),
            Type::create_func_type(
                params_ty,
                ret_ty,
            ),
        ).map_err(|msg| Error::new(self.pos, &msg))?;

        let ty_info =
            (0..self.params_len)
                .fold(ty_info, |acc, x| acc.remove(&x.to_string()));
        Ok((ty_info, Type::TyVar(func_ty_id)))
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
        let (ty_info, params_ty) =
            ty_get_all(self.params.iter(), ty_info)?;
        let (ret_ty_id, ty_info) = ty_info.no_name_get();
        let ret_ty = Type::TyVar(ret_ty_id);
        let (ty_info, func_ty) = (&self.func).ty_get(ty_info)?;
        let ty_info =
            ty_info.unify(func_ty, Type::create_func_type(params_ty, ret_ty.clone()))
                .map_err(|msg| Error::new(SourcePosition::new(), &msg))?;
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
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_var_id, ty_info) = ty_info.get(self.id.to_string());
        Ok((ty_info, Type::TyVar(ty_var_id)))
    }
}

impl TypeGet for GlobalVariableIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_var_id, ty_info) = ty_info.get(self.id.clone());
        Ok((ty_info, Type::TyVar(ty_var_id)))
    }
}

impl TypeGet for TupleIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (ty_info, elements_ty) =
            ty_get_all(self.elements.iter(), ty_info)?;
        let ret_ty = Type::TupleType(Box::new(TupleType { element_tys: elements_ty }));
        Ok((ty_info, ret_ty))
    }
}

impl TypeGet for LambdaIr {
    fn ty_get(&self, ty_info: TypeInfo) -> TyCheckResult<(TypeInfo, Type)> {
        let (mut ty_info, envs_ty) =
            ty_get_all(self.env.iter(), ty_info)?;
        ty_info.in_nest();
        let (ty_info, mut params_ty) =
            (0..self.func.params_len)
                .fold((ty_info, vec![]), move |mut acc, id| {
                    let id = self.func.params_len - id - 1;
                    let (ty_id, ty_info) = acc.0.get((id).to_string());
                    acc.1.push(Type::TyVar(ty_id));
                    acc.0 = ty_info;
                    acc
                });
        let (ty_info, func_ty) = (&self.func).ty_get(ty_info)?;
        let mut ty_info =
            (0..self.func.params_len)
                .fold(ty_info, |acc, x| acc.remove(&(x).to_string()));
        ty_info.out_nest();
        let (ret_id, ty_info) = ty_info.no_name_get();
        let ty_info =
            envs_ty.iter().zip(&params_ty)
                .fold(Ok(ty_info),
                      |acc, (x, y)| acc?.unify(x.clone(), y.clone()))
                .map_err(|msg| Error::new(self.func.pos, &msg))?;

        let func_ty2 = FuncType { param_types: params_ty, ret_type: Type::TyVar(ret_id) };

        let lambda_ty = Type::create_lambda_type(envs_ty, func_ty2.clone());
        let (func_id, ty_info) = ty_info.global_get(self.func.name.clone());
        let ty_info = ty_info.unify(func_ty, Type::FuncType(Box::new(func_ty2)))
            .map_err(|msg| Error::new(self.func.pos, &msg))?;
        Ok((ty_info, lambda_ty))
    }
}