use super::super::error::Error;
use super::super::types::*;
use super::type_env::*;
use super::ir::*;

type TyCheckResult<T> = Result<T, Error>;

trait FuncTypeGet {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute)>;
}

fn func_ty_get<Item: FuncTypeGet, T: Iterator<Item=Item>>(iter: T, mut ty_info: (TypeEnv, TypeSubstitute)) -> TyCheckResult<(TypeEnv, TypeSubstitute)>
{
    for x in iter {
        ty_info = x.ty_get(ty_info.0, ty_info.1)?;
    }
    Ok(ty_info)
}

impl ProgramIr {
    pub fn ty_get(&self) -> TyCheckResult<(TypeEnv, TypeSubstitute)> {
        let ty_info = (TypeEnv::new(), TypeSubstitute::new());
        //外部関数宣言の型チェック
        let ty_info =
            func_ty_get(
                self.ex_dec_func_list.iter().map(|x| x.1), ty_info,
            )?;
        //関数宣言の型チェック
        let ty_info =
            func_ty_get(self.dec_func_list.iter(), ty_info)?;
        //関数定義の型チェック
        let ty_info =
            func_ty_get(self.func_list.iter().map(|x| x.1), ty_info)?;

        Ok(ty_info)
    }
}

use super::super::ast::DecFuncAST;

impl<'a> FuncTypeGet for &'a DecFuncAST {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute)> {
        let (ty_id, ty_env) = ty_env.get(self.name.clone());

        let ty_subst = ty_subst.unify(
            Type::TyVar(ty_id),
            Type::Fn(Box::new(self.ty.clone())),
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_env, ty_subst))
    }
}

impl<'a> FuncTypeGet for &'a FuncIr {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute)> {
        let (ty_env, params_ty): (_, Vec<Type>)
        = (0..self.pamrams_len)
            .fold((ty_env, vec![]), move |mut acc, id| {
                let id = self.pamrams_len - id - 1;
                let (ty_id, ty_env) = acc.0.get(id.to_string());
                acc.0 = ty_env;
                acc.1.push(Type::TyVar(ty_id));
                acc
            });
        let (ty_env, ty_subst, ret_ty) = self.body.ty_get(ty_env, ty_subst)?;
        let (func_ty_id, ty_env) = ty_env.get(self.name.clone());
        let ty_subst = ty_subst.unify(
            Type::TyVar(func_ty_id),
            Type::create_fn_func_type(
                params_ty,
                ret_ty,
            ),
        ).map_err(|msg| Error::new(self.pos, &msg))?;

        let ty_env =
            (0..self.pamrams_len)
                .fold(ty_env, |acc, x| {
                    acc.remove(&x.to_string())
                });
        Ok((ty_env, ty_subst))
    }
}


impl ExprIr {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute, Type)> {
        match self {
            ExprIr::NumIr(x) => x.ty_get(ty_env, ty_subst),
            ExprIr::CallIr(x) => x.ty_get(ty_env, ty_subst),
            ExprIr::OpIr(x) => x.ty_get(ty_env, ty_subst),
            ExprIr::VariableIr(x) => x.ty_get(ty_env, ty_subst),
            ExprIr::GlobalVariableIr(x) => x.ty_get(ty_env, ty_subst),
            ExprIr::TupleIr(x) => x.ty_get(ty_env, ty_subst)
        }
    }
}

impl NumIr {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute, Type)> {
        Ok((ty_env, ty_subst, Type::Int32))
    }
}

impl CallIr {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute, Type)> {
        use combine::stream::state::SourcePosition;
        let (ty_env, ty_subst, params_ty): (TypeEnv, TypeSubstitute, Vec<Type>) =
            self.params
                .iter()
                .fold(Ok((ty_env, ty_subst, Vec::<Type>::new())), |acc, x| {
                    let (ty_env, ty_subst, mut params) = acc?;
                    let (ty_env, ty_subst, param) = x.ty_get(ty_env, ty_subst)?;
                    params.push(param);
                    Ok((ty_env, ty_subst, params))
                })?;
        let (ret_ty_id, ty_env) = ty_env.no_name_get();
        let ret_ty = Type::TyVar(ret_ty_id);
        let (ty_env, ty_subst, func_ty) = self.func.ty_get(ty_env, ty_subst)?;
        let ty_subst =
            ty_subst.unify(func_ty, Type::create_fn_func_type(params_ty, ret_ty.clone()))
                .map_err(|msg| Error::new(SourcePosition::new(), &msg))?;
        Ok((ty_env, ty_subst, ret_ty))
    }
}

impl OpIr {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute, Type)> {
        use combine::stream::state::SourcePosition;
        let (ty_env, ty_subst, l_expr_ty) = self.l_expr
            .ty_get(ty_env, ty_subst)?;
        let ty_subst = ty_subst.unify(l_expr_ty, Type::Int32)
            .map_err(|msg| Error::new(SourcePosition::new(), &msg))?;

        let (ty_env, ty_subst, r_expr_ty) = self.r_expr
            .ty_get(ty_env, ty_subst)?;
        let ty_subst = ty_subst.unify(r_expr_ty, Type::Int32)
            .map_err(|msg| Error::new(SourcePosition::new(), &msg))?;
        Ok((ty_env, ty_subst, Type::Int32))
    }
}

impl VariableIr {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute, Type)> {
        let (ty_var_id, ty_env) = ty_env.get(self.id.to_string());
        Ok((ty_env, ty_subst, Type::TyVar(ty_var_id)))
    }
}

impl GlobalVariableIr {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute, Type)> {
        let (ty_var_id, ty_env) = ty_env.get(self.id.clone());
        Ok((ty_env, ty_subst, Type::TyVar(ty_var_id)))
    }
}

impl TupleIr {
    fn ty_get(&self, ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TyCheckResult<(TypeEnv, TypeSubstitute, Type)> {
        let (ty_env, ty_subst, elements_ty): (TypeEnv, TypeSubstitute, Vec<Type>) =
            self.elements
                .iter()
                .fold(Ok((ty_env, ty_subst, Vec::<Type>::new())), |acc, x| {
                    let (ty_env, ty_subst, mut elements) = acc?;
                    let (ty_env, ty_subst, element) = x.ty_get(ty_env, ty_subst)?;
                    elements.push(element);
                    Ok((ty_env, ty_subst, elements))
                })?;
        let ret_ty = Type::TupleType(Box::new(TupleType { element_tys: elements_ty }));
        Ok((ty_env, ty_subst, ret_ty))
    }
}