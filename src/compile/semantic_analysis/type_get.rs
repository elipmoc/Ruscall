use super::super::error::Error;
use super::super::types::types::*;
use super::type_env::*;
use super::mir::*;
use super::type_inference::assump_env::AssumpEnv;

type TyCheckResult<T> = Result<T, Error>;

trait TypeGet {
    fn ty_get(&self, TypeInfo, assump_env: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)>;
}

fn ty_get_all<Item: TypeGet, T: Iterator<Item=Item>>(iter: T, ty_info: TypeInfo, assump_env: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Vec<Qual<Type>>)>
{
    let (ty_info, assump_env, ty_list) =
        iter.fold(Ok((ty_info, assump_env, vec![])), |acc, x| {
            let (ty_info, assump_env, mut ty_list) = acc?;
            let (ty_info, assump_env, ty) = x.ty_get(ty_info, assump_env)?;
            ty_list.push(ty);
            Ok((ty_info, assump_env, ty_list))
        })?;
    Ok((ty_info, assump_env, ty_list))
}

impl ProgramMir {
    pub fn ty_get(mut self) -> TyCheckResult<ProgramMir> {
        let assump = AssumpEnv::new();
        //外部関数宣言の型チェック
        let (ty_info, mut assump, _) =
            ty_get_all(
                self.ex_dec_func_list.iter(), self.ty_info, assump,
            )?;
        self.explicit_func_list.iter()
            .map(|x| (x.func.name.clone(), x.scheme.get_qual().clone()))
            .chain(self.ex_dec_func_list.iter().map(|x| (
                x.name.clone(),
                Qual {
                    t: Type::LambdaType(Box::new(LambdaType { env_ty: None, func_ty: x.ty.t.clone() })),
                    ps: x.ty.ps.clone(),
                }
            )))
            .for_each(|(name, qual)| assump.global_set(name, Scheme::Forall { qual, tgen_count: 0 }));
        //関数宣言の型チェック
        let (ty_info, assump, _) =
            ty_get_all(self.implicit_func_list.iter().map(|(_, x)| x), ty_info, assump)?;
        //関数定義の型チェック
        let (ty_info, _, _) =
            ty_get_all(self.explicit_func_list.iter().map(|x| x), ty_info, assump)?;
        self.ty_info = ty_info;
        Ok(self)
    }
}

impl<'a> TypeGet for &'a ImplicitFunc {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let (mut ty_info, mut assump, q) = (&self.func).ty_get(ty_info, assump)?;
        let scheme = Scheme::quantify(q, &mut ty_info)
            .map_err(|msg| Error::new(self.func.pos, &msg))?;
        assump.global_set(self.func.name.clone(), scheme.clone());
        Ok((ty_info, assump, scheme.get_qual().clone()))
    }
}

impl<'a> TypeGet for &'a ExplicitFunc {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        match &self.scheme {
            Scheme::Forall { qual, .. } => {
                let (ty_info, assump, ty) = (&self.func).ty_get(ty_info, assump)?;
                let (mut ty_info, q) = ty_info.qual_unify(
                    ty,
                    qual.clone(),
                ).map_err(|msg| Error::new(self.func.pos, &msg))?;
                let func_ty_id = ty_info.global_get(self.func.name.clone());
                ty_info.unify(
                    Type::TyVar(func_ty_id.clone()),
                    q.t.clone(),
                ).map_err(|msg| Error::new(self.func.pos, &msg))?;
                Ok((ty_info, assump, q))
            }
        }
    }
}

impl<'a> TypeGet for &'a DecFuncMir {
    fn ty_get(&self, mut ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let func_q = self.ty.clone();
        let func_ty_id = ty_info.global_get(self.name.clone());
        ty_info.unify(
            Type::TyVar(func_ty_id.clone()),
            Type::LambdaType(Box::new(LambdaType { env_ty: None, func_ty: func_q.t })),
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, assump, Qual::new(Type::TyVar(func_ty_id))))
    }
}

impl<'a> TypeGet for &'a FuncMir {
    fn ty_get(&self, mut ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        ty_info.in_nest();
        let params_ty: Vec<Type>
        = (0..self.params_len)
            .map(|id| {
                let id = self.params_len - id - 1;
                let ty_id = ty_info.get(id.to_string());
                Type::TyVar(ty_id)
            }).collect();
        let (mut ty_info, assump, ret_q) = (&self.body).ty_get(ty_info, assump)?;
        let func_q =
            match assump.global_get(&self.name) {
                Some(scheme) => scheme.get_qual().clone(),
                None => Qual::new(Type::TyVar(ty_info.global_get(self.name.clone())))
            };

        let (mut ty_info, q) = ty_info.qual_unify(
            func_q,
            Qual {
                ps: ret_q.ps
                ,
                t: Type::create_func_type(
                    params_ty,
                    ret_q.t,
                ),
            },
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        ty_info.out_nest();
        Ok((ty_info, assump, q))
    }
}


impl<'a> TypeGet for &'a ExprMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        match self {
            ExprMir::NumMir(x) => x.ty_get(ty_info, assump),
            ExprMir::BoolMir(x) => x.ty_get(ty_info, assump),
            ExprMir::IfMir(x) => x.ty_get(ty_info, assump),
            ExprMir::CallMir(x) => x.ty_get(ty_info, assump),
            ExprMir::OpMir(x) => x.ty_get(ty_info, assump),
            ExprMir::VariableMir(x) => x.ty_get(ty_info, assump),
            ExprMir::GlobalVariableMir(x) => x.ty_get(ty_info, assump),
            ExprMir::TupleMir(x) => x.ty_get(ty_info, assump),
            ExprMir::TupleStructMir(x) => x.ty_get(ty_info, assump),
            ExprMir::IndexPropertyMir(x) => x.ty_get(ty_info, assump),
            ExprMir::NamePropertyMir(x) => x.ty_get(ty_info, assump),
            ExprMir::LambdaMir(x) => x.ty_get(ty_info, assump)
        }
    }
}

impl TypeGet for NumMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        Ok((ty_info, assump, Qual::new(Type::create_int32())))
    }
}

impl TypeGet for BoolMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        Ok((ty_info, assump, Qual::new(Type::create_bool())))
    }
}

impl TypeGet for IfMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let (ty_info, assump, cond_ty) = (&self.cond).ty_get(ty_info, assump)?;
        let (ty_info, assump, t_expr_ty) = (&self.t_expr).ty_get(ty_info, assump)?;
        let (ty_info, assump, f_expr_ty) = (&self.f_expr).ty_get(ty_info, assump)?;
        let (ty_info, q1) = ty_info.qual_unify(cond_ty, Qual::new(Type::create_bool()))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let (ty_info, q2) = ty_info.qual_unify(t_expr_ty, f_expr_ty)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let ret_ty = Type::TyVar(self.ty_id.clone());
        let (ty_info, mut ret_q) = ty_info.qual_unify(Qual::new(ret_ty), q2)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let (ty_info, ps) = ty_info.preds_merge_unify(q1.ps, ret_q.ps)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        ret_q.ps = ps;
        Ok((ty_info, assump, ret_q))
    }
}

impl TypeGet for CallMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let (ty_info, assump, params_qs) =
            ty_get_all(self.params.iter(), ty_info, assump)?;
        let (pss, param_types) = Qual::split(params_qs);
        let ret_ty_id = self.ty_id.clone();
        let ret_ty = Type::TyVar(ret_ty_id);
        let (ty_info, assump, func_q) = (&self.func).ty_get(ty_info, assump)?;
        let (ty_info, func_q) = ty_info.qual_condition_add_unify(func_q, Condition::Call(Box::new(FuncType { param_types, ret_type: ret_ty.clone() })))
            .map_err(|msg| Error::new(self.func.get_pos(), &msg))?;
        let (ty_info, ps) = ty_info.predss_merge_unify(pss)
            .map_err(|msg| Error::new(self.func.get_pos(), &msg))?;
        let (ty_info, ps) = ty_info.preds_merge_unify(ps, func_q.ps)
            .map_err(|msg| Error::new(self.func.get_pos(), &msg))?;
        let ret_ty = ty_info.look_up(&self.ty_id);
        Ok((ty_info, assump, Qual { ps, t: ret_ty }))
    }
}

impl TypeGet for OpMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let ret_ty = match &(self.op) as &str {
            "==" => Type::create_bool(),
            _ => Type::create_int32(),
        };

        let (ty_info, assump, l_expr_ty) = (&self.l_expr)
            .ty_get(ty_info, assump)?;
        let (ty_info, l_q) = ty_info.qual_unify(l_expr_ty, Qual::new(Type::create_int32()))
            .map_err(|msg| Error::new(self.l_expr.get_pos(), &msg))?;

        let (ty_info, assump, r_expr_ty) = (&self.r_expr)
            .ty_get(ty_info, assump)?;
        let (ty_info, r_q) = ty_info.qual_unify(r_expr_ty, Qual::new(Type::create_int32()))
            .map_err(|msg| Error::new(self.r_expr.get_pos(), &msg))?;
        let (ty_info, ps) = ty_info.preds_merge_unify(r_q.ps, l_q.ps)
            .map_err(|msg| Error::new(self.l_expr.get_pos(), &msg))?;
        Ok((ty_info, assump, Qual { ps, t: ret_ty }))
    }
}

impl<'a> TypeGet for &'a VariableMir {
    fn ty_get(&self, mut ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let ty_var_id = ty_info.get(self.id.to_string());
        let q = Qual::new(Type::TyVar(ty_var_id));
        let (ty_info, q) = ty_info.qual_unify(q, Qual::new(Type::TyVar(self.ty_id.clone())))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, assump, q))
    }
}

impl TypeGet for GlobalVariableMir {
    fn ty_get(&self, mut ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        match assump.global_get(&self.id).cloned() {
            Some(scheme) => {
                let q = scheme.fresh_inst(&mut ty_info);
                let (ty_info, q) = ty_info.qual_unify(q, Qual::new(Type::TyVar(self.ty_id.clone())))
                    .map_err(|msg| Error::new(self.pos, &msg))?;
                Ok((ty_info, assump, q))
            }
            None => {
                let ty_var_id = ty_info.global_get(self.id.clone());
                let ty = ty_info.unify(Type::TyVar(ty_var_id), Type::TyVar(self.ty_id.clone()))
                    .map_err(|msg| Error::new(self.pos, &msg))?;
                Ok((ty_info, assump, Qual::new(ty)))
            }
        }
    }
}

impl TypeGet for TupleMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let (ty_info, assump, elements_qs) =
            ty_get_all(self.elements.iter(), ty_info, assump)?;
        let (pss, element_tys) = Qual::split(elements_qs);
        let (ty_info, ps) = ty_info.predss_merge_unify(pss)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let tuple_q = Qual { ps, t: Type::TupleType(Box::new(TupleType { element_tys })) };
        let (ty_info, q) = ty_info.qual_unify(tuple_q, Qual::new(Type::TyVar(self.ty_id.clone())))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, assump, q))
    }
}

impl TypeGet for TupleStructMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let (ty_info, assump, tuple_q) = self.tuple.ty_get(ty_info, assump)?;
        let internal_ty = match self.ty.ty {
            StructInternalType::RecordType(ref x) => Type::TupleType(Box::new(TupleType {
                element_tys: x.element_tys.iter()
                    .map(|(_, x)| x.clone())
                    .collect()
            })),
            StructInternalType::TupleType(ref x) => Type::TupleType(Box::new(x.clone()))
        };
        let (ty_info, tuple_q) = ty_info.qual_unify(tuple_q, Qual::new(internal_ty))
            .map_err(|msg| Error::new(self.tuple.pos, &msg))?;
        Ok((ty_info, assump, Qual { ps: tuple_q.ps, t: Type::StructType(Box::new(self.ty.clone())) }))
    }
}

impl TypeGet for LambdaMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let (mut ty_info, assump, envs_qs) =
            ty_get_all(self.env.iter(), ty_info, assump)?;
        ty_info.in_nest();
        let params_ty: Vec<Type> =
            (0..self.params_len)
                .map(|id| {
                    let id = self.params_len - id - 1;
                    let ty_id = ty_info.get((id).to_string());
                    Type::TyVar(ty_id)
                }).collect();
        let func_ty = match assump.global_get(&self.func_name) {
            Some(scheme) => scheme.clone().fresh_inst(&mut ty_info),
            None => Qual::new(Type::TyVar(ty_info.global_get(self.func_name.clone())))
        };
        let (mut ty_info, func_ty) = ty_info.qual_unify(func_ty, Qual::new(Type::TyVar(self.func_id.clone())))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        ty_info.out_nest();
        let mut new_envs_qs = Vec::with_capacity(envs_qs.len());
        {
            let iter = envs_qs.into_iter().zip(&params_ty);
            for (q, t) in iter {
                let (new_ty_info, q) = ty_info.qual_unify(q, Qual::new(t.clone()))
                    .map_err(|msg| Error::new(self.pos, &msg))?;
                ty_info = new_ty_info;
                new_envs_qs.push(q);
            }
        }
        let (pss, envs_ty) = Qual::split(new_envs_qs);
        let (mut ty_info, ps) = ty_info.predss_merge_unify(pss)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let ret_id = ty_info.no_name_get();
        let func_ty2 = FuncType { param_types: params_ty, ret_type: Type::TyVar(ret_id) };
        let (ty_info, func_q) = ty_info.qual_unify(func_ty, Qual::new(Type::create_func_type(func_ty2.param_types, func_ty2.ret_type)))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let func_ty2 = match func_q.t {
            Type::LambdaType(x) => x.func_ty,
            _ => panic!("error!")
        };
        let mut lambda_q = if envs_ty.len() == 0 {
            Qual {
                ps: func_q.ps,
                t: Type::LambdaType(Box::new(LambdaType {
                    func_ty: func_ty2.clone(),
                    env_ty: None,
                })),
            }
        } else {
            Qual {
                ps: func_q.ps,
                t: Type::create_lambda_type(envs_ty, func_ty2.clone()),
            }
        };
        let (ty_info, ps) = ty_info.preds_merge_unify(ps, lambda_q.ps)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        lambda_q.ps = ps;
        let (ty_info, lambda_q) = ty_info.qual_unify(lambda_q, Qual::new(Type::TyVar(self.ty_id.clone())))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((ty_info, assump, lambda_q))
    }
}


impl TypeGet for IndexPropertyMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let (ty_info, assump, expr_ty) = (&self.expr).ty_get(ty_info, assump)?;
        let property_ty =
            Type::TyVar(
                self.ty_id.clone(),
            );
        let (ty_info, mut q) =
            ty_info.qual_condition_add_unify(expr_ty, Condition::Items(Box::new(ImplItems::with_index_property(self.index, property_ty.clone()))),
            ).map_err(|msg| Error::new(self.pos, &msg))?;
        q.t = ty_info.type_look_up(&property_ty);

        Ok((ty_info, assump, q))
    }
}

impl TypeGet for NamePropertyMir {
    fn ty_get(&self, ty_info: TypeInfo, assump: AssumpEnv) -> TyCheckResult<(TypeInfo, AssumpEnv, Qual<Type>)> {
        let (ty_info, assump, expr_ty) = (&self.expr).ty_get(ty_info, assump)?;
        let property_ty =
            Type::TyVar(
                self.ty_id.clone(),
            );
        let (ty_info, mut q) =
            ty_info.qual_condition_add_unify(expr_ty, Condition::Items(Box::new(ImplItems::with_name_property(self.property_name.clone(), property_ty.clone()))),
            ).map_err(|msg| Error::new(self.pos, &msg))?;
        q.t = property_ty;
        Ok((ty_info, assump, q))
    }
}