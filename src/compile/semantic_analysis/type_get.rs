use super::super::error::Error;
use super::super::types::*;
use super::type_inference::type_substitute::TypeSubstitute;
use super::type_inference::type_env::TypeEnv;
use super::mir::*;
use super::type_inference::assump_env::AssumpEnv;

type TyCheckResult<T> = Result<T, Error>;

trait TypeGet {
    fn ty_get(&self, &mut TypeSubstitute, &mut TypeEnv, assump_env: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)>;
}

fn ty_get_all<Item: TypeGet, T: Iterator<Item=Item>>(iter: T, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump_env: AssumpEnv) -> TyCheckResult<(AssumpEnv, Vec<Qual<Type>>)>
{
    let (assump_env, ty_list) =
        iter.fold(Ok((assump_env, vec![])), |acc, x| {
            let (assump_env, mut ty_list) = acc?;
            let (assump_env, ty) = x.ty_get(ty_sub, ty_env, assump_env)?;
            ty_list.push(ty);
            Ok((assump_env, ty_list))
        })?;
    Ok((assump_env, ty_list))
}

impl ProgramMir {
    pub fn ty_get(mut self) -> TyCheckResult<(ProgramMir, AssumpEnv)> {
        let assump = AssumpEnv::new();
        //外部関数宣言の型チェック
        let (mut assump, _) =
            ty_get_all(
                self.ex_dec_func_list.iter(), &mut self.ty_sub, &mut self.ty_env, assump,
            )?;
        {
            self.explicit_func_list.iter()
                .map(|x| (x.func.name.clone(), x.scheme.get_qual().clone()))
                .chain(self.ex_dec_func_list.iter().map(|x| (
                    x.name.clone(),
                    Qual {
                        t: Type::TApp(Box::new(x.ty.t.clone())),
                        ps: x.ty.ps.clone(),
                    }
                )))
                .for_each(|(name, qual)| assump.global_set(name, Scheme::quantify(qual.t.get_func_ty().0.tv_list(), qual)));
        }
        //関数宣言の型チェック
        let (assump, _) =
            ty_get_all(self.implicit_func_list.iter().map(|(_, x)| x), &mut self.ty_sub, &mut self.ty_env, assump)?;
        //関数定義の型チェック
        let (assump, _) =
            ty_get_all(self.explicit_func_list.iter().map(|x| x), &mut self.ty_sub, &mut self.ty_env, assump)?;
        // main関数の型をチェック
        let main_func_q = assump.global_get(&"main".to_string()).unwrap().clone().fresh_inst(&mut self.ty_env);
        let main_func_ty = Type::create_func_type(vec![Type::create_tuple_type(vec![])], Type::create_int32());
        self.ty_sub.qual_unify(main_func_q, Qual::new(main_func_ty)).map_err(|msg| Error::new(self.get_func_mir(&"main".to_string()).unwrap().pos, &msg))?;

        println!("\nAssump List \n");
        println!("{:?}", assump);
        Ok((self, assump))
    }
}

impl<'a> TypeGet for &'a ImplicitFunc {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let (mut assump, q) = (&self.func).ty_get(ty_sub, ty_env, assump)?;
        let scheme = Scheme::quantify(q.tv_list(), q);
        assump.global_set(self.func.name.clone(), scheme.clone());
        Ok((assump, scheme.get_qual().clone()))
    }
}

impl<'a> TypeGet for &'a ExplicitFunc {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        match assump.global_get(&self.func.name).unwrap().clone() {
            Scheme::Forall { qual, .. } => {
                let (mut assump, ty) = (&self.func).ty_get(ty_sub, ty_env, assump)?;

                let ty = Scheme::quantify(ty.t.get_func_ty().0.tv_list(), ty);
                let mut ty = ty.get_qual().clone().apply(&ty_sub, true);
                let mut qual = qual.clone().apply(&ty_sub, true);
                ty.ps = ty_sub.preds_simply(ty.ps, ty.t.tv_list());
                qual.ps = ty_sub.preds_simply(qual.ps, qual.t.tv_list());
                if qual != ty {
                    return Err(Error::new(self.func.pos, &format!("declar: {:?} \nactual: {:?}", qual, ty)));
                }
                Ok((assump, qual))
            }
        }
    }
}

impl<'a> TypeGet for &'a DecFuncMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let func_type_q = self.ty.clone();
        let func_q = Qual::new(ty_env.global_get(self.name.clone()));
        let q = ty_sub.qual_unify(
            func_q,
            Qual {
                t: Type::TApp(Box::new(func_type_q.t)),
                ps: func_type_q.ps,
            },
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((assump, q))
    }
}

impl<'a> TypeGet for &'a FuncMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        ty_env.in_nest();
        let params_ty: Vec<Type>
            = (0..self.params_len)
            .map(|id| {
                let id = self.params_len - id - 1;
                let ty = ty_env.get(id.to_string());
                ty
            }).collect();
        let (assump, ret_q) = (&self.body).ty_get(ty_sub, ty_env, assump)?;
        let func_q =
            match assump.global_get(&self.name) {
                Some(scheme) => scheme.get_qual().clone(),
                None => Qual::new(ty_env.global_get(self.name.clone()))
            };

        let q = ty_sub.qual_unify(
            func_q,
            Qual {
                ps: ret_q.ps,
                t: Type::create_func_type(
                    params_ty,
                    ret_q.t,
                ),
            },
        ).map_err(|msg| Error::new(self.pos, &msg))?;
        ty_env.out_nest();
        Ok((assump, q))
    }
}


impl<'a> TypeGet for &'a ExprMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        match self {
            ExprMir::NumMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::BoolMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::IfMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::CallMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::OpMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::VariableMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::GlobalVariableMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::TupleMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::TupleStructMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::IndexPropertyMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::NamePropertyMir(x) => x.ty_get(ty_sub, ty_env, assump),
            ExprMir::LambdaMir(x) => x.ty_get(ty_sub, ty_env, assump)
        }
    }
}

impl TypeGet for NumMir {
    fn ty_get(&self, _ty_sub: &mut TypeSubstitute, _ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        Ok((assump, Qual::new(Type::create_int32())))
    }
}

impl TypeGet for BoolMir {
    fn ty_get(&self, _ty_sub: &mut TypeSubstitute, _ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        Ok((assump, Qual::new(Type::create_bool())))
    }
}

impl TypeGet for IfMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let (assump, cond_ty) = (&self.cond).ty_get(ty_sub, ty_env, assump)?;
        let (assump, t_expr_ty) = (&self.t_expr).ty_get(ty_sub, ty_env, assump)?;
        let (assump, f_expr_ty) = (&self.f_expr).ty_get(ty_sub, ty_env, assump)?;
        let q1 = ty_sub.qual_unify(cond_ty, Qual::new(Type::create_bool()))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let q2 = ty_sub.qual_unify(t_expr_ty, f_expr_ty)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let ret_ty = Type::TyVar(self.ty_id.clone());
        let mut ret_q = ty_sub.qual_unify(Qual::new(ret_ty), q2)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let ps = ty_sub.preds_merge_unify(q1.ps, ret_q.ps)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        ret_q.ps = ps;
        Ok((assump, ret_q))
    }
}

impl TypeGet for CallMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let (assump, params_qs) =
            ty_get_all(self.params.iter(), ty_sub, ty_env, assump)?;
        let (pss, param_types) = Qual::split(params_qs);
        let ret_type = Type::TyVar(self.ty_id.clone());
        let (assump, func_q) = (&self.func).ty_get(ty_sub, ty_env, assump)?;
        let func_q = ty_sub.qual_add_condition_unify(func_q, Condition::Call(Box::new(Type::create_func_type(param_types, ret_type).to_func_ty())))
            .map_err(|msg| Error::new(self.func.get_pos(), &msg))?;
        let ret_ty = ty_sub.look_up(&self.ty_id, false);
        let ps = ty_sub.predss_merge_unify(pss)
            .map_err(|msg| Error::new(self.func.get_pos(), &msg))?;
        let ps = ty_sub.preds_merge_unify(ps, func_q.ps)
            .map_err(|msg| Error::new(self.func.get_pos(), &msg))?;
        Ok((assump, Qual { ps, t: ret_ty }))
    }
}

impl TypeGet for OpMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let ret_ty = match &(self.op) as &str {
            "==" => Type::create_bool(),
            _ => Type::create_int32(),
        };

        let (assump, l_expr_ty) = (&self.l_expr)
            .ty_get(ty_sub, ty_env, assump)?;
        let l_q = ty_sub.qual_unify(l_expr_ty, Qual::new(Type::create_int32()))
            .map_err(|msg| Error::new(self.l_expr.get_pos(), &msg))?;

        let (assump, r_expr_ty) = (&self.r_expr)
            .ty_get(ty_sub, ty_env, assump)?;
        let r_q = ty_sub.qual_unify(r_expr_ty, Qual::new(Type::create_int32()))
            .map_err(|msg| Error::new(self.r_expr.get_pos(), &msg))?;
        let ps = ty_sub.preds_merge_unify(r_q.ps, l_q.ps)
            .map_err(|msg| Error::new(self.l_expr.get_pos(), &msg))?;
        Ok((assump, Qual { ps, t: ret_ty }))
    }
}

impl<'a> TypeGet for &'a VariableMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let q = Qual::new(ty_env.get(self.id.to_string()));
        let q = ty_sub.qual_unify(q, Qual::new(Type::TyVar(self.ty_id.clone())))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((assump, q))
    }
}

impl TypeGet for GlobalVariableMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        match assump.global_get(&self.id).cloned() {
            Some(scheme) => {
                let q = scheme.fresh_inst(ty_env);
                let q = ty_sub.qual_unify(q, Qual::new(Type::TyVar(self.ty_id.clone())))
                    .map_err(|msg| Error::new(self.pos, &msg))?;
                Ok((assump, q))
            }
            None => {
                let q = Qual::new(ty_env.global_get(self.id.clone()));
                let q = ty_sub.qual_unify(q, Qual::new(Type::TyVar(self.ty_id.clone())))
                    .map_err(|msg| Error::new(self.pos, &msg))?;
                Ok((assump, q))
            }
        }
    }
}

impl TypeGet for TupleMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let (assump, elements_qs) =
            ty_get_all(self.elements.iter(), ty_sub, ty_env, assump)?;
        let (pss, element_tys) = Qual::split(elements_qs);
        let tuple_ty = Type::TupleType(Box::new(TupleType { element_tys }));
        let ps = ty_sub.predss_merge_unify(pss)
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let tuple_q = Qual { ps, t: tuple_ty };
        let q = ty_sub.qual_unify(tuple_q, Qual::new(Type::TyVar(self.ty_id.clone())))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((assump, q))
    }
}

impl TypeGet for TupleStructMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let (assump, tuple_q) = self.tuple.ty_get(ty_sub, ty_env, assump)?;
        let internal_ty = match self.ty.ty {
            StructInternalType::RecordType(ref x) => Type::TupleType(Box::new(TupleType {
                element_tys: x.element_tys.iter()
                    .map(|(_, x)| x.clone())
                    .collect()
            })),
            StructInternalType::TupleType(ref x) => Type::TupleType(Box::new(x.clone()))
        };
        let tuple_q = ty_sub.qual_unify(tuple_q, Qual::new(internal_ty))
            .map_err(|msg| Error::new(self.tuple.pos, &msg))?;
        Ok((assump, Qual { ps: tuple_q.ps, t: Type::StructType(Box::new(self.ty.clone())) }))
    }
}

impl TypeGet for LambdaMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        ty_env.in_nest();
        let params_ty: Vec<Type> =
            (0..self.params_len)
                .map(|id| {
                    let id = self.params_len - id - 1;
                    let ty = ty_env.get((id).to_string());
                    ty
                }).collect();
        let func_ty = match assump.global_get(&self.func_name) {
            Some(scheme) => {
                scheme.clone().fresh_inst(ty_env)
            }
            None => Qual::new(ty_env.global_get(self.func_name.clone()))
        };
        let func_ty = ty_sub.qual_unify(func_ty, Qual::new(Type::TyVar(self.ty_id.clone())))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        ty_env.out_nest();

        let ret_type = ty_env.no_name_get();
        let func_q = ty_sub.qual_unify(func_ty, Qual::new(Type::create_func_type(params_ty, ret_type)))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        let mut lambda_q = Qual {
            ps: func_q.ps,
            t: func_q.t,
        };
        let lambda_q = ty_sub.qual_unify(lambda_q, Qual::new(Type::TyVar(self.ty_id.clone())))
            .map_err(|msg| Error::new(self.pos, &msg))?;
        Ok((assump, lambda_q))
    }
}


impl TypeGet for IndexPropertyMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let (assump, expr_ty) = (&self.expr).ty_get(ty_sub, ty_env, assump)?;
        let property_ty =
            Type::TyVar(
                self.ty_id.clone(),
            );
        let mut q =
            ty_sub.qual_add_condition_unify(expr_ty, Condition::Items(Box::new(ImplItems::with_index_property(self.index, property_ty.clone()))),
            ).map_err(|msg| Error::new(self.pos, &msg))?;
        q.t = ty_sub.type_look_up(&property_ty, false);

        Ok((assump, q))
    }
}

impl TypeGet for NamePropertyMir {
    fn ty_get(&self, ty_sub: &mut TypeSubstitute, ty_env: &mut TypeEnv, assump: AssumpEnv) -> TyCheckResult<(AssumpEnv, Qual<Type>)> {
        let (assump, expr_ty) = (&self.expr).ty_get(ty_sub, ty_env, assump)?;
        let property_ty =
            Type::TyVar(
                self.ty_id.clone(),
            );
        let mut q =
            ty_sub.qual_add_condition_unify(expr_ty, Condition::Items(Box::new(ImplItems::with_name_property(self.property_name.clone(), property_ty.clone()))),
            ).map_err(|msg| Error::new(self.pos, &msg))?;
        q.t = property_ty;
        Ok((assump, q))
    }
}