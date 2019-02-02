use super::super::super::types::*;
use super::type_env::TypeSubstitute;
use super::occurs_check::occurs_check;
use std::fmt::Debug;
use std::collections::HashMap;

//型エラーの生成
fn create_error<A: Debug, B: Debug, T>(ty1: &A, ty2: &B) -> Result<T, String> {
    Err(format!(
        "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
        ty1, ty2
    ))
}

//エラーメッセージの生成
fn create_error_msg<T>(msg: String) -> Result<T, String> {
    Err(format!(
        "error! \n {}", msg
    ))
}

impl TypeSubstitute {
    //単一化処理
    pub fn unify(&mut self, ty1: Type, ty2: Type) -> Result<Type, String> {
        let (ty1, ty2) = (self.type_look_up(&ty1), self.type_look_up(&ty2));
        if ty1 == ty2 { return Ok(ty1); }
        match (ty1, ty2) {
            (Type::TGen(_), Type::TGen(_)) => panic!("hoge"),
            (Type::TGen(_), ty) | (ty, Type::TGen(_)) => Ok(ty),
            (Type::TyVar(id), ty) | (ty, Type::TyVar(id)) => {
                self.safe_insert(id.clone(), ty.clone());
                Ok(ty)
            }
            (Type::LambdaType(ty1), Type::LambdaType(ty2)) => self.lambda_unify(*ty1, *ty2),
            (Type::TupleType(ty1), Type::TupleType(ty2)) => self.tuple_unify(*ty1, *ty2),
            (ty1, ty2) => create_error(&ty1, &ty2)
        }
    }

    //型変数に型を代入する
    fn safe_insert(&mut self, ty_id: TypeId, insert_ty: Type) {
        if occurs_check(&self.ty_sub, &insert_ty, &ty_id) == false {
            println!("{:?}=>{:?}", ty_id, insert_ty);
            self.ty_sub.insert(ty_id, insert_ty);
        } else {
            println!("occurs! {:?}=>{:?}", ty_id, insert_ty);
        }
    }
    fn qual_left(&mut self, q1: Qual<Type>, q2: &Qual<Type>) -> Result<Qual<Type>, String> {
        match &q2.t {
            Type::TyVar(id) => {
                match q2.ps.get(&Type::TyVar(id.clone())) {
                    Some(p) => self.qual_add_condition_unify(q1, p.cond.clone()),
                    None => Ok(q1)
                }
            }
            _ => Ok(q1)
        }
    }

    pub fn qual_unify(&mut self, q1: Qual<Type>, q2: Qual<Type>) -> Result<Qual<Type>, String> {
        let q1 = self.last_qual(q1)?;
        let q2 = self.last_qual(q2)?;
        let q1 = self.qual_left(q1, &q2)?;
        let q2 = self.qual_left(q2, &q1)?;
        let t = self.unify(q1.t, q2.t)?;
        let ps = self.preds_merge_unify(q1.ps, q2.ps)?;
        Ok(Qual { ps, t })
    }

    pub fn last_qual(&mut self, q: Qual<Type>) -> Result<Qual<Type>, String> {
        let ty = self.type_look_up(&q.t);
        let ps = self.preds_reduction(q.ps)?;
        Ok(Qual { ps, t: ty })
    }

    pub fn preds_reduction(&mut self, ps: Preds) -> Result<Preds, String> {
        let mut new_ps = HashMap::new();
        for (_, p) in ps {
            match self.type_look_up(&p.ty) {
                Type::TyVar(id) => {
                    let p = Pred { ty: Type::TyVar(id.clone()), cond: p.cond };
                    match new_ps.remove(&Type::TyVar(id.clone())) {
                        Some(p2) => {
                            let p = self.pred_unify(p2, p)?;
                            new_ps.insert(Type::TyVar(id.clone()), p);
                        }
                        None => { new_ps.insert(Type::TyVar(id), p); }
                    }
                }
                t => { self.qual_add_condition_unify(Qual::new(t), p.cond)?; }
            }
        };
        Ok(new_ps)
    }

    pub fn preds_merge_unify(&mut self, ps1: Preds, mut ps2: Preds) -> Result<Preds, String> {
        let ps1 = self.preds_reduction(ps1)?;
        let mut new_ps = HashMap::new();
        for (k, p1) in ps1 {
            match ps2.remove(&k) {
                Some(p2) => {
                    let p = self.pred_unify(p1, p2)?;
                    new_ps.insert(k, p);
                }
                None => { new_ps.insert(k, p1); }
            };
        }
        ps2.into_iter().for_each(|(k, p)| { new_ps.insert(k, p); });
        Ok(new_ps)
    }

    //Qualに新たに制約を追加する操作と単一化処理
    pub fn qual_add_condition_unify(&mut self, q: Qual<Type>, c: Condition) -> Result<Qual<Type>, String> {
        let mut q = self.last_qual(q)?;
        use self::Type::*;
        match &q.t {
            t @ TCon { .. } => { return create_error(&t, &c); }
            TyVar(x) => {
                let p = q.ps.remove(&TyVar(x.clone()));
                match p {
                    Some(p) => {
                        let p = self.pred_unify(p, Pred { ty: TyVar(x.clone()), cond: c })?;
                        q.ps.insert(TyVar(x.clone()), p)
                    }
                    None => q.ps.insert(TyVar(x.clone()), Pred { ty: TyVar(x.clone()), cond: c })
                };
            }
            tgen @ TGen(_) => {
                let p = q.ps.remove(&tgen);
                let fresh_tvar = TyVar(self.ty_env.no_name_get());
                match p {
                    Some(p) => {
                        let p = self.pred_unify(p, Pred { ty: fresh_tvar.clone(), cond: c })?;
                        q.ps.insert(fresh_tvar, p)
                    }
                    None => q.ps.insert(fresh_tvar.clone(), Pred { ty: fresh_tvar, cond: c })
                };
            }
            LambdaType(x) => {
                match c {
                    Condition::Call(c) => {
                        let env_len = x.env_ty.clone().map(|t| t.element_tys.len()).unwrap_or(0);
                        let mut func_ty = x.func_ty.clone();
                        func_ty.param_types = func_ty.param_types.clone().into_iter().skip(env_len).collect::<Vec<_>>();
                        self.fn_unify(func_ty, *c)?;
                    }
                    Condition::Empty => (),
                    c => { return create_error(&x, &c); }
                };
            }
            TupleType(x) => { self.tuple_condition_unify((**x).clone(), c)?; }
            StructType(x) => { self.tuple_condition_unify((**x).clone(), c)?; }
        }
        Ok(q)
    }

    //型制約の単一化処理
    fn pred_unify(&mut self, p1: Pred, p2: Pred) -> Result<Pred, String> {
        use self::Condition::*;
        if p1.ty != p2.ty { panic!("error!") }
        match (p1.cond, p2.cond) {
            (Call(fn_ty1), Call(fn_ty2)) => {
                let new_fn_ty = self.fn_unify(*fn_ty1, *fn_ty2)?;
                let ty_id = self.ty_env.no_name_get();
                Ok(Pred { cond: Condition::Call(Box::new(new_fn_ty)), ty: Type::TyVar(ty_id) })
            }
            (c, Empty) | (Empty, c) => {
                let ty_id = self.ty_env.no_name_get();
                Ok(Pred { ty: Type::TyVar(ty_id), cond: c })
            }
            (Items(impl_items1), Items(impl_items2)) => {
                let impl_items = ImplItems::merge(
                    *impl_items1, *impl_items2,
                    &mut |ty1, ty2| {
                        self.unify(ty1, ty2)
                    })?;
                Ok(Pred { ty: p1.ty, cond: Items(Box::new(impl_items)) })
            }
            (p1, p2) => create_error(&p1, &p2)
        }
    }

    //タプルと型制約の単一化
    fn tuple_condition_unify<T: TupleTypeBase + Debug>(&mut self, tuple_ty: T, c: Condition)
                                                       -> Result<T, String> {
        match c {
            c @ Condition::Call(_) => { return create_error(&tuple_ty, &c); }
            Condition::Items(ref impl_items) => {
                for (name, ty) in impl_items.get_name_properties() {
                    match tuple_ty.get_elements_from_record_name(name) {
                        Some(element_ty) => { self.unify(ty.clone(), element_ty.clone())?; }
                        None => { return create_error_msg("not found property".to_string() + &name.clone()); }
                    }
                };
                for (index, ty) in impl_items.get_index_properties() {
                    let index = *index as usize;
                    if index >= tuple_ty.get_elements_len() {
                        return create_error_msg(format!("over index {:?}.{}", tuple_ty, index));
                    }
                    self.unify(ty.clone(), tuple_ty.get_elements_at(index).clone())?;
                };
            }
            Condition::Empty => ()
        };
        Ok(tuple_ty)
    }

    //関数の単一化処理
    fn fn_unify(&mut self, ty1: FuncType, ty2: FuncType) -> Result<FuncType, String> {
        if ty1.param_types.len() != ty2.param_types.len() {
            return create_error(&ty1, &ty2);
        }
        let mut new_param_types: Vec<Type> = Vec::with_capacity(ty1.param_types.len());
        for (x, y) in ty1.param_types.into_iter().zip(ty2.param_types) {
            let ty = self.unify(x, y)?;
            new_param_types.push(ty);
        }
        let new_ret_type = self.unify(ty1.ret_type, ty2.ret_type)?;
        Ok(FuncType { param_types: new_param_types, ret_type: new_ret_type })
    }

    //タプルの単一化処理
    fn tuple_unify(&mut self, ty1: TupleType, ty2: TupleType) -> Result<Type, String> {
        if ty1.element_tys.len() != ty2.element_tys.len() {
            return create_error(&ty1, &ty2);
        }
        for (x, y) in ty1.element_tys.clone().into_iter().zip(ty2.element_tys) {
            self.unify(x, y)?;
        }
        Ok(Type::TupleType(Box::new(ty1)))
    }

    //ラムダの単一化処理
    fn lambda_unify(&mut self, ty1: LambdaType, ty2: LambdaType) -> Result<Type, String> {
        match (ty1.env_ty.clone(), ty2.env_ty) {
            (Some(x), Some(y)) => { self.tuple_unify(x, y)?; }
            (None, None) => (),
            (None, x) | (x, None) => { return create_error(&"void", &x); }
        }
        self.fn_unify(ty1.func_ty.clone(), ty2.func_ty)?;
        Ok(Type::LambdaType(Box::new(ty1)))
    }
}