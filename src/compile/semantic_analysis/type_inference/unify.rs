use super::super::super::types::*;
use super::type_substitute::TypeSubstitute;
use super::occurs_check::occurs_check;
use std::fmt::Debug;

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

use std::collections::HashSet;

impl TypeSubstitute {
    //単一化処理
    pub fn unify(&mut self, ty1: Type, ty2: Type) -> Result<Type, String> {
        let (ty1, ty2) = (self.type_look_up(&ty1, false), self.type_look_up(&ty2, true));
        if ty1 == ty2 { return Ok(ty1); }
        match (ty1, ty2) {
            (Type::TGen(_, _), Type::TGen(_, _)) => panic!("hoge"),
            (Type::TGen(_, ty_id), ty) | (ty, Type::TGen(_, ty_id)) => self.unify(ty, Type::TyVar(ty_id)),
            (Type::TyVar(id), ty) | (ty, Type::TyVar(id)) => {
                self.tvar_insert(id.clone(), ty.clone());
                Ok(ty)
            }
            (Type::TApp(ty1), Type::TApp(ty2)) => Ok(Type::TApp(Box::new(self.fn_unify(*ty1, *ty2)?))),
            (Type::TupleType(ty1), Type::TupleType(ty2)) => Ok(Type::TupleType(Box::new(self.tuple_unify(*ty1, *ty2)?))),
            (Type::StructType(ty1), Type::StructType(ty2)) => self.struct_unify(*ty1, *ty2),
            (ty1, ty2) => create_error(&ty1, &ty2)
        }
    }

    //型変数に型を代入する
    fn tvar_insert(&mut self, ty_id: TypeId, insert_ty: Type) {
        if occurs_check(&self.ty_sub, &insert_ty, &ty_id) == false {
            println!("{:?}=>{:?}", ty_id, insert_ty);
            self.ty_sub.insert(ty_id, insert_ty);
        } else {
            panic!("occurs");
        }
    }

    pub fn qual_unify(&mut self, q1: Qual<Type>, q2: Qual<Type>) -> Result<Qual<Type>, String> {
        let t = self.unify(q1.t, q2.t)?;
        let ps = self.preds_merge_unify(q1.ps, q2.ps)?;
        let ps = self.preds_reduction(ps)?;
        Ok(Qual { ps, t })
    }

    pub fn preds_reduction(&mut self, ps: Preds) -> Result<Preds, String> {
        let mut new_ps = Preds::new();
        for (_, p) in ps.into_iter() {
            let p = p.apply(&self, true);
            match new_ps.remove(&p.ty) {
                Some(p2) => {
                    let p = self.pred_unify(p2, p)?;
                    new_ps.insert(p.ty.clone(), p);
                }
                None => {
                    let ps = self.qual_add_condition_unify(Qual::new(p.ty), p.cond)?.ps;
                    new_ps = self.preds_merge_unify(new_ps, ps)?;
                }
            }
        };
        if new_ps == new_ps.clone().apply(&self, true) {
            Ok(new_ps)
        } else {
            self.preds_reduction(new_ps)
        }
    }

    pub fn preds_merge_unify(&mut self, ps1: Preds, mut ps2: Preds) -> Result<Preds, String> {
        let mut new_ps = Preds::new();
        for (k, p1) in ps1.into_iter() {
            match ps2.remove(&k) {
                Some(p2) => {
                    let p = self.pred_unify(p1, p2)?;
                    new_ps.insert(p.ty.clone(), p);
                }
                None => { new_ps.insert(p1.ty.clone(), p1); }
            };
        }
        ps2.into_iter().for_each(|(_, p)| { new_ps.insert(p.ty.clone(), p); });
        Ok(new_ps)
    }

    pub fn predss_merge_unify(&mut self, pss: Vec<Preds>) -> Result<Preds, String> {
        let ps = pss.into_iter()
            .fold(Ok(Preds::new()), |acc: Result<_, String>, ps2| {
                let ps1 = acc?;
                self.preds_merge_unify(ps1, ps2)
            })?;
        Ok(ps)
    }

    //不要なpredを取り除く
    pub fn preds_simply(&self, ps: Preds, tv_list: HashSet<TypeId>) -> Preds {
        Preds(
            ps.into_iter().filter(|(t, _)| {
                match t {
                    Type::TyVar(ty_id) => tv_list.contains(&ty_id),
                    Type::TGen(_, _) => true,
                    Type::TCon { .. } | Type::TupleType(_) | Type::TApp(_) | Type::StructType(_) => false
                }
            }).collect()
        )
    }

    //Qualに新たに制約を追加する操作と単一化処理
    pub fn qual_add_condition_unify(&mut self, mut q: Qual<Type>, c: Condition) -> Result<Qual<Type>, String> {
        use self::Type::*;
        match &q.t {
            t @ TCon { .. } => { return create_error(&t, &c); }
            TyVar(x) => {
                let p = q.ps.remove(&TyVar(x.clone()));
                match p {
                    Some(p) => {
                        let p = self.pred_unify(p, Pred { ty: TyVar(x.clone()), cond: c })?;
                        q.ps.insert(p.ty.clone(), p)
                    }
                    None => q.ps.insert(TyVar(x.clone()), Pred { ty: TyVar(x.clone()), cond: c })
                };
            }
            tgen @ TGen(_, _) => {
                match q.ps.remove(&tgen) {
                    Some(p) => {
                        let p = self.pred_unify(p, Pred { ty: tgen.clone(), cond: c })?;
                        q.ps.insert(tgen.clone(), p)
                    }
                    None => q.ps.insert(tgen.clone(), Pred { ty: tgen.clone(), cond: c })
                };
            }
            TApp(x) => {
                match c {
                    Condition::Call(c) => {
                        let mut func_ty = x.clone();
                        //func_ty.param_types = func_ty.param_types.clone().into_iter().collect::<Vec<_>>();
                        self.fn_unify(*func_ty, *c)?;
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
                Ok(Pred { cond: Condition::Call(Box::new(new_fn_ty)), ty: p1.ty })
            }
            (cond, Empty) | (Empty, cond) => Ok(Pred { ty: p1.ty, cond }),
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
    fn tuple_condition_unify<T: TupleTypeBase + Debug>(&mut self, tuple_ty: T, c: Condition) -> Result<T, String> {
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
    fn fn_unify(&mut self, ty1: TApp, ty2: TApp) -> Result<TApp, String> {
        Ok(TApp(self.unify(ty1.0, ty2.0)?, self.unify(ty1.1, ty2.1)?))
    }

    //タプルの単一化処理
    fn tuple_unify<T: TupleTypeBase + Debug>(&mut self, ty1: T, ty2: T) -> Result<T, String> {
        if ty1.get_elements_len() != ty2.get_elements_len() {
            return create_error(&ty1, &ty2);
        }
        for idx in 0..ty1.get_elements_len() {
            self.unify(ty1.get_elements_at(idx).clone(), ty2.get_elements_at(idx).clone())?;
        }
        Ok(ty1)
    }

    //構造体の単一化処理
    fn struct_unify(&mut self, ty1: StructType, ty2: StructType) -> Result<Type, String> {
        if ty1.name != ty2.name {
            return create_error(&ty1, &ty2);
        }
        self.tuple_unify(ty1.ty.clone(), ty2.ty.clone())?;
        Ok(Type::StructType(Box::new(ty1)))
    }
}