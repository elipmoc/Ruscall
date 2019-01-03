use super::typ::*;
use super::typ::{Type::*, Kind::*};
use super::types::*;
use std::collections::HashMap;

#[derive(Clone, PartialEq)]
//制約と型をセットにしたもの
//psが制約
//tがその制約がかけられたなにか
//例: ps: aはNum制約がある。 t:a->a
//説明：　Num制約がかかった型変数aがあり、型はa->aである
pub struct Qual<T> {
    ps: Vec<Pred>,
    t: T,
}

impl<T> Qual<T> {
    pub fn new(ps: Vec<Pred>, t: T) -> Qual<T> {
        Qual { t, ps }
    }
}

#[derive(Clone, PartialEq)]
//idは型クラス名。tyはその型クラスのインスタンスを表す。
//例：　id:"Num" ty:"a"
//説明：　型変数aはNum制約を持つ
pub enum Pred {
    IsIn { id: Id, ty: Type }
}

impl Pred {
    pub fn overlap(&self, other: &Self) -> bool {
        use super::unify::{mgu, lift};
        lift(mgu)(self.clone(), other.clone()).is_ok()
    }
}

//型クラスを表す
pub struct Class {
    //スーパークラス
    supers: Vec<Id>,
    //スーパークラスのインスタンス
    instances: Vec<Instance>,
}

impl Class {
    pub fn new(supers: Vec<Id>, instances: Vec<Instance>) -> Class {
        Class { supers, instances }
    }
}

#[derive(Clone)]
pub struct Instance {
    qual: Qual<Pred>,
}

impl Instance {
    pub fn new(qual: Qual<Pred>) -> Instance {
        Instance { qual }
    }

    pub fn get_qual(&self) -> &Qual<Pred> {
        &self.qual
    }
}

pub struct ClassEnv {
    classes: HashMap<Id, Class>,
    defaults: Vec<Type>,
}

impl ClassEnv {
    pub fn new() -> ClassEnv {
        ClassEnv {
            classes: HashMap::new(),
            defaults: vec![t_int()],
        }
    }
    pub fn defined(&self, id: &Id) -> bool {
        self.classes.contains_key(id)
    }
    pub fn get_super(&self, id: &Id) -> &Vec<Id> {
        &self.classes.get(id).unwrap().supers
    }
    pub fn get_instances(&self, id: &Id) -> &Vec<Instance> {
        &self.classes.get(id).unwrap().instances
    }
    pub fn modify(mut self, id: Id, class: Class) -> ClassEnv {
        self.classes.insert(id, class);
        self
    }
    pub fn add_class(self, id: Id, supers: Vec<Id>) -> Result<Self, String> {
        if self.defined(&id) {
            return Err("class already defined".to_string());
        }
        if supers.iter().any(|id| self.defined(id) == false) {
            return Err("superclass not defined".to_string());
        }
        Ok(self.modify(id, Class::new(supers, vec![])))
    }
    pub fn add_inst(self, ps: Vec<Pred>, p: Pred) -> Result<Self, String> {
        match p {
            IsIn { id, ty } => {
                let mut insts = (*self.get_instances(&id)).clone();
                if self.defined(&id) == false { return Err("no class for instance".to_string()); }
                {
                    let mut qs = insts.iter().map(|x| &x.get_qual().t);
                    if qs.any(|q| IsIn { id: id.clone(), ty: ty.clone() }.overlap(&q)) { return Err("overlapping instance".to_string()); }
                }
                insts.push(Instance::new(Qual::new(ps, IsIn { id: id.clone(), ty })));
                let c = Class::new(self.get_super(&id).clone(), insts);
                Ok(
                    self.modify(
                        id.clone(),
                        c,
                    )
                )
            }
        }
    }
    pub fn by_super(&self, p: Pred) -> Vec<Pred> {
        match p {
            IsIn { id, ty } => {
                let mut ps = self.get_super(&id).iter().map(|super_id|
                    self.by_super(IsIn { id: super_id.clone(), ty: ty.clone() })
                ).flatten().collect::<Vec<_>>();
                ps.push(IsIn { id, ty });
                ps
            }
        }
    }
    pub fn by_inst(&self, p: Pred) -> Result<Vec<Pred>, String> {
        use super::unify::{lift, matc_h};
        match p {
            IsIn { id, ty } => {
                self.get_instances(&id).iter().map(|inst| {
                    let subst = lift(matc_h)(inst.get_qual().t.clone(), IsIn { id: id.clone(), ty: ty.clone() })?;
                    Ok(inst.get_qual().ps.iter().map(|p| p.apply(&subst)).collect())
                }).find(Result::is_ok).unwrap_or(Err("error".to_string()))
            }
        }
    }
    //帰結関数
    pub fn entail(&self, ps: Vec<Pred>, p: Pred) -> bool {
        ps.clone().into_iter().map(|p| self.by_super(p)).any(|ps2| ps2.contains(&p))
            ||
            match self.by_inst(p) {
                Err(_) => false,
                Ok(qs) => qs.into_iter().all(|q| self.entail(ps.clone(), q))
            }
    }
}

use self::Pred::*;

impl<T: Types> Types for Qual<T> {
    fn apply(&self, s: &Subst) -> Self {
        Qual::new(
            self.ps.apply(s),
            self.t.apply(s),
        )
    }
    fn tv(&self) -> HashSet<&TyVar> {
        hash_set_union(self.ps.tv(), &self.t.tv())
    }
}

impl Types for Pred {
    fn apply(&self, s: &Subst) -> Self {
        match self {
            IsIn { id, ty } => IsIn { id: id.clone(), ty: ty.apply(s) }
        }
    }
    fn tv(&self) -> HashSet<&TyVar> {
        match self {
            IsIn { ty, .. } => ty.tv()
        }
    }
}

#[test]
fn example() {
    //Num a => a ->Int
    Qual::new(
        vec![IsIn {
            id: Id::with_str("Num"),
            ty: TVar(TyVar::new(Id::with_str("a"), Star)),
        }],
        create_fn(
            TVar(TyVar::new(Id::with_str("a"), Star)),
            t_int(),
        ),
    );
}