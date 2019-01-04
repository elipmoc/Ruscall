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
    IsIn { class_name: Id, ty: Type }
}

impl Pred {
    pub fn overlap(&self, other: &Self) -> bool {
        use super::unify::{mgu, lift};
        lift(mgu)(self.clone(), other.clone()).is_ok()
    }
    //述語がhead-normal-formかどうかを判定する
    //例えば、Eq[a]などはEq aに置き換えられるのでfalse
    pub fn in_hnf(&self) -> bool {
        fn hnf(ty: &Type) -> bool {
            match ty {
                TVar(_) => true,
                TCon(_) => false,
                TAp(t, _) => hnf(&t),
                TGen(_) => panic!("undefined")
            }
        }
        match self {
            IsIn { ty, .. } => hnf(ty)
        }
    }
}

//型クラスのスーパークラスとインスタンスを表す
//型クラス名はClassEnvのKeyで保存する。
pub struct Class {
    //スーパークラス
    supers: Vec<Id>,
    //インスタンス
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
    //指定された型クラスのスーパークラス一覧を返す
    pub fn get_super(&self, id: &Id) -> &Vec<Id> {
        &self.classes.get(id).unwrap().supers
    }
    //指定された型クラスのインスタンス一覧を返す
    pub fn get_instances(&self, id: &Id) -> &Vec<Instance> {
        &self.classes.get(id).unwrap().instances
    }
    //指定された型クラスの情報を上書きする
    pub fn modify(mut self, id: Id, class: Class) -> ClassEnv {
        self.classes.insert(id, class);
        self
    }
    //型クラスを新たに追加する。（インスタンスは空）
    pub fn add_class(self, id: Id, supers: Vec<Id>) -> Result<Self, String> {
        if self.defined(&id) {
            return Err("class already defined".to_string());
        }
        if supers.iter().any(|id| self.defined(id) == false) {
            return Err("superclass not defined".to_string());
        }
        Ok(self.modify(id, Class::new(supers, vec![])))
    }
    //追加した型クラスにインスタンスを追加する
    pub fn add_inst(self, ps: Vec<Pred>, p: Pred) -> Result<Self, String> {
        match p {
            IsIn { class_name, ty } => {
                let mut insts = (*self.get_instances(&class_name)).clone();
                if self.defined(&class_name) == false { return Err("no class for instance".to_string()); }
                {
                    let mut qs = insts.iter().map(|x| &x.get_qual().t);
                    if qs.any(|q| IsIn { class_name: class_name.clone(), ty: ty.clone() }.overlap(&q)) { return Err("overlapping instance".to_string()); }
                }
                insts.push(Instance::new(Qual::new(ps, IsIn { class_name: class_name.clone(), ty })));
                let c = Class::new(self.get_super(&class_name).clone(), insts);
                Ok(
                    self.modify(
                        class_name.clone(),
                        c,
                    )
                )
            }
        }
    }
    //述語　X =>aに対して、Xのスーパークラス(X1,X1..Xn)をすべて列挙し、
    //[X=>a,X1=>a,X2=>a..Xn=>a]の述語のリストを作り出す（当然これらはすべて満たされるはずである）
    pub fn by_super(&self, p: Pred) -> Vec<Pred> {
        match p {
            IsIn { class_name, ty } => {
                let mut ps = self.get_super(&class_name).iter().map(|super_id|
                    self.by_super(IsIn { class_name: super_id.clone(), ty: ty.clone() })
                ).flatten().collect::<Vec<_>>();
                ps.push(IsIn { class_name, ty });
                ps
            }
        }
    }
    //述語　X =>aに対して、Xのインスタンス(I1,I1..In)をすべて列挙し、
    //インスタンスの述語部分とX =>a がマッチする
    pub fn by_inst(&self, p: Pred) -> Option<Vec<Pred>> {
        use super::unify::{lift, matc_h};
        match p {
            IsIn { class_name: id, ty } => {
                self.get_instances(&id).iter().map(|inst| {
                    let subst = lift(matc_h)(inst.get_qual().t.clone(), IsIn { class_name: id.clone(), ty: ty.clone() })?;
                    Ok(inst.get_qual().ps.iter().map(|p| p.apply(&subst)).collect())
                }).find_map(|ps_result: Result<_, String>|
                    match ps_result {
                        Err(_) => None,
                        Ok(ps) => Some(ps)
                    }
                )
            }
        }
    }
    //帰結関数
    pub fn entail(&self, ps: Vec<Pred>, p: Pred) -> bool {
        ps.clone().into_iter().map(|p| self.by_super(p)).any(|ps2| ps2.contains(&p))
            ||
            match self.by_inst(p) {
                None => false,
                Some(qs) => qs.into_iter().all(|q| self.entail(ps.clone(), q))
            }
    }
    //述語をhead-normal-formになるまで文脈をそぎ落とす
    pub fn to_hnf(&self, p: Pred) -> Result<Vec<Pred>, String> {
        if p.in_hnf()
            {
                Ok(vec![p])
            } else {
            match self.by_inst(p) {
                None => Err("context reduction".to_string()),
                Some(ps) => self.to_hnfs(ps)
            }
        }
    }
    pub fn to_hnfs(&self, ps: Vec<Pred>) -> Result<Vec<Pred>, String> {
        let pss = ps.into_iter().map(|p| self.to_hnf(p)).collect::<Result<Vec<_>, _>>()?;
        Ok(pss.into_iter().flatten().collect())
    }
    pub fn simplify(&self, ps: Vec<Pred>) -> Vec<Pred> {
        return looop(self, vec![], ps);
        fn looop(ce: &ClassEnv, mut rs: Vec<Pred>, mut ps: Vec<Pred>) -> Vec<Pred> {
            if ps.len() == 0 { rs } else {
                let p = ps.pop().unwrap();
                if ce.entail(merge_clone(&rs, &ps), p.clone()) { looop(ce, rs, ps) } else {
                    rs.insert(0, p);
                    looop(ce, rs, ps)
                }
            }
        }
        fn merge_clone(a: &Vec<Pred>, b: &Vec<Pred>) -> Vec<Pred> {
            let mut a = a.clone();
            a.append(&mut b.clone());
            a
        }
    }
    //文脈の削除
    pub fn reduce(&self, ps: Vec<Pred>) -> Result<Vec<Pred>, String> {
        let qs = self.to_hnfs(ps)?;
        Ok(self.simplify(qs))
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
            IsIn { class_name: id, ty } => IsIn { class_name: id.clone(), ty: ty.apply(s) }
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
            class_name: Id::with_str("Num"),
            ty: TVar(TyVar::new(Id::with_str("a"), Star)),
        }],
        create_fn(
            TVar(TyVar::new(Id::with_str("a"), Star)),
            t_int(),
        ),
    );
}