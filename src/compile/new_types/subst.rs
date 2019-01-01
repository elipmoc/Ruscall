use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use super::typ::{TyVar, Type};
use super::typ::Type::*;
use super::types::*;

//型代入の写像
pub struct Subst(HashMap<TyVar, Type>);

impl Subst {
    //空の型代入写像の作成
    pub fn new() -> Subst {
        Subst(HashMap::new())
    }

    //一つだけ対応を持つ型代入写像の作成
    pub fn one_with(v: TyVar, t: Type) -> Subst {
        let mut h = HashMap::new();
        h.insert(v, t);
        Subst(h)
    }

    pub fn get(&self, v: &TyVar) -> Option<&Type> {
        self.0.get(v)
    }

    //左の型代入優先のマージ
    pub fn left_merge(self, right: Self) -> Self {
        let h: HashMap<_, _> = right.0.into_iter().map(|(v, t)| {
            (v, t.apply(&self))
        }).collect();
        Subst(h.into_iter().chain(self.0.into_iter()).collect())
    }

    //両方の型代入が一致するようにマージ（一致しなければErr）
    pub fn merge(self, other: Self) -> Result<Self, String> {
        let agree =
            self.0.iter().all(|(v, _)| {
                match other.get(v) {
                    None => true,
                    Some(_) => TVar(v.clone()).apply(&self) == TVar(v.clone()).apply(&other)
                }
            });
        if agree {
            Ok(Subst(self.0.into_iter().chain(other.0.into_iter()).collect()))
        } else { Err("merge error!".to_string()) }
    }
}