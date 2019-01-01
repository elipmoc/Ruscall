pub use std::collections::HashSet;
pub use super::subst::Subst;
pub use super::typ::TyVar;
use std::hash::Hash;

//一つの要素だけをもつHashSetを生成するヘルパ関数
pub fn one_hash_set<T: Eq + Hash>(v: T) -> HashSet<T> {
    let mut h = HashSet::new();
    h.insert(v);
    h
}

//二つのhash_setのunionを取るヘルパ関数
pub fn hash_set_union<'f, T: Eq + Hash>(a: HashSet<&'f T>, b: &HashSet<&'f T>) -> HashSet<&'f T> {
    a.union(b).map(|x| *x).collect()
}

//型代入の操作と、持っている型変数を列挙する機能を持つトレイト
pub trait Types {
    fn apply(&self, &Subst) -> Self;
    fn tv(&self) -> HashSet<&TyVar>;
}

impl<T: Types> Types for Vec<T> {
    fn apply(&self, s: &Subst) -> Self {
        self.iter().map(|x| x.apply(s)).collect()
    }
    fn tv(&self) -> HashSet<&TyVar> {
        self.iter().map(|x| x.tv())
            .fold(HashSet::new(), |acc, x| hash_set_union(acc, &x))
    }
}