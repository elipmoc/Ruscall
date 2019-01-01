use super::typ::*;
use super::typ::{Type::*, Kind::*};
use super::types::*;

#[derive(Clone, PartialEq)]
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
pub enum Pred {
    IsIn { id: Id, ty: Type }
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