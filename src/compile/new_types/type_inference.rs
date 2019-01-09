use super::subst::Subst;
use super::unify::mgu;
use super::typ::*;
use super::types::Types;
use super::traits::*;

//型推論をするための構造体
pub struct Infer {
    subst: Subst,
    var_num: u32,
}

use std::ops::Drop;

impl Infer {
    fn unify(mut self, t1: Type, t2: Type) -> Result<Self, String> {
        let subst = self.subst;
        self.subst = mgu(t1.apply(&subst), t2.apply(&subst))?;
        Ok(self)
    }
    fn new_tvar(&mut self, k: Kind) -> Type {
        let id = Id::enum_id(self.var_num);
        let t_var = TyVar::new(id, k);
        self.var_num += 1;
        Type::TVar(t_var)
    }
    fn fresh_inst(&mut self, scheme: Scheme)->Qual<Type> {
        match scheme {
            Scheme::Forall { kinds, qual } => {
                let ts = kinds.into_iter().map(|k| self.new_tvar(k));
                qual.inst(&ts.collect())
            }
        }
    }
}

trait Instantiate {
    fn inst(self, &Vec<Type>) -> Self;
}

impl Instantiate for Type {
    fn inst(self, ts: &Vec<Type>) -> Self {
        use super::typ::Type::*;
        match self {
            TAp(a, b) => TAp(Box::new(a.inst(ts)), Box::new(b.inst(ts))),
            TGen(n) => ts[n as usize].clone(),
            t => t
        }
    }
}

impl<A: Instantiate> Instantiate for Vec<A> {
    fn inst(self, ts: &Vec<Type>) -> Self {
        self.into_iter().map(|a| a.inst(ts)).collect()
    }
}

impl<T: Instantiate> Instantiate for Qual<T> {
    fn inst(self, ts: &Vec<Type>) -> Self {
        Qual::new(self.ps.inst(ts), self.t.inst(ts))
    }
}

impl Instantiate for Pred {
    fn inst(self, ts: &Vec<Type>) -> Self {
        use super::traits::Pred::*;
        match self {
            IsIn { class_name, ty } => {
                IsIn { class_name, ty: ty.inst(ts) }
            }
        }
    }
}
