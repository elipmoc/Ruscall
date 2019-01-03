use super::typ::*;
use super::typ::Type::*;
use super::subst::Subst;
use super::types::Types;

type UnifyResult = Result<Subst, String>;

pub fn mgu(a: Type, b: Type) -> UnifyResult {
    match (a, b) {
        (TAp(a1, a2), TAp(b1, b2)) => {
            let s1 = mgu(*a1, *b1)?;
            let s2 =
                mgu(
                    a2.apply(&s1),
                    b2.apply(&s1),
                )?;
            Ok(s2.right_merge(s1))
        }
        (TVar(v), t) | (t, TVar(v)) => var_bind(v, t),
        (TCon(ref tc1), TCon(ref tc2))if tc1 == tc2 => Ok(Subst::new()),
        _ => Err("types do not unifiy".to_string())
    }
}

//match
pub fn matc_h(a: Type, b: Type) -> UnifyResult {
    match (a, b) {
        (TAp(a1, a2), TAp(b1, b2)) => {
            let s1 = matc_h(*a1, *b1)?;
            let s2 = matc_h(*a2, *b2)?;
            s1.merge(s2)
        }
        (TVar(ref v), ref t)if v.kind() == t.kind() => Ok(Subst::one_with(v.clone(), t.clone())),
        (TCon(ref tc1), TCon(ref tc2))if tc1 == tc2 => Ok(Subst::new()),
        _ => Err("types do not match".to_string())
    }
}

use super::traits::Pred;

pub fn lift<F: Fn(Type, Type) -> UnifyResult>(f: F) -> impl Fn(Pred, Pred) -> UnifyResult {
    use super::traits::Pred::*;
    move |a: Pred, b: Pred| {
        match (a, b) {
            (IsIn { id: id1, ty: ty1 }, IsIn { id: id2, ty: ty2 }) => {
                if id1 == id2 {
                    f(ty1, ty2)
                } else {
                    Err("classes differ".to_string())
                }
            }
        }
    }
}

fn var_bind(v: TyVar, t: Type) -> UnifyResult {
    if t == TVar(v.clone()) {
        return Ok(Subst::new());
    }
    if t.tv().contains(&v) {
        return Err("occures check fails".to_string());
    }
    if v.kind() != t.kind() {
        return Err("kind do not match".to_string());
    }
    Ok(Subst::one_with(v, t))
}