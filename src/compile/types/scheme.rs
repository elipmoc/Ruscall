use super::*;

#[derive(Clone, PartialEq, Debug)]
//型スキーム
pub enum Scheme {
    Forall { qual: Qual<Type>, tgen_count: usize }
}

use super::super::semantic_analysis::type_inference::type_env::*;

impl Scheme {
    pub fn get_qual(&self) -> &Qual<Type> {
        match self {
            Scheme::Forall { qual, .. } => &qual
        }
    }

    //量化する
    pub fn quantify(q: Qual<Type>, ty_info: &mut TypeInfo) -> Result<Self, String> {
        let mut n: usize = 0;
        for ty_id in q.t.tv_list() {
            ty_info.unify(Type::TyVar(ty_id), Type::TGen(n))?;
            n += 1;
        };
        Ok(Scheme::Forall { qual: ty_info.last_qual(q)?, tgen_count: n })
    }
    //型スキームのTGenをフレッシュな型変数に置き換えたQualを生成
    pub fn fresh_inst(self, ty_info: &mut TypeInfo) -> Qual<Type> {
        match self {
            Scheme::Forall { mut qual, tgen_count } => {
                let fresh_types = (0..tgen_count).map(|_| Type::TyVar(ty_info.no_name_get())).collect::<Vec<_>>();
                qual.t = qual.t.inst(&fresh_types);
                qual
            }
        }
    }
}