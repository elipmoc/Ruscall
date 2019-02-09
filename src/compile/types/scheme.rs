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
    //量化しないで型スキームに
    pub fn to_scheme(q: Qual<Type>) -> Self {
        Scheme::Forall { qual: q, tgen_count: 0 }
    }

    //量化する
    pub fn quantify(q: Qual<Type>, ty_info: &mut TypeInfo) -> Result<Self, String> {
        let mut q = ty_info.last_qual(q.clone())?;
        use super::super::semantic_analysis::type_inference::type_substitute::TypeSubstitute;
        let mut ty_sub = TypeSubstitute::new();
        let mut n: usize = 0;
        for ty_id in q.tv_list() {
            ty_sub.ty_sub.insert(ty_id, Type::TGen(n, ty_id));
            n += 1;
        };
        q = q.apply(&ty_sub, false);
        Ok(Scheme::Forall { qual: q, tgen_count: n })
    }
    //型スキームのTGenをフレッシュな型変数に置き換えたQualを生成
    pub fn fresh_inst(self, ty_info: &mut TypeInfo) -> Qual<Type> {
        match self {
            Scheme::Forall { qual, tgen_count } => {
                let fresh_types = (0..tgen_count).map(|_| ty_info.no_name_get()).collect::<Vec<_>>();
                qual.inst(&fresh_types)
            }
        }
    }
}