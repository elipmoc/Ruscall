use super::super::super::types::*;
use std::collections::HashMap;
use super::type_substitute::TypeSubstitute;

#[derive(Debug, PartialEq)]
pub struct TypeEnv {
    env: Vec<HashMap<String, TypeId>>,
    id: usize,
    nest: usize,
}

//型環境
impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv {
            env: vec![HashMap::new()],
            id: 0,
            nest: 0,
        }
    }

    //環境をネスト
    fn in_nest(&mut self) {
        self.nest += 1;
        self.env.push(HashMap::new());
    }

    //環境のネストを抜ける
    fn out_nest(&mut self) {
        self.nest -= 1;
        self.env.pop();
    }

    //変数名に対応した型変数を生成する
    fn global_get(&mut self, symbol: String) -> TypeId {
        match self.env[0].remove(&symbol) {
            Some(x) => {
                self.env[0].insert(symbol, x.clone());
                x
            }
            _ => {
                println!("{:?}:={:?}", symbol, self.id);
                self.env[0].insert(symbol, TypeId::new(self.id));
                self.id += 1;
                TypeId::new(self.id - 1)
            }
        }
    }

    //変数名に対応した型変数を生成する
    fn get(&mut self, symbol: String) -> TypeId {
        match self.env[self.nest].remove(&symbol) {
            Some(x) => {
                self.env[self.nest].insert(symbol, x.clone());
                x
            }
            _ => {
                println!("{:?}:={:?}", symbol, self.id);
                self.env[self.nest].insert(symbol, TypeId::new(self.id));
                self.id += 1;
                TypeId::new(self.id - 1)
            }
        }
    }

    //無名の変数に対応した型変数を生成する
    pub fn no_name_get(&mut self) -> TypeId {
        self.id += 1;
        println!("{{no_symbol}}:={:?}", self.id - 1);
        TypeId::new(self.id - 1)
    }
}

//型を解決した結果を持つ
pub struct TypeResolved(HashMap<String, Type>);

impl TypeResolved {
    fn new(ty_env: &TypeEnv, ty_subst: &TypeSubstitute) -> TypeResolved {
        TypeResolved(
            ty_env
                .env[0]
                .iter()
                .map(|(k, v)| (k.clone(), ty_subst.look_up(&v)))
                .collect(),
        )
    }
}

use std::fmt;

impl fmt::Debug for TypeResolved
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use compile::types::show_type::ShowType;
        write!(f, "{}", self.0.iter().fold("".to_string(), |acc, x|
            acc + x.0 + "\n=>" + &(x.1.show()) + "\n\n",
        ))
    }
}

//型環境と型代入をひとまとめにした
#[derive(Debug, PartialEq)]
pub struct TypeInfo(TypeSubstitute);

impl TypeInfo {
    pub fn new() -> TypeInfo {
        TypeInfo(TypeSubstitute::new())
    }

    pub fn look_up(&self, ty_id: &TypeId) -> Type {
        self.0.look_up(ty_id)
    }
    pub fn type_look_up(&self, ty: &Type) -> Type { self.0.type_look_up(ty) }

    pub fn look_up_func_name(&mut self, name: String) -> Type {
        let ty_id = self.0.ty_env.global_get(name);
        self.0.look_up(&ty_id)
    }

    pub fn last_qual(&mut self, q: Qual<Type>) -> Result<Qual<Type>, String> {
        self.0.last_qual(q)
    }

    pub fn get(&mut self, id: String) -> TypeId {
        self.0.ty_env.get(id)
    }

    pub fn global_get(&mut self, id: String) -> TypeId {
        self.0.ty_env.global_get(id)
    }

    pub fn no_name_get(&mut self) -> TypeId {
        self.0.ty_env.no_name_get()
    }

    pub fn unify(&mut self, ty1: Type, ty2: Type) -> Result<Type, String> {
        self.0.unify(ty1, ty2)
    }

    pub fn qual_unify(&mut self, q1: Qual<Type>, q2: Qual<Type>) -> Result<Qual<Type>, String> {
        let q = self.0.qual_unify(q1, q2)?;
        Ok(q)
    }

    pub fn qual_condition_add_unify(&mut self, q: Qual<Type>, c: Condition) -> Result<Qual<Type>, String> {
        let q = self.0.qual_add_condition_unify(q, c)?;
        Ok(q)
    }

    pub fn preds_merge_unify(&mut self, ps1: Preds, ps2: Preds) -> Result<Preds, String> {
        let ps = self.0.preds_merge_unify(ps1, ps2)?;
        Ok(ps)
    }
    pub fn predss_merge_unify(&mut self, pss: Vec<Preds>) -> Result<Preds, String> {
        let ps = pss.into_iter()
            .fold(Ok(Preds::new()), |acc: Result<_, String>, ps2| {
                let ps1 = acc?;
                let ps = self.preds_merge_unify(ps1, ps2)?;
                Ok(ps)
            })?;
        Ok(ps)
    }


    pub fn get_type_resolved(&self) -> TypeResolved {
        TypeResolved::new(&self.0.ty_env, &self.0)
    }

    pub fn in_nest(&mut self) {
        self.0.ty_env.in_nest();
    }

    pub fn out_nest(&mut self) {
        self.0.ty_env.out_nest();
    }
}
