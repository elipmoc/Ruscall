use super::super::super::types::*;
use std::collections::HashMap;
use super::type_substitute::TypeSubstitute;

#[derive(Debug, PartialEq, Clone)]
pub struct TypeEnv {
    env: Vec<HashMap<String, Type>>,
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
    fn global_get(&mut self, symbol: String) -> Type {
        match self.env[0].remove(&symbol) {
            Some(x) => {
                self.env[0].insert(symbol, x.clone());
                x
            }
            _ => {
                println!("{:?}:={:?}", symbol, self.id);
                let ty = Type::TyVar(TypeId::new(self.id));
                self.env[0].insert(symbol, ty.clone());
                self.id += 1;
                ty
            }
        }
    }

    //変数名に対応した型変数を生成する
    fn get(&mut self, symbol: String) -> Type {
        match self.env[self.nest].remove(&symbol) {
            Some(x) => {
                self.env[self.nest].insert(symbol, x.clone());
                x
            }
            _ => {
                println!("{:?}:={:?}", symbol, self.id);
                let ty = Type::TyVar(TypeId::new(self.id));
                self.env[self.nest].insert(symbol, ty.clone());
                self.id += 1;
                ty
            }
        }
    }

    //無名の変数に対応した型変数を生成する
    pub fn no_name_get(&mut self) -> Type {
        Type::TyVar(self.fresh_type_id())
    }
    pub fn fresh_type_id(&mut self) -> TypeId {
        let ty_id = TypeId::new(self.id);
        println!("{{no_symbol}}:={:?}", self.id);
        self.id += 1;
        ty_id
    }
}

//型環境と型代入をひとまとめにした
#[derive(Debug, PartialEq, Clone)]
pub struct TypeInfo(TypeSubstitute);

impl TypeInfo {
    pub fn new() -> TypeInfo {
        TypeInfo(TypeSubstitute::new())
    }
    pub fn look_up(&self, ty_id: &TypeId) -> Type {
        self.0.look_up(ty_id, false)
    }
    pub fn type_look_up(&self, ty: &Type, inst_flag: bool) -> Type { self.0.type_look_up(ty, inst_flag) }

    pub fn look_up_func_name(&mut self, name: String) -> Type {
        let ty = self.0.ty_env.global_get(name);
        self.0.type_look_up(&ty, false)
    }

    pub fn last_qual(&mut self, q: Qual<Type>) -> Result<Qual<Type>, String> {
        self.0.last_qual(q)
    }

    pub fn get(&mut self, id: String) -> Type {
        self.0.ty_env.get(id)
    }

    pub fn global_get(&mut self, id: String) -> Type {
        self.0.ty_env.global_get(id)
    }

    pub fn no_name_get(&mut self) -> Type {
        self.0.ty_env.no_name_get()
    }
    pub fn fresh_type_id(&mut self) -> TypeId {
        self.0.ty_env.fresh_type_id()
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

    pub fn in_nest(&mut self) {
        self.0.ty_env.in_nest();
    }

    pub fn out_nest(&mut self) {
        self.0.ty_env.out_nest();
    }
}
