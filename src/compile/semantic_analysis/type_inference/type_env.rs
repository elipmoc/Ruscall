use super::super::super::types::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct TypeEnv {
    env: Vec<HashMap<String, Type>>,
    id: usize,
    nest: usize ,
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
    pub fn in_nest(&mut self) {
        self.nest += 1;
        self.env.push(HashMap::new());
    }

    //環境のネストを抜ける
    pub fn out_nest(&mut self) {
        self.nest -= 1;
        self.env.pop();
    }

    //変数名に対応した型変数を生成する
    pub fn global_get(&mut self, symbol: String) -> Type {
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
    pub fn get(&mut self, symbol: String) -> Type {
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