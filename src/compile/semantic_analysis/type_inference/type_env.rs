use super::super::super::types::types::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub(in super) struct TypeEnv {
    env: Vec<HashMap<String, TypeId>>,
    id: usize,
    nest: usize,
}

//型環境
impl TypeEnv {
    fn new() -> TypeEnv {
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

//型代入環境
#[derive(Debug, PartialEq)]
pub(in super) struct TypeSubstitute(pub HashMap<TypeId, Type>);

impl TypeSubstitute {
    fn new() -> Self {
        TypeSubstitute(HashMap::new())
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
pub struct TypeInfo(TypeEnv, TypeSubstitute);

impl TypeInfo {
    pub fn new() -> TypeInfo {
        TypeInfo(TypeEnv::new(), TypeSubstitute::new())
    }

    pub fn look_up(&self, ty_id: &TypeId) -> Type {
        self.1.look_up(ty_id)
    }

    pub fn look_up_func_name(&mut self, name: String) -> Type {
        self.1.look_up(&self.0.global_get(name))
    }

    pub fn get(&mut self, id: String) -> TypeId {
        self.0.get(id)
    }

    pub fn global_get(&mut self, id: String) -> TypeId {
        self.0.global_get(id)
    }

    pub fn no_name_get(&mut self) -> TypeId {
        self.0.no_name_get()
    }

    pub fn unify(mut self, ty1: Type, ty2: Type) -> Result<TypeInfo, String> {
        let (ty_sub, _, ty_env) = self.1.start_unify(self.0, ty1, ty2)?;
        self.1 = ty_sub;
        self.0 = ty_env;
        Ok(self)
    }

    pub fn get_type_resolved(&self) -> TypeResolved {
        TypeResolved::new(&self.0, &self.1)
    }

    pub fn in_nest(&mut self) {
        self.0.in_nest();
    }

    pub fn out_nest(&mut self) {
        self.0.out_nest();
    }
}
