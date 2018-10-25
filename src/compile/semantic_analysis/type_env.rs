use super::super::types::*;
use std::collections::HashMap;

#[derive(Debug)]
struct TypeEnv {
    env: HashMap<String, usize>,
    id: usize,
}

//型環境
impl TypeEnv {
    fn new() -> TypeEnv {
        TypeEnv {
            env: HashMap::new(),
            id: 0,
        }
    }

    //変数名に対応した型変数を生成する
    fn get(mut self, symbol: String) -> (usize, TypeEnv) {
        match self.env.remove(&symbol) {
            Some(x) => {
                self.env.insert(symbol, x.clone());
                (x, self)
            }
            _ => {
                self.env.insert(symbol, self.id);
                self.id += 1;
                (self.id - 1, self)
            }
        }
    }

    //変数に対応しない型変数を生成する
    fn no_name_get(mut self) -> (usize, TypeEnv) {
        self.id += 1;
        (self.id - 1, self)
    }

    //変数名と型変数との対応を取り除く
    fn remove(mut self, symbol: &String) -> TypeEnv {
        self.env.remove(symbol);
        self
    }
}

//型代入環境
#[derive(Debug)]
struct TypeSubstitute(pub HashMap<usize, Type>);

impl TypeSubstitute {
    fn new() -> TypeSubstitute {
        TypeSubstitute(HashMap::new())
    }

    fn insert(mut self, id: usize, ty: Type) -> Result<TypeSubstitute, String> {
        match self.0.remove(&id) {
            Some(ty2) => {
                let mut ty_subst = self.unify(ty, ty2.clone())?;
                ty_subst.0.insert(id, ty2);
                Ok(ty_subst)
            }
            None => {
                self.0.insert(id, ty);
                Ok(self)
            }
        }
    }

    fn unify(self, ty1: Type, ty2: Type) -> Result<TypeSubstitute, String> {
        match (ty1, ty2) {
            (ref ty1, ref ty2) if ty1 == ty2 => Ok(self),
            (Type::TyVar(id), ty) => self.insert(id.clone(), ty),
            (ty, Type::TyVar(id)) => self.insert(id.clone(), ty),
            (Type::FuncType(ty1), Type::FuncType(ty2)) => self.fn_unify(*ty1, *ty2),
            (Type::TupleType(ty1), Type::TupleType(ty2)) => self.tuple_unify(*ty1, *ty2),
            (ty1, ty2) => {
                return Err(format!(
                    "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                    ty1, ty2
                ));
            }
        }
    }

    fn fn_unify(mut self, ty1: FuncType, ty2: FuncType) -> Result<TypeSubstitute, String> {
        for (x, y) in ty1.param_types.into_iter().zip(ty2.param_types) {
            self = self.unify(x, y)?;
        }
        self.unify(ty1.ret_type, ty2.ret_type)
    }

    fn tuple_unify(mut self, ty1: TupleType, ty2: TupleType) -> Result<TypeSubstitute, String> {
        for (x, y) in ty1.element_tys.into_iter().zip(ty2.element_tys) {
            self = self.unify(x, y)?;
        }
        Ok(self)
    }

    // 型変数に対応する単相型を見つけて返す。見つからなかったら空タプルの型を返す
    fn look_up(&self, id: &usize) -> Type {
        match self.0.get(&id) {
            Some(ty) => self.type_look_up(ty),
            None => Type::TupleType(Box::new(TupleType {
                element_tys: vec![],
            })),
        }
    }

    fn type_look_up(&self, ty: &Type) -> Type {
        match ty {
            Type::TyVar(id) => self.look_up(id),
            Type::FuncType(x) => self.func_look_up(x),
            Type::TupleType(x) => self.tuple_look_up(x),
            ty => ty.clone(),
        }
    }
    fn func_look_up(&self, ty: &FuncType) -> Type {
        Type::create_func_type(
            ty.param_types
                .iter()
                .map(|ty| self.type_look_up(ty))
                .collect(),
            self.type_look_up(&ty.ret_type),
        )
    }
    fn tuple_look_up(&self, ty: &TupleType) -> Type {
        Type::create_tuple_type(
            ty.element_tys
                .iter()
                .map(|ty| self.type_look_up(ty))
                .collect(),
        )
    }
}

//型を解決した結果を持つ
#[derive(Debug)]
pub struct TypeResolved(HashMap<String, Type>);

impl TypeResolved {
    fn new(ty_env: TypeEnv, ty_subst: TypeSubstitute) -> TypeResolved {
        TypeResolved(
            ty_env
                .env
                .into_iter()
                .map(|(k, v)| (k, ty_subst.look_up(&v)))
                .collect(),
        )
    }

    //型変数に対応する単相型を返す
    pub fn get(&self, id: String) -> Type {
        self.0[&id].clone()
    }
}

//型環境と型代入をひとまとめにした
pub struct TypeInfo(TypeEnv, TypeSubstitute);

impl TypeInfo {
    pub fn new() -> TypeInfo {
        TypeInfo(TypeEnv::new(), TypeSubstitute::new())
    }

    pub fn get(self, id: String) -> (usize, TypeInfo) {
        let (id, ty_env) = self.0.get(id);
        (id, TypeInfo(ty_env, self.1))
    }

    pub fn no_name_get(self) -> (usize, TypeInfo) {
        let (id, ty_env) = self.0.no_name_get();
        (id, TypeInfo(ty_env, self.1))
    }

    pub fn unify(mut self, ty1: Type, ty2: Type) -> Result<TypeInfo, String> {
        self.1 = self.1.unify(ty1, ty2)?;
        Ok(self)
    }

    pub fn get_type_resolved(self) -> TypeResolved {
        TypeResolved::new(self.0, self.1)
    }

    pub fn remove(mut self, symbol: &String) -> TypeInfo {
        self.0 = self.0.remove(symbol);
        self
    }
}
