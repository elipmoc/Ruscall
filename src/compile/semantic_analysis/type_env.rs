use super::super::types::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
struct TypeEnv {
    env: HashMap<String, TypeId>,
    id: usize,
    nest: usize,
}

//型環境
impl TypeEnv {
    fn new() -> TypeEnv {
        TypeEnv {
            env: HashMap::new(),
            id: 0,
            nest: 0,
        }
    }

    fn in_nest(&mut self) {
        self.nest += 1;
    }

    fn out_nest(&mut self) {
        self.nest -= 1;
    }

    //変数名に対応した型変数を生成する
    fn global_get(&mut self, symbol: String) -> TypeId {
        match self.env.remove(&symbol) {
            Some(x) => {
                self.env.insert(symbol, x.clone());
                x
            }
            _ => {
                self.env.insert(symbol, TypeId::new(self.id));
                self.id += 1;
                TypeId::new(self.id - 1)
            }
        }
    }

    //変数名に対応した型変数を生成する
    fn get(&mut self, symbol: String) -> TypeId {
        let symbol = if self.nest != 0 { self.nest.to_string() + "&" + &symbol } else { symbol };
        match self.env.remove(&symbol) {
            Some(x) => {
                self.env.insert(symbol, x.clone());
                x
            }
            _ => {
                self.env.insert(symbol, TypeId::new(self.id));
                self.id += 1;
                TypeId::new(self.id - 1)
            }
        }
    }

    //無名の変数に対応した型変数を生成する
    fn no_name_get(&mut self) -> TypeId {
        self.id += 1;
        TypeId::new(self.id - 1)
    }

    //変数名と型変数との対応を取り除く
    fn remove(&mut self, symbol: &String) {
        let symbol = if self.nest != 0 { self.nest.to_string() + "&" + &symbol } else { symbol.clone() };
        self.env.remove(&symbol);
    }
}

//型代入環境
#[derive(Debug, PartialEq)]
struct TypeSubstitute(pub HashMap<TypeId, Type>);

impl TypeSubstitute {
    fn new() -> TypeSubstitute {
        TypeSubstitute(HashMap::new())
    }

    fn insert(mut self, ty_id: TypeId, ty: Type) -> Result<TypeSubstitute, String> {
        match self.0.remove(&ty_id) {
            Some(ty2) => {
                self.0.insert(ty_id, ty2.clone());
                self.unify(ty, ty2.clone())
            }
            None => {
                self.0.insert(ty_id, ty);
                Ok(self)
            }
        }
    }

    fn unify(self, ty1: Type, ty2: Type) -> Result<TypeSubstitute, String> {
        match (ty1, ty2) {
            (Type::TyVar(id, conds), ty) => self.ty_var_unify(id, conds, ty),
            (ty, Type::TyVar(id, conds)) => self.ty_var_unify(id, conds, ty),
            (Type::LambdaType(ty1), Type::LambdaType(ty2)) => self.lambda_unify(*ty1, *ty2),
            (Type::TupleType(ty1), Type::TupleType(ty2)) => self.tuple_unify(*ty1, *ty2),
            (ty1, ty2) => {
                if(ty1==ty2){
                    return Ok(self);

                }
                Err(format!(
                    "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                    ty1, ty2
                ))
            }
        }
    }

    fn ty_var_unify(mut self, ty_id: TypeId, conds: Vec<TypeCondition>, ty: Type) -> Result<TypeSubstitute, String> {
        match ty {
            Type::TyVar(id, conds2) => {
                for cond in conds.iter() {
                    for cond2 in conds2.iter() {
                        match (cond, cond2) {
                            (TypeCondition::Call(x), TypeCondition::Call(y)) => {
                                self = self.fn_unify(x.clone(), y.clone())?;
                            }
                        };
                    }
                }
                self = self.insert(ty_id, Type::TyVar(id, conds2))?;
            }
            Type::LambdaType(mut x) => {
                for cond in conds.into_iter() {
                    match cond {
                        TypeCondition::Call(c) => {
                            let env_len = x.env_ty.clone().map(|t| t.element_tys.len()).unwrap_or(0);
                            let mut func_ty = x.func_ty.clone();
                            func_ty.param_types = func_ty.param_types.clone().into_iter().skip(env_len).collect::<Vec<_>>();
                            self = self.fn_unify(func_ty, c)?;
                        }
                    }
                };
                self = self.insert(ty_id, Type::LambdaType(x))?;
            }
            ty => {
                if conds.len() != 0 {
                    return Err(format!(
                        "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                        Type::TyVar(ty_id, conds), ty
                    ));
                } else {
                    self = self.insert(ty_id, ty)?;
                }
            }
        };
        Ok(self)
    }

    fn fn_unify(mut self, ty1: FuncType, ty2: FuncType) -> Result<TypeSubstitute, String> {
        if ty1.param_types.len() != ty2.param_types.len() {
            return Err(format!(
                "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                ty1, ty2
            ));
        }
        for (x, y) in ty1.param_types.into_iter().zip(ty2.param_types) {
            self = self.unify(x, y)?;
        }
        self.unify(ty1.ret_type, ty2.ret_type)
    }

    fn tuple_unify(mut self, ty1: TupleType, ty2: TupleType) -> Result<TypeSubstitute, String> {
        if ty1.element_tys.len() != ty2.element_tys.len() {
            return Err(format!(
                "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                ty1, ty2
            ));
        }
        for (x, y) in ty1.element_tys.into_iter().zip(ty2.element_tys) {
            self = self.unify(x, y)?;
        }
        Ok(self)
    }

    fn lambda_unify(mut self, mut ty1: LambdaType, mut ty2: LambdaType) -> Result<TypeSubstitute, String> {
        match (ty1.env_ty, ty2.env_ty) {
            (Some(x), Some(y)) => {
                self = self.tuple_unify(x, y)?;
            }
            (None, None) => (),
            (None, x) | (x, None) => {
                return Err(format!(
                    "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                    "void", x
                ));
            }
        }
        self.fn_unify(ty1.func_ty, ty2.func_ty)
    }

    // 型変数に対応する単相型を見つけて返す。見つからなかったら空タプルの型を返す
    fn look_up(&self, ty_id: &TypeId, conds: &Vec<TypeCondition>) -> Type {
        match self.0.get(ty_id) {
            Some(ty) => self.type_look_up(ty),
            None => {
                if conds.len() == 1 {
                    match conds[0].clone() {
                        TypeCondition::Call(c) =>
                            Type::LambdaType(Box::new(LambdaType {
                                env_ty: None,
                                func_ty: self.func_look_up(&c)
                            }))
                    }
                } else {
                    Type::TupleType(Box::new(TupleType {
                        element_tys: vec![],
                    }))
                }
            }
        }
    }

    fn type_look_up(&self, ty: &Type) -> Type {
        match ty {
            Type::TyVar(ty_id, conds) => self.look_up(ty_id, conds),
            Type::TupleType(x) => Type::TupleType(Box::new(self.tuple_look_up(x))),
            Type::LambdaType(x) => self.lambda_look_up(x),
            ty => ty.clone(),
        }
    }
    fn func_look_up(&self, ty: &FuncType) -> FuncType {
        FuncType {
            param_types:
            ty.param_types
                .iter()
                .map(|ty| self.type_look_up(ty))
                .collect(),
            ret_type: self.type_look_up(&ty.ret_type),
        }
    }

    fn tuple_look_up(&self, ty: &TupleType) -> TupleType {
        TupleType {
            element_tys:
            ty.element_tys
                .iter()
                .map(|ty| self.type_look_up(ty))
                .collect(),
        }
    }

    fn lambda_look_up(&self, ty: &LambdaType) -> Type {
        Type::LambdaType(Box::new(LambdaType {
            env_ty: ty.env_ty.clone().map(|x| self.tuple_look_up(&x)),
            func_ty: self.func_look_up(&ty.func_ty),
        }))
    }
}

//型を解決した結果を持つ
pub struct TypeResolved(HashMap<String, Type>);

impl TypeResolved {
    fn new(ty_env: &TypeEnv, ty_subst: &TypeSubstitute) -> TypeResolved {
        TypeResolved(
            ty_env
                .env
                .iter()
                .map(|(k, v)| (k.clone(), ty_subst.look_up(&v, &vec![])))
                .collect(),
        )
    }

    //型変数に対応する単相型を返す
    pub fn get(&self, id: String) -> Type {
        self.0[&id].clone()
    }
}

use std::fmt;

impl fmt::Debug for TypeResolved
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use compile::show_type::ShowType;
        write!(f, "{}", self.0.iter().fold("".to_string(), |acc, x|
            acc + x.0 + "::" + &(x.1.show()) + "\n",
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

    pub fn look_up(&self, ty_id: &TypeId, conds: &Vec<TypeCondition>) -> Type {
        self.1.look_up(ty_id, conds)
    }

    pub fn look_up_name(&mut self, name: String) -> Type {
        self.1.look_up(&self.0.get(name), &vec![])
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
        self.1 = self.1.unify(ty1, ty2)?;
        Ok(self)
    }

    pub fn get_type_resolved(&self) -> TypeResolved {
        TypeResolved::new(&self.0, &self.1)
    }

    pub fn remove(&mut self, symbol: &String) {
        self.0.remove(symbol);
    }

    pub fn in_nest(&mut self) {
        self.0.in_nest();
    }

    pub fn out_nest(&mut self) {
        self.0.out_nest();
    }
}
