use super::super::super::types::types::*;
use super::occurs_check::occurs_check;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
struct TypeEnv {
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

    fn in_nest(&mut self) {
        self.nest += 1;
        self.env.push(HashMap::new());
    }

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
    fn no_name_get(&mut self) -> TypeId {
        self.id += 1;
        println!("{{no_symbol}}:={:?}", self.id - 1);
        TypeId::new(self.id - 1)
    }
}

//型代入環境
#[derive(Debug, PartialEq)]
struct TypeSubstitute(pub HashMap<TypeId, Type>);

impl TypeSubstitute {
    fn new() -> Self {
        TypeSubstitute(HashMap::new())
    }


    fn insert2(self, ty_env: TypeEnv, ty1: Type, ty2: Type) -> Result<(Self, Type, TypeEnv), String> {
        if ty1 == ty2 {
            return Ok((self, ty1, ty_env));
        }
        match (&ty1, &ty2) {
            (Type::TyVar(ref ty_id1, _), Type::TyVar(ref ty_id2, _)) => {
                match (self.0.get(ty_id1).map(Clone::clone), self.0.get(ty_id2).map(Clone::clone)) {
                    (Some(ty1), Some(ty2)) => self.insert2(ty_env, ty1.clone(), ty2.clone()),
                    (Some(ty1), None) => self.insert2(ty_env, ty1.clone(), ty2.clone()),
                    (None, Some(ty2)) => self.insert2(ty_env, ty1.clone(), ty2.clone()),
                    (None, None) => {
                        let (mut ty_sub, ty, ty_env) = self.unify(ty_env, ty1.clone(), ty2.clone())?;
                        ty_sub.safe_insert(ty_id1.clone(), ty.clone());
                        ty_sub.safe_insert(ty_id2.clone(), ty.clone());
                        Ok((ty_sub, ty, ty_env))
                    }
                }
            }

            (Type::TyVar(ref ty_id1, _), _) => {
                match self.0.get(ty_id1).map(Clone::clone) {
                    Some(ty1) => self.insert2(ty_env, ty1.clone(), ty2.clone()),
                    None => {
                        let (mut ty_sub, ty, ty_env) = self.unify(ty_env, ty1.clone(), ty2.clone())?;
                        ty_sub.safe_insert(ty_id1.clone(), ty.clone());
                        Ok((ty_sub, ty, ty_env))
                    }
                }
            }

            (_, Type::TyVar(ref ty_id2, _)) => {
                match self.0.get(ty_id2).map(Clone::clone) {
                    Some(ty2) => self.insert2(ty_env, ty1.clone(), ty2.clone()),
                    None => {
                        let (mut ty_sub, ty, ty_env) = self.unify(ty_env, ty1.clone(), ty2.clone())?;
                        ty_sub.safe_insert(ty_id2.clone(), ty.clone());
                        Ok((ty_sub, ty, ty_env))
                    }
                }
            }
            (ty1, ty2) => self.unify(ty_env, ty1.clone(), ty2.clone())
        }
    }

    fn safe_insert(&mut self, ty_id: TypeId, insert_ty: Type) {
        if occurs_check(&self.0, &insert_ty, &ty_id) == false {
            println!("{:?}=>{:?}", ty_id, insert_ty);
            self.0.insert(ty_id, insert_ty);
        } else {
            println!("occurs! {:?}=>{:?}", ty_id, insert_ty);
        }
    }

    fn unify(self, ty_env: TypeEnv, ty1: Type, ty2: Type) -> Result<(Self, Type, TypeEnv), String> {
        match (ty1, ty2) {
            (Type::TyVar(id, conds), ty) => self.ty_var_unify(ty_env, id, conds, ty),
            (ty, Type::TyVar(id, conds)) => self.ty_var_unify(ty_env, id, conds, ty),
            (Type::LambdaType(ty1), Type::LambdaType(ty2)) => self.lambda_unify(ty_env, *ty1, *ty2),
            (Type::TupleType(ty1), Type::TupleType(ty2)) => self.tuple_unify(ty_env, *ty1, *ty2),
            (ty1, ty2) => {
                if ty1 == ty2 {
                    return Ok((self, ty1, ty_env));
                }
                Err(format!(
                    "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                    ty1, ty2
                ))
            }
        }
    }
    fn ty_var_unify(mut self, mut ty_env: TypeEnv, ty_id: TypeId, cond: TypeCondition, ty: Type) -> Result<(Self, Type, TypeEnv), String> {
        let result_ty =
            match ty {
                Type::TyVar(id, mut cond2) => {
                    match (cond.call, cond2.call) {
                        (Some(fn_ty1), Some(fn_ty2)) => {
                            let (ty_sub, new_fn_ty, new_ty_env) = self.fn_unify(ty_env, *fn_ty1, *fn_ty2)?;
                            self = ty_sub;
                            ty_env = new_ty_env;
                            Type::TyVar(ty_env.no_name_get(), TypeCondition { call: Some(Box::new(new_fn_ty)) })
                        }
                        (fn_ty1, None) => {
                            Type::TyVar(ty_id, TypeCondition { call: fn_ty1 })
                        }
                        (None, fn_ty2) => {
                            Type::TyVar(id, TypeCondition { call: fn_ty2 })
                        }
                    }
                }
                Type::LambdaType(x) => {
                    match cond.call {
                        Some(c) => {
                            let env_len = x.env_ty.clone().map(|t| t.element_tys.len()).unwrap_or(0);
                            let mut func_ty = x.func_ty.clone();
                            func_ty.param_types = func_ty.param_types.clone().into_iter().skip(env_len).collect::<Vec<_>>();
                            let (ty_sub, _, new_ty_env) = self.fn_unify(ty_env, func_ty, *c)?;
                            self = ty_sub;
                            ty_env = new_ty_env;
                        }
                        _ => ()
                    };
                    Type::LambdaType(x.clone())
                }
                ty => {
                    if cond.call.is_some() {
                        return Err(format!(
                            "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                            Type::TyVar(ty_id, cond), ty
                        ));
                    } else {
                        ty
                    }
                }
            };
        Ok((self, result_ty, ty_env))
    }

    fn fn_unify(mut self, mut ty_env: TypeEnv, ty1: FuncType, ty2: FuncType) -> Result<(Self, FuncType, TypeEnv), String> {
        if ty1.param_types.len() != ty2.param_types.len() {
            return Err(format!(
                "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                ty1, ty2
            ));
        }
        let mut new_param_types: Vec<Type> = Vec::with_capacity(ty1.param_types.len());
        for (x, y) in ty1.param_types.into_iter().zip(ty2.param_types) {
            let (ty_sub, ty, new_ty_env) = self.insert2(ty_env, x, y)?;
            self = ty_sub;
            ty_env = new_ty_env;
            new_param_types.push(ty);
        }
        let (ty_sub, new_ret_type, ty_env) = self.insert2(ty_env, ty1.ret_type, ty2.ret_type)?;
        Ok((ty_sub, FuncType { param_types: new_param_types, ret_type: new_ret_type }, ty_env))
    }

    fn tuple_unify(mut self, mut ty_env: TypeEnv, ty1: TupleType, ty2: TupleType) -> Result<(Self, Type, TypeEnv), String> {
        if ty1.element_tys.len() != ty2.element_tys.len() {
            return Err(format!(
                "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                ty1, ty2
            ));
        }
        for (x, y) in ty1.element_tys.clone().into_iter().zip(ty2.element_tys) {
            let (ty_sub, _, new_ty_env) = self.insert2(ty_env, x, y)?;
            self = ty_sub;
            ty_env = new_ty_env;
        }
        Ok((self, Type::TupleType(Box::new(ty1)), ty_env))
    }

    fn lambda_unify(mut self, mut ty_env: TypeEnv, ty1: LambdaType, ty2: LambdaType) -> Result<(Self, Type, TypeEnv), String> {
        match (ty1.env_ty.clone(), ty2.env_ty) {
            (Some(x), Some(y)) => {
                let (ty_sub, _, new_ty_env) = self.tuple_unify(ty_env, x, y)?;
                self = ty_sub;
                ty_env = new_ty_env;
            }
            (None, None) => (),
            (None, x) | (x, None) => {
                return Err(format!(
                    "TypeSubstitute insert error! \n expect:{:?} \n actual:{:?}",
                    "void", x
                ));
            }
        }
        let (ty_sub, _, ty_env) = self.fn_unify(ty_env, ty1.func_ty.clone(), ty2.func_ty)?;
        Ok((ty_sub, Type::LambdaType(Box::new(ty1)), ty_env))
    }

    // 型変数に対応する単相型を見つけて返す。見つからなかったら空タプルの型を返す
    fn look_up(&self, ty_id: &TypeId, cond: &TypeCondition) -> Type {
        match self.0.get(ty_id) {
            Some(ty) => self.type_look_up(ty),
            None => {
                match &cond.call {
                    Some(x) =>
                        Type::LambdaType(Box::new(LambdaType {
                            env_ty: None,
                            func_ty: self.func_look_up(&x),
                        })),

                    None =>
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
            Type::StructType(x) => self.struct_look_up(x),
            Type::Int32 | Type::Bool => ty.clone(),
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

    fn record_look_up(&self, ty: &RecordType) -> RecordType {
        RecordType {
            element_tys:
            ty.element_tys
                .iter()
                .map(|(name, ty)| (name.clone(), self.type_look_up(ty)))
                .collect(),
        }
    }

    fn struct_look_up(&self, ty: &StructType) -> Type {
        let mut ty = ty.clone();
        ty.ty =
            match ty.ty {
                StructInternalType::TupleType(x) => StructInternalType::TupleType(self.tuple_look_up(&x)),
                StructInternalType::RecordType(x) => StructInternalType::RecordType(self.record_look_up(&x)),
            };
        Type::StructType(Box::new(ty))
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
                .map(|(k, v)| (k.clone(), ty_subst.look_up(&v, &TypeCondition::new())))
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
        use compile::types::show_type::ShowType;
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

    pub fn look_up(&self, ty_id: &TypeId, cond: &TypeCondition) -> Type {
        self.1.look_up(ty_id, cond)
    }

    pub fn look_up_func_name(&mut self, name: String) -> Type {
        self.1.look_up(&self.0.global_get(name), &TypeCondition::new())
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
        let (ty_sub, _, ty_env) = self.1.insert2(self.0, ty1, ty2)?;
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
