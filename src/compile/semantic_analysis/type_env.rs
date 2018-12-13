use super::super::types::types::*;
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
    fn new() -> TypeSubstitute {
        TypeSubstitute(HashMap::new())
    }

    //TypeにTypeIdが出現するか検査
    fn occurs_check(&self, ty: &Type, ty_id: &TypeId) -> bool {
        impl TupleType {
            fn occurs_check(&self, ty_sub: &TypeSubstitute, ty_id: &TypeId) -> bool {
                self.element_tys.iter().any(|e| ty_sub.occurs_check(e, ty_id))
            }
        }
        match ty {
            Type::TyVar(id, conds) => {
                conds.iter().any(|x|
                    match x {
                        TypeCondition::Call(x) =>
                            self.occurs_check(&x.ret_type, &ty_id)
                                ||
                                x.param_types.iter().any(|x| self.occurs_check(x, &ty_id))
                    }
                ) ||
                    if id == ty_id { true } else {
                        if self.0.contains_key(&id) {
                            self.occurs_check(&self.0[id], &ty_id)
                        } else {
                            false
                        }
                    }
            }
            Type::Int32 | Type::Bool => false,
            Type::TupleType(x) => x.occurs_check(&self, ty_id),
            Type::LambdaType(x) => {
                let x = &**x;
                x.env_ty.as_ref().unwrap_or(&TupleType { element_tys: vec![] })
                    .element_tys.iter().any(|e| self.occurs_check(e, ty_id))
                    ||
                    x.func_ty.param_types.iter().any(|e| self.occurs_check(e, ty_id))
                    ||
                    self.occurs_check(&x.func_ty.ret_type, ty_id)
            }
            Type::StructType(x) => {
                match x.ty {
                    StructInternalType::TupleType(ref x) => x.occurs_check(&self, ty_id),
                    StructInternalType::RecordType(ref x) => x.element_tys.iter().any(|(_, e)| self.occurs_check(e, ty_id))
                }
            }
            _ => panic!("undefined type!")
        }
    }

    fn insert(mut self, ty_id: TypeId, ty: Type) -> Result<TypeSubstitute, String> {
        match self.0.remove(&ty_id) {
            Some(ty2) => {
                self.0.insert(ty_id, ty2.clone());
                self.unify(ty, ty2.clone())
            }
            None => {
                if self.occurs_check(&ty, &ty_id) == false {
                    println!("{:?}=>{:?}", ty_id, ty);
                    self.0.insert(ty_id, ty);
                } else {
                    println!("occurs! {:?}=>{:?}", ty_id, ty);
                }
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
                if ty1 == ty2 {
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
                if conds.len() > conds2.len() {
                    self = self.insert(id, Type::TyVar(ty_id, conds))?;
                } else {
                    self = self.insert(ty_id, Type::TyVar(id, conds2))?;
                }
            }
            Type::LambdaType(x) => {
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

    fn lambda_unify(mut self, ty1: LambdaType, ty2: LambdaType) -> Result<TypeSubstitute, String> {
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
                                func_ty: self.func_look_up(&c),
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

    pub fn look_up(&self, ty_id: &TypeId, conds: &Vec<TypeCondition>) -> Type {
        self.1.look_up(ty_id, conds)
    }

    pub fn look_up_func_name(&mut self, name: String) -> Type {
        self.1.look_up(&self.0.global_get(name), &vec![])
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

    pub fn in_nest(&mut self) {
        self.0.in_nest();
    }

    pub fn out_nest(&mut self) {
        self.0.out_nest();
    }
}
