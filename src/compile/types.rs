// 型変数のインデックス
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeId(usize);

impl TypeId {
    pub fn new(id: usize) -> TypeId {
        TypeId(id)
    }
    pub fn get_id(&self) -> usize {
        self.0
    }
}

//型制約
#[derive(Clone, PartialEq)]
pub enum TypeCondition {
    Call(FuncType)
}

#[derive(Clone, PartialEq)]
pub enum Type {
    Int32,
    TupleType(Box<TupleType>),
    TyVar(TypeId, Vec<TypeCondition>),
    LambdaType(Box<LambdaType>),
}

impl Type {
    pub fn create_func_type(param_types: Vec<Type>, ret_type: Type) -> Type {
        Type::LambdaType(Box::new(LambdaType { env_ty: None, func_ty: FuncType { param_types, ret_type } }))
    }
    pub fn create_tuple_type(element_tys: Vec<Type>) -> Type {
        Type::TupleType(Box::new(TupleType { element_tys }))
    }
    pub fn create_lambda_type(env_tys: Vec<Type>, func_ty: FuncType) -> Type {
        Type::LambdaType(Box::new(LambdaType { env_ty: Some(TupleType { element_tys: env_tys }), func_ty }))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub param_types: Vec<Type>,
    pub ret_type: Type,
}


#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub element_tys: Vec<Type>
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaType {
    pub env_ty: Option<TupleType>,
    pub func_ty: FuncType,
}