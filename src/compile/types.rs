#[derive(Clone, PartialEq)]
pub enum Type {
    Int32,
    FuncType(Box<FuncType>),
    TupleType(Box<TupleType>),
    TyVar(usize),
    LambdaType(Box<LambdaType>)
}

impl Type {
    pub fn create_func_type(param_types: Vec<Type>, ret_type: Type) -> Type {
        Type::FuncType(Box::new(FuncType { param_types, ret_type }))
    }
    pub fn create_tuple_type(element_tys: Vec<Type>) -> Type {
        Type::TupleType(Box::new(TupleType { element_tys }))
    }
    pub fn create_lambda_type(env_tys:Vec<Type>,func_ty:FuncType)->Type{
        Type::LambdaType(Box::new(LambdaType{env_tys,func_ty}))
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
    pub env_tys: Vec<Type>,
    pub func_ty:FuncType
}