
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int32,
    FuncType(Box<FuncType>),
    Fn(Box<FuncType>),
    TupleType(Box<TupleType>),
    TyVar(usize),
}

impl Type {
    pub fn create_func_type(param_types: Vec<Type>, ret_type: Type) -> Type {
        Type::FuncType(Box::new(FuncType { param_types, ret_type }))
    }
    pub fn create_fn_func_type(param_types: Vec<Type>, ret_type: Type) -> Type {
        Type::Fn(Box::new(FuncType { param_types, ret_type }))
    }
    pub fn create_tuple_type(element_tys: Vec<Type>) -> Type {
        Type::TupleType(Box::new(TupleType { element_tys }))
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