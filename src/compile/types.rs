#[derive(Debug, PartialEq)]
pub enum Type {
    Int32,
    FuncType(Box<FuncType>),
}

impl Type {
    pub fn create_func_type(param_types: Vec<Type>, ret_type: Type) -> Type {
        Type::FuncType(Box::new(FuncType { param_types, ret_type }))
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncType {
    pub param_types: Vec<Type>,
    pub ret_type: Type,
}

impl FuncType {
    pub fn params_len(&self) -> usize {
        self.param_types.len()
    }
    pub fn get_ret_type(self) -> Type {
        self.ret_type
    }
}
