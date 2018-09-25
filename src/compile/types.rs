#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int32,
    FuncType(Box<FuncType>),
    Unknown,
    Fn(Box<Type>),
}

impl Type {
    pub fn create_func_type(param_types: Vec<Type>, ret_type: Type) -> Type {
        Type::FuncType(Box::new(FuncType { param_types, ret_type }))
    }
    pub fn create_fn_func_type(param_types: Vec<Type>, ret_type: Type) -> Type {
        Type::Fn(Box::new(Type::FuncType(Box::new(FuncType { param_types, ret_type }))))
    }

    pub fn merge(self, other: Type) -> Type {
        match self {
            Type::Unknown => other,
            Type::FuncType(x) => x.merge(other),
            Type::Fn(x) => match other {
                Type::Fn(y) => Type::Fn(Box::new(x.merge(*y))),
                _ => Type::Fn(Box::new(*x))
            }
            x => x
        }
    }

    pub fn type_equal(&self, other: &Type) -> bool {
        match self {
            Type::Unknown => true,
            Type::FuncType(x) => {
                match other {
                    Type::FuncType(y) => x.type_equal(y),
                    _ => false
                }
            }
            Type::Fn(x) => {
                match other {
                    Type::Fn(y) => x.type_equal(y),
                    _ => false
                }
            }
            x => {
                match other {
                    Type::Unknown => true,
                    y => x.eq(&y)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
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
    pub fn type_equal(&self, other: &FuncType) -> bool {
        if self.param_types.len() != other.param_types.len() {
            return false;
        }
        self.param_types.iter()
            .zip(&other.param_types)
            .fold(true, |acc, (x, y)| x.type_equal(&y) && acc)
            && self.ret_type.type_equal(&other.ret_type)
    }

    pub fn merge(self, other: Type) -> Type {
        match other {
            Type::FuncType(x) => {
                let x = *x;
                Type::create_func_type(
                    self.param_types.into_iter().zip(x.param_types)
                        .map(|(a, b)| a.merge(b)).collect(),
                    self.ret_type.merge(x.ret_type),
                )
            }
            _ => Type::FuncType(Box::new(self))
        }
    }
}
