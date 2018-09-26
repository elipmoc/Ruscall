use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int32,
    FuncType(Box<FuncType>),
    Unknown,
    Fn(Box<Type>),
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Type) -> Option<Ordering> {
        if self == other {
            return Option::Some(Ordering::Equal);
        }
        match (self, other) {
            (Type::Unknown, _) => Option::Some(Ordering::Less),
            (Type::FuncType(x), Type::FuncType(y)) => x.partial_cmp(&y),
            (Type::Fn(x), Type::Fn(y)) => x.partial_cmp(y),
            (_, Type::Unknown) => Option::Some(Ordering::Greater),
            _ => Option::None
        }
    }
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
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct FuncType {
    pub param_types: Vec<Type>,
    pub ret_type: Type,
}

fn partial_cmp_merge(left: &Option<Ordering>, right: &Option<Ordering>) -> Option<Ordering> {
    if let Option::Some(left) = left {
        if let Option::Some(right) = right {
            return
                match left.clone() {
                    Ordering::Equal => Option::Some(*right),
                    Ordering::Greater if *right != Ordering::Less => Option::Some(*left),
                    Ordering::Less if *right != Ordering::Greater => Option::Some(*left),
                    _ => Option::None
                };
        }
    }
    Option::None
}

impl PartialOrd for FuncType {
    fn partial_cmp(&self, other: &FuncType) -> Option<Ordering> {
        if self.param_types.len() != other.param_types.len() {
            return Option::None;
        }
        let params_ord = self.param_types.iter()
            .zip(&other.param_types)
            .map(|(x, y)| x.partial_cmp(y))
            .fold(
                Option::Some(Ordering::Equal),
                |acc, x| partial_cmp_merge(&acc, &x),
            );

        partial_cmp_merge(&params_ord, &self.ret_type.partial_cmp(&other.ret_type))
    }
}

impl FuncType {
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
