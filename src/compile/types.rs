use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int32,
    FuncType(Box<FuncType>),
    Unknown,
    Fn(Box<FuncType>),
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
        Type::Fn(Box::new(FuncType { param_types, ret_type }))
    }

    pub fn merge(self, other: Type) -> Option<Type> {
        match (self, other) {
            (Type::Unknown, other) => Option::Some(other),
            (x, Type::Unknown) => Option::Some(x),
            (Type::FuncType(x), other) => Option::Some(Type::FuncType(Box::new(x.merge(other)?))),
            (Type::Fn(x), Type::Fn(y)) => Option::Some(Type::Fn(Box::new(x.merge(Type::FuncType(Box::new(*y)))?))),
            ref x if x.0 == x.1 => Option::Some(x.0.clone()),
            _ => Option::None
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct FuncType {
    pub param_types: Vec<Type>,
    pub ret_type: Type,
}

fn partial_cmp_merge(left: &Option<Ordering>, right: &Option<Ordering>) -> Option<Ordering> {
    match (left, right) {
        (Option::Some(Ordering::Equal), Option::Some(right)) => Option::Some(*right),
        (Option::Some(Ordering::Greater), Option::Some(Ordering::Less)) => Option::None,
        (Option::Some(Ordering::Less), Option::Some(Ordering::Greater)) => Option::None,
        (left, _) => *left,
    }
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
    pub fn merge(self, other: Type) -> Option<FuncType> {
        match other {
            Type::FuncType(x) => {
                let x = *x;
                Option::Some(
                    FuncType {
                        param_types: self.param_types.into_iter().zip(x.param_types)
                            .map(|(a, b)| a.merge(b)).collect::<Option<Vec<Type>>>()?,
                        ret_type: self.ret_type.merge(x.ret_type)?,
                    }
                )
            }
            Type::Unknown => Option::Some(self),
            _ => Option::None
        }
    }
}
