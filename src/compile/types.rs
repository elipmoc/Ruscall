use std::cmp::Ordering;
use super::show_type::type_error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int32,
    FuncType(Box<FuncType>),
    Unknown,
    Fn(Box<FuncType>),
    TupleType(Box<TupleType>),
}

type MergeResult<T> = Result<T, String>;

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Type) -> Option<Ordering> {
        if self == other {
            return Option::Some(Ordering::Equal);
        }
        match (self, other) {
            (Type::Unknown, _) => Option::Some(Ordering::Less),
            (Type::FuncType(x), Type::FuncType(y)) => x.partial_cmp(&y),
            (Type::Fn(x), Type::Fn(y)) => x.partial_cmp(y),
            (Type::TupleType(x), Type::TupleType(y)) => x.partial_cmp(y),
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

    pub fn merge(self, other: Type) -> MergeResult<Type> {
        match (self, other) {
            (Type::Unknown, other) => Ok(other),
            (x, Type::Unknown) => Ok(x),
            (Type::FuncType(x), other) => Ok(Type::FuncType(Box::new(x.merge(other)?))),
            (Type::TupleType(x), other) => Ok(Type::TupleType(Box::new(x.merge(other)?))),
            (Type::Fn(x), Type::Fn(y)) => Ok(Type::Fn(Box::new(x.merge(Type::FuncType(Box::new(*y)))?))),
            ref x if x.0 == x.1 => Ok(x.0.clone()),
            (x, y) => type_error(&y, &x)
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
        (Some(Ordering::Equal), right) => *right,
        (Some(Ordering::Greater), Some(Ordering::Less)) => None,
        (Some(Ordering::Less), Some(Ordering::Greater)) => None,
        (_,None)=>None,
        (None,_)=>None,
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

    pub fn merge(self, other: Type) -> MergeResult<FuncType> {
        match other {
            Type::FuncType(x) => {
                let x = *x;
                if x.param_types.len() != self.param_types.len() {
                    return type_error(&x, &self);
                }
                Ok(
                    FuncType {
                        param_types: self.param_types.into_iter().zip(x.param_types)
                            .map(|(a, b)| a.merge(b)).collect::<MergeResult<Vec<Type>>>()?,
                        ret_type: self.ret_type.merge(x.ret_type)?,
                    }
                )
            }
            Type::Unknown => Ok(self),
            _ => type_error(&other, &self)
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct TupleType {
    pub element_tys: Vec<Type>
}

impl PartialOrd for TupleType {
    fn partial_cmp(&self, other: &TupleType) -> Option<Ordering> {
        if self.element_tys.len() != other.element_tys.len() {
            return Option::None;
        }
        let elements_ord = self.element_tys.iter()
            .zip(&other.element_tys)
            .map(|(x, y)| x.partial_cmp(y))
            .fold(
                Option::Some(Ordering::Equal),
                |acc, x| partial_cmp_merge(&acc, &x),
            );
        elements_ord
    }
}

impl TupleType {


    pub fn merge(self, other: Type) -> MergeResult<TupleType> {
        match other {
            Type::TupleType(x) => {
                let x = *x;
                if x.element_tys.len() != self.element_tys.len() {
                    return type_error( &x, &self);
                }
                Ok(
                    TupleType {
                        element_tys: self.element_tys.into_iter().zip(x.element_tys)
                            .map(|(a, b)| a.merge(b)).collect::<MergeResult<Vec<Type>>>()?,
                    }
                )
            }
            Type::Unknown => Ok(self),
            _ => type_error(&other, &self)
        }
    }
}
