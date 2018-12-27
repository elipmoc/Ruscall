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
#[derive(Clone, PartialEq, Debug)]
pub enum TypeCondition {
    Call(Box<FuncType>),
    Empty,
    ImplItems(Box<ImplItems>),
}

impl TypeCondition {
    pub fn new() -> Self {
        TypeCondition::Empty
    }

    pub fn with_call(fn_ty: FuncType) -> Self {
        TypeCondition::Call(Box::new(fn_ty))
    }

    pub fn with_impl_tuple_property(index: u32, ty: Type) -> Self {
        TypeCondition::ImplItems(Box::new(ImplItems::new(index, ty)))
    }

    pub fn is_call(&self) -> bool {
        if let TypeCondition::Call(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_empty(&self) -> bool {
        if let TypeCondition::Empty = self {
            true
        } else {
            false
        }
    }
}

use std::collections::HashMap;
use std::collections::hash_map::{Values,Iter};

#[derive(Clone, PartialEq, Debug)]
pub struct ImplItems(pub HashMap<u32, Type>);

impl ImplItems {
    fn new(index: u32, ty: Type) -> Self {
        let mut x = ImplItems(HashMap::new());
        x.0.insert(index, ty);
        x
    }

    pub fn merge(other1: Self, other2: Self) -> Self {
        ImplItems(other1.0.into_iter().chain(other2.0.into_iter()).collect())
    }

    pub fn types(&self) -> Values<u32, Type> {
        self.0.values()
    }

    pub fn get_tuple_properties(&self) -> Iter<u32, Type> {
        self.0.iter()
    }
}

#[derive(Clone, PartialEq)]
pub enum Type {
    Int32,
    Bool,
    TupleType(Box<TupleType>),
    TyVar(TypeId, TypeCondition),
    LambdaType(Box<LambdaType>),
    StructType(Box<StructType>),
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
pub enum StructInternalType {
    RecordType(RecordType),
    TupleType(TupleType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    pub ty: StructInternalType,
    pub name: String,
}


#[derive(Debug, Clone, PartialEq)]
pub struct RecordType {
    pub element_tys: Vec<(String, Type)>
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaType {
    pub env_ty: Option<TupleType>,
    pub func_ty: FuncType,
}