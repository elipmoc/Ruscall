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

use std::hash::{Hash, Hasher};

impl Hash for TypeCondition {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl TypeCondition {
    pub fn new() -> Self {
        TypeCondition::Empty
    }

    pub fn with_call(fn_ty: FuncType) -> Self {
        TypeCondition::Call(Box::new(fn_ty))
    }

    pub fn with_impl_index_property(index: u32, ty: Type) -> Self {
        TypeCondition::ImplItems(Box::new(ImplItems::with_index_property(index, ty)))
    }

    pub fn with_impl_name_property(name: String, ty: Type) -> Self {
        TypeCondition::ImplItems(Box::new(ImplItems::with_name_property(name, ty)))
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
use std::collections::hash_map::{Values, Iter};

#[derive(Clone, PartialEq, Debug)]
//ある要素が定義されているという制約を保存する構造体
pub struct ImplItems {
    //x.1 などのプロパティアクセスの制約
    index_properties: HashMap<u32, Type>,
    //x.hoge などのプロパティアクセスの制約
    name_properties: HashMap<String, Type>,
}

impl ImplItems {
    fn with_index_property(index: u32, ty: Type) -> Self {
        let mut x = ImplItems { index_properties: HashMap::new(), name_properties: HashMap::new() };
        x.index_properties.insert(index, ty);
        x
    }

    fn with_name_property(name: String, ty: Type) -> Self {
        let mut x = ImplItems { index_properties: HashMap::new(), name_properties: HashMap::new() };
        x.name_properties.insert(name, ty);
        x
    }

    pub fn merge<ACC, F: Fn(ACC, Type, Type) -> Result<(ACC, Type), String>>(mut other1: Self, other2: Self, mut acc: ACC, func: &mut F) -> Result<(ACC, Self), String> {
        for (key, ty) in other2.index_properties {
            if let Some(ty2) = other1.index_properties.remove(&key) {
                let (new_acc, ty) = func(acc, ty2, ty)?;
                acc = new_acc;
                other1.index_properties.insert(key, ty);
            } else {
                other1.index_properties.insert(key, ty);
            }
        }
        for (key, ty) in other2.name_properties {
            if let Some(ty2) = other1.name_properties.remove(&key) {
                let (new_acc, ty) = func(acc, ty2, ty)?;
                acc = new_acc;
                other1.name_properties.insert(key, ty);
            } else {
                other1.name_properties.insert(key, ty);
            }
        }

        Ok((
            acc,
            ImplItems {
                index_properties: other1.index_properties,
                name_properties: other1.name_properties,
            }))
    }

    pub fn get_index_property_types(&self) -> Values<u32, Type> {
        self.index_properties.values()
    }

    pub fn get_index_properties(&self) -> Iter<u32, Type> {
        self.index_properties.iter()
    }

    pub fn get_name_property_types(&self) -> Values<String, Type> {
        self.name_properties.values()
    }

    pub fn get_name_properties(&self) -> Iter<String, Type> {
        self.name_properties.iter()
    }
}

#[derive(Clone, PartialEq, Hash)]
pub enum Type {
    TCon { name: String },
    TupleType(Box<TupleType>),
    TyVar(TypeId, TypeCondition),
    LambdaType(Box<LambdaType>),
    StructType(Box<StructType>),
}

impl Type {
    pub fn create_int32() -> Type {
        Type::TCon { name: "Int32".to_string() }
    }
    pub fn create_bool() -> Type {
        Type::TCon { name: "Bool".to_string() }
    }
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

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FuncType {
    pub param_types: Vec<Type>,
    pub ret_type: Type,
}


#[derive(Debug, Clone, PartialEq, Hash)]
pub struct TupleType {
    pub element_tys: Vec<Type>
}

impl TupleTypeBase for TupleType {
    fn get_elements_at(&self, index: usize) -> &Type {
        &self.element_tys[index]
    }

    fn get_elements_len(&self) -> usize {
        self.element_tys.len()
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum StructInternalType {
    RecordType(RecordType),
    TupleType(TupleType),
}


impl TupleTypeBase for StructInternalType {
    fn get_elements_at(&self, index: usize) -> &Type {
        match self {
            StructInternalType::RecordType(x) => x.get_elements_at(index),
            StructInternalType::TupleType(x) => x.get_elements_at(index)
        }
    }

    fn get_elements_len(&self) -> usize {
        match self {
            StructInternalType::RecordType(x) => x.get_elements_len(),
            StructInternalType::TupleType(x) => x.get_elements_len()
        }
    }
    fn get_elements_from_record_name(&self, record_name: &String) -> Option<&Type> {
        match self {
            StructInternalType::RecordType(x) => x.get_elements_from_record_name(record_name),
            StructInternalType::TupleType(x) => x.get_elements_from_record_name(record_name)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct StructType {
    pub ty: StructInternalType,
    pub name: String,
}

impl TupleTypeBase for StructType {
    fn get_elements_at(&self, index: usize) -> &Type {
        &self.ty.get_elements_at(index)
    }

    fn get_elements_len(&self) -> usize {
        self.ty.get_elements_len()
    }

    fn get_elements_from_record_name(&self, record_name: &String) -> Option<&Type> {
        self.ty.get_elements_from_record_name(record_name)
    }
}


#[derive(Debug, Clone, PartialEq, Hash)]
pub struct RecordType {
    pub element_tys: Vec<(String, Type)>
}

impl TupleTypeBase for RecordType {
    fn get_elements_at(&self, index: usize) -> &Type {
        &self.element_tys[index].1
    }

    fn get_elements_len(&self) -> usize {
        self.element_tys.len()
    }
    fn get_elements_from_record_name(&self, record_name: &String) -> Option<&Type> {
        self.element_tys.iter().find(|(name, _)| name == record_name).map(|(_, ty)| ty)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct LambdaType {
    pub env_ty: Option<TupleType>,
    pub func_ty: FuncType,
}

//タプルぽく振る舞えるかの制約
pub trait TupleTypeBase {
    fn get_elements_at(&self, index: usize) -> &Type;
    fn get_elements_len(&self) -> usize;
    fn get_elements_from_record_name(&self, _: &String) -> Option<&Type> {
        None
    }
}