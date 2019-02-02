use super::*;

//型制約
#[derive(Clone, PartialEq, Debug)]
pub struct Pred {
    pub ty: Type,
    pub cond: Condition,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Condition {
    Call(Box<FuncType>),
    Empty,
    Items(Box<ImplItems>),
}

use std::hash::{Hash, Hasher};

impl Hash for Pred {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

use self::Condition::*;

impl Pred {
    pub fn is_call(&self) -> bool {
        if let Call(_) = self.cond {
            true
        } else {
            false
        }
    }

    pub fn is_empty(&self) -> bool {
        if let Empty = self.cond {
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
    pub fn with_index_property(index: u32, ty: Type) -> Self {
        let mut x = ImplItems { index_properties: HashMap::new(), name_properties: HashMap::new() };
        x.index_properties.insert(index, ty);
        x
    }

    pub fn with_name_property(name: String, ty: Type) -> Self {
        let mut x = ImplItems { index_properties: HashMap::new(), name_properties: HashMap::new() };
        x.name_properties.insert(name, ty);
        x
    }

    pub fn merge<F: FnMut(Type, Type) -> Result<Type, String>>(mut other1: Self, other2: Self, func: &mut F) -> Result<Self, String> {
        for (key, ty) in other2.index_properties {
            if let Some(ty2) = other1.index_properties.remove(&key) {
                let ty = func(ty2, ty)?;
                other1.index_properties.insert(key, ty);
            } else {
                other1.index_properties.insert(key, ty);
            }
        }
        for (key, ty) in other2.name_properties {
            if let Some(ty2) = other1.name_properties.remove(&key) {
                let ty = func(ty2, ty)?;
                other1.name_properties.insert(key, ty);
            } else {
                other1.name_properties.insert(key, ty);
            }
        }

        Ok(
            ImplItems {
                index_properties: other1.index_properties,
                name_properties: other1.name_properties,
            }
        )
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

pub type Preds = HashMap<Type, Pred>;
