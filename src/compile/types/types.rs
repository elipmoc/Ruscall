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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    TCon { name: String },
    TGen(usize),
    TupleType(Box<TupleType>),
    TyVar(TypeId),
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
    pub fn create_func_type2(func_ty: FuncType) -> Type {
        Type::LambdaType(Box::new(LambdaType { env_ty: None, func_ty }))
    }
    pub fn create_tuple_type(element_tys: Vec<Type>) -> Type {
        Type::TupleType(Box::new(TupleType { element_tys }))
    }
    pub fn create_lambda_type(env_tys: Vec<Type>, func_ty: FuncType) -> Type {
        Type::LambdaType(Box::new(LambdaType { env_ty: Some(TupleType { element_tys: env_tys }), func_ty }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub param_types: Vec<Type>,
    pub ret_type: Type,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

use std::collections::HashSet;

pub trait Types {
    //型が持っている型変数一覧を列挙する
    fn tv_list(&self) -> HashSet<TypeId>;
}

impl<'a, T: Types> Types for &'a T {
    fn tv_list(&self) -> HashSet<TypeId> {
        (*self).tv_list()
    }
}

impl<Item: Types> Types for Vec<Item> {
    fn tv_list(&self) -> HashSet<TypeId> {
        self.iter().fold(HashSet::new(), |mut acc, ty| {
            acc.extend(ty.tv_list());
            acc
        })
    }
}

impl Types for Type {
    fn tv_list(&self) -> HashSet<TypeId> {
        use self::Type::*;
        match self {
            TCon { .. } | TGen(_) => HashSet::new(),
            TyVar(ty_id) => {
                let mut tv = HashSet::new();
                tv.insert(ty_id.clone());
                tv
            }
            TupleType(tuple_ty) => tuple_ty.element_tys.tv_list(),
            StructType(struct_ty) => {
                match &struct_ty.ty {
                    StructInternalType::TupleType(tuple_ty) => tuple_ty.element_tys.tv_list(),
                    StructInternalType::RecordType(record_ty) => record_ty.element_tys.iter().map(|(_, ty)| ty).collect::<Vec<_>>().tv_list()
                }
            }
            LambdaType(lambda_ty) => {
                let mut tv = match &lambda_ty.env_ty {
                    Some(tuple_ty) => tuple_ty.element_tys.tv_list(),
                    None => HashSet::new()
                };
                tv.extend(lambda_ty.func_ty.param_types.tv_list());
                tv.extend(lambda_ty.func_ty.ret_type.tv_list());
                tv
            }
        }
    }
}

//量化された型を実体化できる性質を持つ型クラス
pub trait Instantiate {
    fn inst(self, fresh_types: &Vec<Type>) -> Self;
}

impl<Item: Instantiate> Instantiate for Vec<Item> {
    fn inst(self, fresh_types: &Vec<Type>) -> Vec<Item> {
        self.into_iter().map(|x| x.inst(fresh_types)).collect()
    }
}

impl<Item: Instantiate> Instantiate for Vec<(String, Item)> {
    fn inst(self, fresh_types: &Vec<Type>) -> Vec<(String, Item)> {
        self.into_iter().map(|(s, i)| (s, i.inst(fresh_types))).collect()
    }
}

impl Instantiate for Type {
    fn inst(self, fresh_types: &Vec<Type>) -> Type {
        use self::Type::*;
        match self {
            ty @ TCon { .. } => ty,
            TGen(n) => fresh_types[n].clone(),
            ty @ TyVar(_) => ty,
            TupleType(tuple_ty) => {
                let mut tuple_ty = *tuple_ty;
                tuple_ty.element_tys = tuple_ty.element_tys.inst(fresh_types);
                TupleType(Box::new(tuple_ty))
            }
            StructType(struct_ty) => {
                let mut struct_ty = *struct_ty;
                struct_ty.ty = match struct_ty.ty {
                    StructInternalType::TupleType(mut tuple_ty) => {
                        tuple_ty.element_tys = tuple_ty.element_tys.inst(fresh_types);
                        StructInternalType::TupleType(tuple_ty)
                    }
                    StructInternalType::RecordType(mut record_ty) => {
                        record_ty.element_tys = record_ty.element_tys.inst(fresh_types);
                        StructInternalType::RecordType(record_ty)
                    }
                };
                StructType(Box::new(struct_ty))
            }
            LambdaType(lambda_ty) => {
                let mut lambda_ty = *lambda_ty;
                lambda_ty.env_ty = match lambda_ty.env_ty {
                    Some(mut tuple_ty) => {
                        tuple_ty.element_tys = tuple_ty.element_tys.inst(fresh_types);
                        Some(tuple_ty)
                    }
                    None => None
                };
                lambda_ty.func_ty.param_types = (lambda_ty.func_ty.param_types.inst(fresh_types));
                lambda_ty.func_ty.ret_type = lambda_ty.func_ty.ret_type.inst(fresh_types);
                LambdaType(Box::new(lambda_ty))
            }
        }
    }
}


pub type Preds = HashMap<Type, Pred>;

#[derive(Clone, PartialEq, Debug)]
//制約と型をセットにしたもの
//psが制約
//tがその制約がかけられたなにか
//例: ps: aはNum制約がある。 t:a->a
//説明：　Num制約がかかった型変数aがあり、型はa->aである
pub struct Qual<T> {
    pub ps: Preds,
    pub t: T,
}

impl<T> Qual<T> {
    pub fn new(t: T) -> Qual<T> {
        Qual { ps: HashMap::new(), t }
    }
    //複数のqualをpredとtでそれぞれ分割する。
    pub fn split(qs: Vec<Qual<T>>) -> (Vec<Preds>, Vec<T>) {
        let mut pss = Vec::with_capacity(qs.len());
        let mut ts = Vec::with_capacity(qs.len());
        for q in qs {
            pss.push(q.ps);
            ts.push(q.t);
        }
        (pss, ts)
    }
}


#[derive(Clone, PartialEq, Debug)]
//型スキーム
pub enum Scheme {
    Forall { qual: Qual<Type>, tgen_count: usize }
}

use super::super::semantic_analysis::type_inference::type_env::*;

impl Scheme {
    pub fn get_qual(&self) -> &Qual<Type> {
        match self {
            Scheme::Forall { qual, .. } => &qual
        }
    }

    //量化する
    pub fn quantify(q: Qual<Type>, ty_info: &mut TypeInfo) -> Result<Self, String> {
        let mut n: usize = 0;
        for ty_id in q.t.tv_list() {
            ty_info.unify(Type::TyVar(ty_id), Type::TGen(n))?;
            n += 1;
        };
        Ok(Scheme::Forall { qual: ty_info.last_qual(q)?, tgen_count: n })
    }
    //型スキームのTGenをフレッシュな型変数に置き換えたQualを生成
    pub fn fresh_inst(self, ty_info: &mut TypeInfo) -> Qual<Type> {
        match self {
            Scheme::Forall { mut qual, tgen_count } => {
                let fresh_types = (0..tgen_count).map(|_| Type::TyVar(ty_info.no_name_get())).collect::<Vec<_>>();
                qual.t = qual.t.inst(&fresh_types);
                qual
            }
        }
    }
}
