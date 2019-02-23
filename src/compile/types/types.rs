// 型変数のインデックス
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct TypeId(usize);

impl TypeId {
    pub fn new(id: usize) -> TypeId {
        TypeId(id)
    }
    pub fn get_id(&self) -> usize {
        self.0
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    TCon { name: String },
    TGen(usize, TypeId),
    TupleType(Box<TupleType>),
    TyVar(TypeId),
    TApp(Box<TApp>),
    StructType(Box<StructType>),
}

impl Type {
    pub fn create_int32() -> Type {
        Type::TCon { name: "Int32".to_string() }
    }
    pub fn create_bool() -> Type {
        Type::TCon { name: "Bool".to_string() }
    }
    pub fn create_func_type(mut params_ty: Vec<Type>, ret_ty: Type) -> Type {
        if params_ty.len() == 0 { ret_ty } else {
            let param_ty = params_ty.pop().unwrap();
            let ret_ty = Type::TApp(Box::new(TApp(param_ty, ret_ty)));
            Type::create_func_type(params_ty, ret_ty)
        }
    }
    pub fn create_tuple_type(element_tys: Vec<Type>) -> Type {
        Type::TupleType(Box::new(TupleType { element_tys }))
    }
    pub fn get_func_ty(&self) -> &TApp {
        match self {
            Type::TApp(ty) => ty,
            _ => panic!("not FuncType")
        }
    }
    pub fn to_func_ty(self) -> TApp {
        match self {
            Type::TApp(ty) => *ty,
            _ => panic!("not FuncType")
        }
    }
    pub fn get_tuple_ty(&self) -> &TupleType {
        match self {
            Type::TupleType(ty) => ty,
            _ => panic!("not TupleType")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TApp(pub Type, pub Type);


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

//タプルぽく振る舞えるかの制約
pub trait TupleTypeBase {
    fn get_elements_at(&self, index: usize) -> &Type;
    fn get_elements_len(&self) -> usize;
    fn get_elements_from_record_name(&self, _: &String) -> Option<&Type> {
        None
    }
}
