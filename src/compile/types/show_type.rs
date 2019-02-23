use self::super::*;
use std::fmt;

impl fmt::Debug for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.show())
    }
}

pub trait ShowType {
    fn show(&self) -> String;
}

pub fn type_error<T, A: ShowType, B: ShowType>(expect: &A, actual: &B) -> Result<T, String> {
    Err(format!("type error!\nexpect: {}\nactual: {}", expect.show(), actual.show()))
}

impl ShowType for Type {
    fn show(&self) -> String {
        match self {
            Type::TCon { name } => name.clone(),
            Type::TGen(n, ty_id) => format!("TGen({} {:?})", n, ty_id),
            Type::TupleType(x) => x.show(),
            Type::TyVar(ty_id) => ty_id.get_id().to_string(),
            Type::TApp(x) => x.show(),
            Type::StructType(x) => x.show(),
        }
    }
}

impl ShowType for TApp {
    fn show(&self) -> String {
        match self.0 {
            Type::TApp(_) => format!("({})->{}", self.0.show(), self.1.show()),
            _ => format!("{}->{}", self.0.show(), self.1.show())
        }
    }
}

impl ShowType for TupleType {
    fn show(&self) -> String {
        "(".to_string()
            + &self.element_tys
            .iter()
            .fold("".to_string(), |acc, x| acc + &x.show() + ",")
            + ")"
    }
}

impl ShowType for RecordType {
    fn show(&self) -> String {
        "{".to_string()
            + &self.element_tys
            .iter()
            .fold("".to_string(), |acc, (name, ty)| acc + name + ":" + &ty.show() + ",")
            + "}"
    }
}

impl ShowType for StructType {
    fn show(&self) -> String {
        self.name.clone() +
            &match self.ty {
                StructInternalType::TupleType(ref x) => x.show(),
                StructInternalType::RecordType(ref x) => x.show()
            }
    }
}

impl ShowType for ImplItems {
    fn show(&self) -> String {
        "impl{".to_string() + &self.get_index_properties().fold(String::new(), |acc, (name, ty)| {
            acc + &format!(" index {}::{:?},", name, ty)
        }) + &self.get_name_properties().fold(String::new(), |acc, (name, ty)| {
            acc + &format!(" name {}::{:?},", name, ty)
        }) + "}"
    }
}