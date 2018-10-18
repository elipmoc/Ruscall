use self::super::types::*;

pub trait ShowType{
    fn show(&self)->String;
}

pub fn type_error<T,A:ShowType,B:ShowType>(expect: &A, actual: &B) -> Result<T,String> {
    Err(format!("type error!\nexpect: {}\nactual: {}", expect.show(), actual.show()))
}

impl ShowType for Type{
    fn show(&self) -> String {
        match self {
            Type::Int32 => "Int32".to_string(),
            Type::Unknown => "Unknown".to_string(),
            Type::FuncType(x) => x.show(),
            Type::Fn(x) => "Fn(".to_string() + &x.show() + ")",
            Type::TupleType(x) => x.show()
        }
    }
}

impl ShowType for FuncType{
    fn show(&self) -> String {
        self.param_types
            .iter()
            .fold("".to_string(), |acc, x| acc + &x.show() + "->")
            + &self.ret_type.show()
    }

}

impl ShowType for TupleType{
    fn show(&self) -> String {
        "(".to_string()
            + &self.element_tys
            .iter()
            .fold("".to_string(), |acc, x| acc + &x.show() + ",")
            + ")"
    }
}