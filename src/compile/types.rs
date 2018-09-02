enum Type {
    Int32,
    FuncType(Box<FuncType>),
}

struct FuncType {
    ret_type: Type,
}

impl FuncType {
    pub fn get_ret_type(self) -> Type {
        self.ret_type
    }
}
