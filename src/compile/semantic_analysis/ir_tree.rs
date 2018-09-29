use self::super::super::types::Type;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct ProgramIr {
    pub func_list: HashMap<String, FuncIr>,
    pub extern_func_list: HashMap<String, DecFuncIr>
}

#[derive(Debug, PartialEq)]
pub struct FuncIr {
    pub name: String,
    pub params: Vec<(usize, Type)>,
    pub body: ExprIr,
    pub ty: Type,
}

impl FuncIr {
    pub fn new(name: String, params: Vec<(usize, Type)>, body: ExprIr, ty: Type) -> FuncIr {
        FuncIr { name, params, body, ty }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprIr {
    OpIr(Box<OpIr>),
    NumIr(NumIr),
    VariableIr(VariableIr),
    GlobalVariableIr(GlobalVariableIr),
    CallIr(Box<CallIr>),
}

impl ExprIr {
    pub fn create_opir(op: String, l_expr: ExprIr, r_expr: ExprIr) -> ExprIr {
        ExprIr::OpIr(Box::new(OpIr { op, l_expr, r_expr, ty: Type::Unknown }))
    }
    pub fn create_variableir(id: usize) -> ExprIr {
        ExprIr::VariableIr(VariableIr { id, ty: Type::Unknown })
    }
    pub fn create_global_variableir(id: String) -> ExprIr {
        ExprIr::GlobalVariableIr(GlobalVariableIr { id, ty: Type::Unknown })
    }
    pub fn create_numir(num: i32) -> ExprIr {
        ExprIr::NumIr(NumIr { num, ty: Type::Unknown })
    }
    pub fn create_callir(func: ExprIr, params: Vec<ExprIr>) -> ExprIr {
        ExprIr::CallIr(Box::new(CallIr { func, params, ty: Type::Unknown }))
    }

    pub fn get_ty(&self) -> &Type {
        match self {
            ExprIr::VariableIr(x) => &x.ty,
            ExprIr::OpIr(x) => &x.ty,
            ExprIr::NumIr(x) => &x.ty,
            ExprIr::CallIr(x) => &x.ty,
            ExprIr::GlobalVariableIr(x) => &x.ty
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct OpIr {
    pub op: String,
    pub l_expr: ExprIr,
    pub r_expr: ExprIr,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct NumIr {
    pub num: i32,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct VariableIr {
    pub id: usize,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct GlobalVariableIr {
    pub id: String,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct CallIr {
    pub func: ExprIr,
    pub params: Vec<ExprIr>,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub struct DecFuncIr {
    pub ty: Type,
    pub name: String,
}