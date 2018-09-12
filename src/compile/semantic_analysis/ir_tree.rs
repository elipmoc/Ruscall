use self::super::global_variable_table::ConfirmGlobalVariableTable;

#[derive(Debug, PartialEq)]
pub struct ProgramIr {
    pub func_list: Vec<FuncIr>,
    pub g_var_table: ConfirmGlobalVariableTable,
}

#[derive(Debug, PartialEq)]
pub struct FuncIr {
    pub name: String,
    pub params: Vec<usize>,
    pub body: ExprIr,
}

impl FuncIr {
    pub fn new(name: String, params: Vec<usize>, body: ExprIr) -> FuncIr {
        FuncIr { name, params, body }
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
        ExprIr::OpIr(Box::new(OpIr { op, l_expr, r_expr }))
    }
    pub fn create_variableir(id: usize) -> ExprIr {
        ExprIr::VariableIr(VariableIr { id })
    }
    pub fn create_global_variableir(id: String) -> ExprIr {
        ExprIr::GlobalVariableIr(GlobalVariableIr { id })
    }
    pub fn create_numir(num: i32) -> ExprIr {
        ExprIr::NumIr(NumIr { num })
    }
    pub fn create_callir(func: ExprIr, params: Vec<ExprIr>) -> ExprIr {
        ExprIr::CallIr(Box::new(CallIr { func, params }))
    }
}

#[derive(Debug, PartialEq)]
pub struct OpIr {
    pub op: String,
    pub l_expr: ExprIr,
    pub r_expr: ExprIr,
}

#[derive(Debug, PartialEq)]
pub struct NumIr {
    pub num: i32
}

#[derive(Debug, PartialEq)]
pub struct VariableIr {
    pub id: usize
}

#[derive(Debug, PartialEq)]
pub struct GlobalVariableIr {
    pub id: String,
}

#[derive(Debug, PartialEq)]
pub struct CallIr {
    pub func: ExprIr,
    pub params: Vec<ExprIr>,
}

#[derive(Debug, PartialEq)]
pub struct FuncProtoIr {
    pub param_size: i32
}

#[derive(Debug, PartialEq)]
pub enum DefGlobalVariableIr {
    Proto(FuncProtoIr)
}