use self::super::global_variable_table::ConfirmGlobalVariableTable;

#[derive(Debug, PartialEq)]
pub struct ProgramIr {
    pub func_list: Vec<FuncIr>,
    pub g_var_table:ConfirmGlobalVariableTable
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
    CallGlobalVariableIr(CallGlobalVariableIr),
}

impl ExprIr {
    pub fn create_opir(op: String, l_expr: ExprIr, r_expr: ExprIr) -> ExprIr {
        ExprIr::OpIr(Box::new(OpIr { op, l_expr, r_expr }))
    }
    pub fn create_variableir(id: usize) -> ExprIr {
        ExprIr::VariableIr(VariableIr { id })
    }
    pub fn create_call_global_variableir(id: String) -> ExprIr { ExprIr::CallGlobalVariableIr(CallGlobalVariableIr { id }) }
    pub fn create_numir(num: i32) -> ExprIr {
        ExprIr::NumIr(NumIr { num })
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
pub struct CallGlobalVariableIr {
    pub id: String
}

#[derive(Debug, PartialEq)]
pub struct FuncProtoIr{
    pub param_size:i32
}

#[derive(Debug, PartialEq)]
pub enum GlobalVariableIr {
    Proto(FuncProtoIr)
}