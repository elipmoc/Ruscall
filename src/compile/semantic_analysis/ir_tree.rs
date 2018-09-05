#[derive(Debug, PartialEq)]
pub struct ProgramIr {
    pub func_list: Vec<FuncIr>
}

#[derive(Debug, PartialEq)]
pub struct FuncIr {
    pub name:String,
    pub params: Vec<usize>,
    pub body: ExprIr,
}

impl FuncIr {
    pub fn new(name:String,params: Vec<usize>, body: ExprIr) -> FuncIr {
        FuncIr { name,params, body }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprIr {
    OpIr(Box<OpIr>),
    NumIr(NumIr),
    VariableIr(VariableIr),
}

impl ExprIr {
    pub fn create_opir(op: String, l_expr: ExprIr, r_expr: ExprIr) -> ExprIr {
        ExprIr::OpIr(Box::new(OpIr { op, l_expr, r_expr }))
    }
    pub fn create_variableir(id: usize) -> ExprIr {
        ExprIr::VariableIr(VariableIr { id })
    }
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