pub struct ProgramIr {
    func_list: Vec<FuncIr>
}

pub struct FuncIr {
    params: Vec<usize>,
    body: ExprIr,
}

impl FuncIr {
    pub fn new(params: Vec<usize>, body: ExprIr) -> FuncIr {
        FuncIr { params, body }
    }
}

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

pub struct OpIr {
    pub op: String,
    pub l_expr: ExprIr,
    pub r_expr: ExprIr,
}

pub struct NumIr {
    pub num: i32
}

pub struct VariableIr {
    id: usize
}