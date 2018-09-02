pub struct ProgramIr {
    func_list: Vec<FuncIr>
}

pub struct FuncIr {
    body: ExprIr
}

pub enum ExprIr {
    OpIr(Box<OpIr>),
    NumIr(NumIr),
    VariableIr(VariableIr),
}

pub struct OpIr {
    op: String,
    l_expr: ExprIr,
    r_expr: ExprIr,
}

pub struct NumIr {
    num: i32
}

pub struct VariableIr{
    id:i32
}