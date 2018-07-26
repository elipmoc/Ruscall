#[derive(Debug, Clone)]
pub struct ProgramAST {
    stmt_list: Vec<StmtAST>,
}

#[derive(Debug, Clone)]
pub enum StmtAST {
    ExprAST(ExprAST),
    InfixAST(InfixAST),
}

#[derive(Debug, Clone)]
pub enum ExprAST {
    NumAST(NumAST),
    OpAST(Box<OpAST>),
}

#[derive(Debug, Clone)]
pub struct InfixAST {
    ty: InfixType,
    priority: i8,
}

#[derive(Debug, Clone)]
pub enum InfixType {
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub struct OpAST {
    op: char,
    l_expr: ExprAST,
    r_expr: ExprAST,
}

#[derive(Debug, Clone)]
pub struct NumAST {
    num: i32,
}
