#[derive(Debug, Clone)]
pub struct ProgramAST {
    pub stmt_list: Vec<StmtAST>,
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
    pub ty: InfixType,
    pub op: String,
    pub priority: i8,
}

#[derive(Debug, Clone)]
pub enum InfixType {
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub struct OpAST {
    op: String,
    l_expr: ExprAST,
    r_expr: ExprAST,
}

#[derive(Debug, Clone)]
pub struct NumAST {
    num: i32,
}
