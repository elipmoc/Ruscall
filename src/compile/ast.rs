#[derive(Debug, Clone, PartialEq)]
pub struct ProgramAST {
    pub stmt_list: Vec<StmtAST>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtAST {
    RawExpr(String),
    ExprAST(ExprAST),
    InfixAST(InfixAST),
    NoneAST,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprAST {
    OpAST(Box<OpAST>),
    NumAST(NumAST),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixAST {
    pub ty: InfixType,
    pub op: String,
    pub priority: i8,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InfixType {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumAST {
    pub num: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpAST {
    pub op: String,
    pub l_expr: ExprAST,
    pub r_expr: ExprAST,
}

impl OpAST {
    pub fn new(op: String, l_expr: ExprAST, r_expr: ExprAST) -> OpAST {
        OpAST {
            op: op,
            l_expr: l_expr,
            r_expr: r_expr,
        }
    }
}
