#[derive(Debug, Clone, PartialEq)]
pub struct ProgramAST {
    pub stmt_list: Vec<StmtAST>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtAST {
    ExprAST(ExprAST),
    InfixAST(InfixAST),
    NoneAST,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprAST {
    OpAST(Box<OpAST>),
    NumAST(NumAST),
}

impl ExprAST {
    pub fn create_op_ast(op: String, l_expr: ExprAST, r_expr: ExprAST) -> ExprAST {
        ExprAST::OpAST(Box::new(OpAST::new(op, l_expr, r_expr)))
    }
    pub fn create_num_ast(num: String) -> ExprAST {
        ExprAST::NumAST(NumAST::new(num))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Priority(pub i8);

#[derive(Debug, Clone, PartialEq)]
pub struct InfixAST {
    pub ty: InfixType,
    pub op: String,
    pub priority: Priority,
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

impl NumAST {
    pub fn new(num: String) -> NumAST {
        NumAST {
            num: num.parse().unwrap(),
        }
    }
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
