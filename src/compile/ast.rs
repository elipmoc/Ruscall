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

impl ExprAST {
    pub fn create_num_ast(num: String) -> ExprAST {
        ExprAST::NumAST(NumAST {
            num: num.parse::<i32>().unwrap(),
        })
    }
    pub fn create_op_ast(op: &String, l_expr: &ExprAST, r_expr: &ExprAST) -> ExprAST {
        ExprAST::OpAST(Box::new(OpAST {
            op: op.clone(),
            l_expr: l_expr.clone(),
            r_expr: r_expr.clone(),
        }))
    }
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
    pub op: String,
    pub l_expr: ExprAST,
    pub r_expr: ExprAST,
}

#[derive(Debug, Clone)]
pub struct NumAST {
    pub num: i32,
}
