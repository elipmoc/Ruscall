use combine::stream::state::SourcePosition;

#[derive(Debug, Clone, PartialEq)]
pub struct ProgramAST {
    pub stmt_list: Vec<StmtAST>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtAST {
    //   ExprAST(ExprAST),
    InfixAST(InfixAST),
    DefFuncAST(DefFuncAST),
    NoneAST,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprAST {
    OpAST(Box<OpAST>),
    NumAST(NumAST),
    VariableAST(VariableAST),
    ParenAST(Box<ParenAST>),
}

impl ExprAST {
    pub fn create_op_ast(
        op: String,
        pos: SourcePosition,
        l_expr: ExprAST,
        r_expr: ExprAST,
    ) -> ExprAST {
        ExprAST::OpAST(Box::new(OpAST::new(op, pos, l_expr, r_expr)))
    }
    pub fn create_num_ast(num: String) -> ExprAST {
        ExprAST::NumAST(NumAST::new(num))
    }
    pub fn create_paren_ast(expr_ast: ExprAST) -> ExprAST {
        ExprAST::ParenAST(Box::new(ParenAST { expr: expr_ast }))
    }
    pub fn create_variable_ast(id: String,pos:SourcePosition) -> ExprAST {
        ExprAST::VariableAST(VariableAST { id ,pos})
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
pub struct ParenAST {
    pub expr: ExprAST,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpAST {
    pub op: String,
    pub l_expr: ExprAST,
    pub r_expr: ExprAST,
    pub pos: SourcePosition,
}

impl OpAST {
    pub fn new(op: String, pos: SourcePosition, l_expr: ExprAST, r_expr: ExprAST) -> OpAST {
        OpAST {
            op: op,
            pos: pos,
            l_expr: l_expr,
            r_expr: r_expr,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableAST {
    pub id: String,
    pub pos:SourcePosition
}

impl VariableAST {
    pub fn new(id: String,pos:SourcePosition) -> VariableAST {
        VariableAST { id,pos}
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefFuncAST {
    pub func_name: String,
    pub params: Vec<VariableAST>,
    pub body: ExprAST,
}

impl DefFuncAST {
    pub fn new(func_name: String, params: Vec<VariableAST>, body: ExprAST) -> DefFuncAST {
        DefFuncAST {
            func_name: func_name,
            params: params,
            body: body,
        }
    }
}
