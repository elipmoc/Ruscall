#[derive(Debug, Clone, PartialEq)]
pub struct ProgramAST {
    pub stmt_list: Vec<StmtAST>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtAST {
    ExprAST(ExprAST),
    InfixAST(InfixAST),
}

impl StmtAST {
    pub fn create_infixl_ast(op: String, priority: i8) -> StmtAST {
        StmtAST::InfixAST(InfixAST {
            op: op,
            priority: priority,
            ty: InfixType::Left,
        })
    }
    pub fn create_infixr_ast(op: String, priority: i8) -> StmtAST {
        StmtAST::InfixAST(InfixAST {
            op: op,
            priority: priority,
            ty: InfixType::Right,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprAST {
    OpTokenListAST(OpTokenListAST),
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
pub enum OpTokenAST {
    NumAST(NumAST),
    Op(String),
}

impl OpTokenAST {
    pub fn create_num_ast(num: String) -> OpTokenAST {
        OpTokenAST::NumAST(NumAST {
            num: num.parse::<i32>().unwrap(),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpTokenListAST {
    pub op_token_list: Vec<OpTokenAST>,
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

//優先順位ごとのinfixのまとまり
pub struct Infixes {
    pub priority: i8,
    pub list: Vec<InfixAST>,
}
use std::cmp::Ordering;

impl Ord for Infixes {
    fn cmp(&self, other: &Infixes) -> Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PartialOrd for Infixes {
    fn partial_cmp(&self, other: &Infixes) -> Option<Ordering> {
        Some(self.priority.cmp(&other.priority))
    }
}

impl PartialEq for Infixes {
    fn eq(&self, other: &Infixes) -> bool {
        self.priority == other.priority
    }
}
impl Eq for Infixes {}

impl OpAST {
    pub fn create_op_ast(infix_list: &mut Vec<Infixes>, op_token_list: OpTokenListAST) {
        infix_list.sort();
    }
    fn create_op_ast2(index: usize) {}
}
