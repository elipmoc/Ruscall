#[derive(Debug, Clone, PartialEq)]
pub struct ProgramAST {
    pub stmt_list: Vec<StmtAST>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtAST {
    RawExpr(String),
    ExprAST(ExprAST),
    InfixAST(InfixAST),
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

//優先順位ごとのinfixのまとまり
#[derive(Debug, Clone)]
pub struct Infixes<'a> {
    pub priority: i8,
    pub list: Vec<&'a InfixAST>,
}

impl<'a> Infixes<'a> {
    pub fn new(priority: i8) -> Infixes<'a> {
        Infixes {
            priority: priority,
            list: vec![],
        }
    }
}

use std::cmp::Ordering;

impl<'a> Ord for Infixes<'a> {
    fn cmp(&self, other: &Infixes) -> Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl<'a> PartialOrd for Infixes<'a> {
    fn partial_cmp(&self, other: &Infixes) -> Option<Ordering> {
        Some(self.priority.cmp(&other.priority))
    }
}

impl<'a> PartialEq for Infixes<'a> {
    fn eq(&self, other: &Infixes) -> bool {
        self.priority == other.priority
    }
}

impl<'a> Eq for Infixes<'a> {}
