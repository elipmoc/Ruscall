use combine::stream::state::SourcePosition;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct ProgramIr {
    pub dec_func_list: Vec<DecFuncIr>,

    pub func_list: HashMap<String, FuncIr>,
    //extern　宣言された関数のリスト
    pub ex_dec_func_list: HashMap<String, DecFuncIr>,
}

impl ProgramIr {
    pub fn empty() -> ProgramIr {
        ProgramIr {
            dec_func_list: vec![],
            func_list: HashMap::new(),
            ex_dec_func_list: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncIr {
    pub name: String,
    pub body: ExprIr,
    pub pamrams_len: usize,
    pub pos: SourcePosition,
}

#[derive(Debug, PartialEq)]
pub enum ExprIr {
    OpIr(Box<OpIr>),
    NumIr(NumIr),
    TupleIr(Box<TupleIr>),
    VariableIr(VariableIr),
    GlobalVariableIr(GlobalVariableIr),
    CallIr(Box<CallIr>),
}

impl ExprIr {
    pub fn create_opir(op: String, l_expr: ExprIr, r_expr: ExprIr) -> ExprIr {
        ExprIr::OpIr(Box::new(OpIr {
            op,
            l_expr,
            r_expr,
        }))
    }
    pub fn create_variableir(id: usize, pos: SourcePosition) -> ExprIr {
        ExprIr::VariableIr(VariableIr {
            id,
            pos,
        })
    }
    pub fn create_global_variableir(id: String, pos: SourcePosition) -> ExprIr {
        ExprIr::GlobalVariableIr(GlobalVariableIr {
            id,
            pos,
        })
    }
    pub fn create_numir(num: i32) -> ExprIr {
        ExprIr::NumIr(NumIr {
            num,
        })
    }
    pub fn create_callir(func: ExprIr, params: Vec<ExprIr>) -> ExprIr {
        ExprIr::CallIr(Box::new(CallIr {
            func,
            params,
        }))
    }

    pub fn create_tupleir(elements: Vec<ExprIr>, pos: SourcePosition) -> ExprIr {
        ExprIr::TupleIr(Box::new(TupleIr {
            elements,
            pos,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub struct OpIr {
    pub op: String,
    pub l_expr: ExprIr,
    pub r_expr: ExprIr,
}

#[derive(Debug, PartialEq)]
pub struct NumIr {
    pub num: i32,
}

#[derive(Debug, PartialEq)]
pub struct TupleIr {
    pub elements: Vec<ExprIr>,
    pub pos: SourcePosition,
}

#[derive(Debug, PartialEq)]
pub struct VariableIr {
    pub id: usize,
    pub pos: SourcePosition,
}

#[derive(Debug, PartialEq)]
pub struct GlobalVariableIr {
    pub id: String,
    pub pos: SourcePosition,
}

#[derive(Debug, PartialEq)]
pub struct CallIr {
    pub func: ExprIr,
    pub params: Vec<ExprIr>,
}

use super::super::ast::DecFuncAST;

pub type DecFuncIr = DecFuncAST;
