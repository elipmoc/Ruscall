use combine::stream::state::SourcePosition;
use super::super::ast::*;
use super::super::types::{TypeId, FuncType};
use super::type_env::TypeInfo;

#[derive(Debug, PartialEq)]
pub struct ProgramIr {
    //関数宣言のリスト
    pub dec_func_list: Vec<DecFuncIr>,
    //関数定義のリスト
    pub func_list: Vec<FuncIr>,
    //extern　宣言された関数のリスト
    pub ex_dec_func_list: Vec<DecFuncIr>,

    pub ty_info: TypeInfo,
}

impl ProgramIr {
    pub fn empty() -> ProgramIr {
        ProgramIr {
            dec_func_list: vec![],
            func_list: vec![],
            ex_dec_func_list: vec![],
            ty_info: TypeInfo::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncIr {
    pub name: String,
    pub body: ExprIr,
    pub params_len: usize,
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
    LambdaIr(Box<LambdaIr>),
}

impl ExprIr {
    pub fn get_pos(&self) -> SourcePosition {
        match self {
            ExprIr::OpIr(x) => x.l_expr.get_pos(),
            ExprIr::NumIr(x) => x.pos,
            ExprIr::TupleIr(x) => x.pos,
            ExprIr::VariableIr(x) => x.pos,
            ExprIr::GlobalVariableIr(x) => x.pos,
            ExprIr::CallIr(x) => x.func.get_pos(),
            ExprIr::LambdaIr(x) => x.pos
        }
    }

    pub fn create_opir(op: String, l_expr: ExprIr, r_expr: ExprIr) -> ExprIr {
        ExprIr::OpIr(Box::new(OpIr {
            op,
            l_expr,
            r_expr,
        }))
    }
    pub fn create_variableir(id: usize, pos: SourcePosition, ty_id: TypeId) -> ExprIr {
        ExprIr::VariableIr(VariableIr { id, pos, ty_id })
    }
    pub fn create_global_variableir(id: String, pos: SourcePosition) -> ExprIr {
        ExprIr::GlobalVariableIr(GlobalVariableIr {
            id,
            pos,
        })
    }
    pub fn create_callir(func: ExprIr, params: Vec<ExprIr>, ty_id: TypeId) -> ExprIr {
        ExprIr::CallIr(Box::new(CallIr { func, params, ty_id }))
    }

    pub fn create_tupleir(elements: Vec<ExprIr>, pos: SourcePosition, ty_id: TypeId) -> ExprIr {
        ExprIr::TupleIr(Box::new(TupleIr { elements, pos, ty_id }))
    }
}

#[derive(Debug, PartialEq)]
pub struct OpIr {
    pub op: String,
    pub l_expr: ExprIr,
    pub r_expr: ExprIr,
}

pub type NumIr = NumAST;

#[derive(Debug, PartialEq)]
pub struct TupleIr {
    pub elements: Vec<ExprIr>,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Debug, PartialEq)]
pub struct VariableIr {
    pub id: usize,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

pub type GlobalVariableIr = VariableAST;

#[derive(Debug, PartialEq)]
pub struct CallIr {
    pub func: ExprIr,
    pub params: Vec<ExprIr>,
    pub ty_id: TypeId,
}

#[derive(Debug, PartialEq)]
pub struct LambdaIr {
    pub env: Vec<VariableIr>,
    pub func_name: String,
    pub params_len: usize,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecFuncIr {
    pub name: String,
    pub ty: FuncType,
    pub extern_flag: bool,
    pub pos: SourcePosition,
}
