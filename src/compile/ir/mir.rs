use combine::stream::state::SourcePosition;
use super::ast::*;
use super::super::types::types::{TypeId, FuncType};
use super::super::semantic_analysis::type_env::TypeInfo;

#[derive(Debug, PartialEq)]
pub struct ProgramMir {
    //関数宣言のリスト
    pub dec_func_list: Vec<DecFuncMir>,
    //関数定義のリスト
    pub func_list: Vec<FuncMir>,
    //extern　宣言された関数のリスト
    pub ex_dec_func_list: Vec<DecFuncMir>,

    pub ty_info: TypeInfo,
}

impl ProgramMir {
    pub fn empty() -> ProgramMir {
        ProgramMir {
            dec_func_list: vec![],
            func_list: vec![],
            ex_dec_func_list: vec![],
            ty_info: TypeInfo::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncMir {
    pub name: String,
    pub body: ExprMir,
    pub params_len: usize,
    pub pos: SourcePosition,
}


#[derive(Debug, PartialEq)]
pub enum ExprMir {
    OpMir(Box<OpMir>),
    NumMir(NumMir),
    BoolMir(BoolMir),
    TupleMir(Box<TupleMir>),
    VariableMir(VariableMir),
    GlobalVariableMir(GlobalVariableMir),
    CallMir(Box<CallMir>),
    LambdaMir(Box<LambdaMir>),
}

impl ExprMir {
    pub fn get_pos(&self) -> SourcePosition {
        match self {
            ExprMir::OpMir(x) => x.l_expr.get_pos(),
            ExprMir::NumMir(x) => x.pos,
            ExprMir::BoolMir(x) => x.pos,
            ExprMir::TupleMir(x) => x.pos,
            ExprMir::VariableMir(x) => x.pos,
            ExprMir::GlobalVariableMir(x) => x.pos,
            ExprMir::CallMir(x) => x.func.get_pos(),
            ExprMir::LambdaMir(x) => x.pos
        }
    }

    pub fn create_op_mir(op: String, l_expr: ExprMir, r_expr: ExprMir) -> ExprMir {
        ExprMir::OpMir(Box::new(OpMir {
            op,
            l_expr,
            r_expr,
        }))
    }
    pub fn create_variable_mir(id: usize, pos: SourcePosition, ty_id: TypeId) -> ExprMir {
        ExprMir::VariableMir(VariableMir { id, pos, ty_id })
    }
    pub fn create_global_variable_mir(id: String, pos: SourcePosition) -> ExprMir {
        ExprMir::GlobalVariableMir(GlobalVariableMir {
            id,
            pos,
        })
    }
    pub fn create_call_mir(func: ExprMir, params: Vec<ExprMir>, ty_id: TypeId) -> ExprMir {
        ExprMir::CallMir(Box::new(CallMir { func, params, ty_id }))
    }

    pub fn create_tuple_mir(elements: Vec<ExprMir>, pos: SourcePosition, ty_id: TypeId) -> ExprMir {
        ExprMir::TupleMir(Box::new(TupleMir { elements, pos, ty_id }))
    }
}

#[derive(Debug, PartialEq)]
pub struct OpMir {
    pub op: String,
    pub l_expr: ExprMir,
    pub r_expr: ExprMir,
}

pub type NumMir = NumAST;
pub type BoolMir = BoolAST;

#[derive(Debug, PartialEq)]
pub struct TupleMir {
    pub elements: Vec<ExprMir>,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Debug, PartialEq)]
pub struct VariableMir {
    pub id: usize,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

pub type GlobalVariableMir = VariableAST;

#[derive(Debug, PartialEq)]
pub struct CallMir {
    pub func: ExprMir,
    pub params: Vec<ExprMir>,
    pub ty_id: TypeId,
}

#[derive(Debug, PartialEq)]
pub struct LambdaMir {
    pub env: Vec<VariableMir>,
    pub func_name: String,
    pub params_len: usize,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecFuncMir {
    pub name: String,
    pub ty: FuncType,
    pub extern_flag: bool,
    pub pos: SourcePosition,
}
