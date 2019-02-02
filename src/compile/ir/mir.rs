use combine::stream::state::SourcePosition;
use super::ast::*;
use super::super::types::*;
use super::super::semantic_analysis::type_env::TypeInfo;
use indexmap::IndexMap;

#[derive(Debug, PartialEq)]
pub struct ProgramMir {
    //関数定義のリスト
    pub implicit_func_list: IndexMap<String, ImplicitFunc>,
    pub explicit_func_list: Vec<ExplicitFunc>,

    pub ex_dec_func_list: Vec<DecFuncMir>,

    pub ty_info: TypeInfo,

}

impl ProgramMir {
    pub fn empty() -> ProgramMir {
        ProgramMir {
            implicit_func_list: IndexMap::new(),
            explicit_func_list: vec![],
            ex_dec_func_list: vec![],
            ty_info: TypeInfo::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
//明示的に型が分かっている関数
pub struct ExplicitFunc {
    pub func: FuncMir,
    pub scheme: Scheme,
}

#[derive(Debug, PartialEq)]
//型が分からない関数
pub struct ImplicitFunc {
    pub func: FuncMir
}


#[derive(Clone, Debug, PartialEq)]
pub struct FuncMir {
    pub name: String,
    pub body: ExprMir,
    pub params_len: usize,
    pub pos: SourcePosition,
}


#[derive(Clone, Debug, PartialEq)]
pub enum ExprMir {
    OpMir(Box<OpMir>),
    NumMir(NumMir),
    BoolMir(BoolMir),
    IfMir(Box<IfMir>),
    TupleMir(Box<TupleMir>),
    TupleStructMir(Box<TupleStructMir>),
    VariableMir(VariableMir),
    GlobalVariableMir(GlobalVariableMir),
    CallMir(Box<CallMir>),
    LambdaMir(Box<LambdaMir>),
    IndexPropertyMir(Box<IndexPropertyMir>),
    NamePropertyMir(Box<NamePropertyMir>),
}

impl ExprMir {
    pub fn get_pos(&self) -> SourcePosition {
        match self {
            ExprMir::OpMir(x) => x.l_expr.get_pos(),
            ExprMir::NumMir(x) => x.pos,
            ExprMir::BoolMir(x) => x.pos,
            ExprMir::IfMir(x) => x.pos,
            ExprMir::TupleMir(x) => x.pos,
            ExprMir::TupleStructMir(x) => x.tuple.pos,
            ExprMir::VariableMir(x) => x.pos,
            ExprMir::GlobalVariableMir(x) => x.pos,
            ExprMir::CallMir(x) => x.func.get_pos(),
            ExprMir::LambdaMir(x) => x.pos,
            ExprMir::IndexPropertyMir(x) => x.pos,
            ExprMir::NamePropertyMir(x) => x.pos
        }
    }

    pub fn create_op_mir(op: String, l_expr: ExprMir, r_expr: ExprMir) -> ExprMir {
        ExprMir::OpMir(Box::new(OpMir {
            op,
            l_expr,
            r_expr,
        }))
    }
    pub fn create_if_mir(cond: ExprMir, t_expr: ExprMir, f_expr: ExprMir, pos: SourcePosition, ty_id: TypeId) -> ExprMir {
        ExprMir::IfMir(Box::new(
            IfMir { cond, t_expr, f_expr, ty_id, pos }
        ))
    }
    pub fn create_variable_mir(id: usize, pos: SourcePosition, ty_id: TypeId) -> ExprMir {
        ExprMir::VariableMir(VariableMir { id, pos, ty_id })
    }
    pub fn create_global_variable_mir(id: String, pos: SourcePosition, ty_id: TypeId) -> ExprMir {
        ExprMir::GlobalVariableMir(GlobalVariableMir {
            id,
            pos,
            ty_id,
        })
    }
    pub fn create_call_mir(func: ExprMir, params: Vec<ExprMir>, ty_id: TypeId) -> ExprMir {
        ExprMir::CallMir(Box::new(CallMir { func, params, ty_id }))
    }

    pub fn create_tuple_mir(elements: Vec<ExprMir>, pos: SourcePosition, ty_id: TypeId) -> ExprMir {
        ExprMir::TupleMir(Box::new(TupleMir { elements, pos, ty_id }))
    }
    pub fn create_tuple_struct_mir(elements: Vec<ExprMir>, pos: SourcePosition, struct_ty: StructType, ty_id: TypeId) -> ExprMir {
        ExprMir::TupleStructMir(Box::new(TupleStructMir {
            tuple: TupleMir { elements, pos, ty_id },
            ty: struct_ty,
        }))
    }
    pub fn create_index_property_mir(expr: ExprMir, pos: SourcePosition, ty_id: TypeId, index: u32) -> ExprMir {
        ExprMir::IndexPropertyMir(Box::new(
            IndexPropertyMir {
                expr,
                pos,
                index,
                ty_id,
            }
        ))
    }
    pub fn create_name_property_mir(expr: ExprMir, pos: SourcePosition, ty_id: TypeId, property_name: String) -> ExprMir {
        ExprMir::NamePropertyMir(Box::new(
            NamePropertyMir {
                expr,
                pos,
                property_name,
                ty_id,
            }
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct OpMir {
    pub op: String,
    pub l_expr: ExprMir,
    pub r_expr: ExprMir,
}

pub type NumMir = NumAST;
pub type BoolMir = BoolAST;

#[derive(Clone, Debug, PartialEq)]
pub struct IfMir {
    pub cond: ExprMir,
    pub t_expr: ExprMir,
    pub f_expr: ExprMir,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleMir {
    pub elements: Vec<ExprMir>,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleStructMir {
    pub tuple: TupleMir,
    pub ty: StructType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IndexPropertyMir {
    pub expr: ExprMir,
    pub index: u32,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamePropertyMir {
    pub expr: ExprMir,
    pub property_name: String,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariableMir {
    pub id: usize,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GlobalVariableMir {
    pub id: String,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallMir {
    pub func: ExprMir,
    pub params: Vec<ExprMir>,
    pub ty_id: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LambdaMir {
    pub env: Vec<VariableMir>,
    pub func_name: String,
    pub params_len: usize,
    pub pos: SourcePosition,
    pub ty_id: TypeId,
    pub func_id: TypeId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DecFuncMir {
    pub name: String,
    pub ty: Qual<FuncType>,
    pub extern_flag: bool,
    pub pos: SourcePosition,
}
