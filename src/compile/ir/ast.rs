use super::super::types::types::*;
use combine::stream::state::SourcePosition;

#[derive(Debug, Clone, PartialEq)]
pub struct ProgramAST {
    pub stmt_list: Vec<StmtAST>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtAST {
    InfixAST(InfixAST),
    DecStructAST(DecStructAST),
    DefFuncAST(DefFuncAST),
    DecFuncAST(DecFuncAST),
    NoneAST,
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
pub struct DecStructAST {
    pub pos: SourcePosition,
    pub ty: StructTypeAST,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprAST {
    OpAST(Box<OpAST>),
    NumAST(NumAST),
    BoolAST(BoolAST),
    IfAST(Box<IfAST>),
    VariableAST(VariableAST),
    ParenAST(Box<ParenAST>),
    FuncCallAST(Box<FuncCallAST>),
    NamedParamsConstructorCallAST(Box<NamedParamsConstructorCallAST>),
    TupleAST(Box<TupleAST>),
    TupleStructAST(Box<TupleStructAST>),
    TuplePropertyAST(Box<TuplePropertyAST>),
    LambdaAST(Box<LambdaAST>),
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
    pub fn create_num_ast(num: String, pos: SourcePosition) -> ExprAST {
        ExprAST::NumAST(NumAST::new(num, pos))
    }
    pub fn create_bool_ast(bool: bool, pos: SourcePosition) -> ExprAST {
        ExprAST::BoolAST(BoolAST { bool, pos })
    }
    pub fn create_if_ast(cond: ExprAST, t_expr: ExprAST, f_expr: ExprAST, pos: SourcePosition) -> ExprAST {
        ExprAST::IfAST(Box::new(
            IfAST { cond, t_expr, f_expr, pos }
        ))
    }
    pub fn create_paren_ast(expr_ast: ExprAST) -> ExprAST {
        ExprAST::ParenAST(Box::new(ParenAST { expr: expr_ast }))
    }
    pub fn create_variable_ast(id: String, pos: SourcePosition) -> ExprAST {
        ExprAST::VariableAST(VariableAST { id, pos })
    }
    pub fn create_func_call_ast(func: ExprAST, param: ExprAST) -> ExprAST {
        ExprAST::FuncCallAST(Box::new(FuncCallAST { func, param }))
    }
    pub fn create_named_params_constructor_call_ast(constructor_name: String, params: Vec<(String, ExprAST)>, pos: SourcePosition) -> ExprAST {
        ExprAST::NamedParamsConstructorCallAST(Box::new(NamedParamsConstructorCallAST { constructor_name, params, pos }))
    }
    pub fn create_tuple_ast(elements: Vec<ExprAST>, pos: SourcePosition) -> ExprAST {
        ExprAST::TupleAST(Box::new(TupleAST { elements, pos }))
    }
    pub fn create_tuple_struct_ast(elements: Vec<ExprAST>, ty: StructTypeAST) -> ExprAST {
        ExprAST::TupleStructAST(Box::new(TupleStructAST {
            tuple: TupleAST { elements, pos: SourcePosition::new() },
            ty,
        }))
    }
    pub fn create_tuple_property_ast(index: String, expr: ExprAST, pos: SourcePosition) -> ExprAST {
        ExprAST::TuplePropertyAST(Box::new(TuplePropertyAST {
            index: index.parse().unwrap(),
            expr,
            pos,
        }))
    }
    pub fn create_lambda_ast(env: Vec<VariableAST>, params: Vec<VariableAST>, body: ExprAST, pos: SourcePosition) -> ExprAST {
        ExprAST::LambdaAST(Box::new(LambdaAST { env, params, body, pos }))
    }

    pub fn get_pos(&self) -> SourcePosition {
        match self {
            ExprAST::NumAST(x) => x.pos,
            ExprAST::BoolAST(x) => x.pos,
            ExprAST::IfAST(x) => x.pos,
            ExprAST::OpAST(x) => x.pos,
            ExprAST::ParenAST(x) => x.expr.get_pos(),
            ExprAST::VariableAST(x) => x.pos,
            ExprAST::FuncCallAST(x) => x.func.get_pos(),
            ExprAST::NamedParamsConstructorCallAST(x) => x.pos,
            ExprAST::TupleAST(x) => x.pos,
            ExprAST::TupleStructAST(x) => x.tuple.pos,
            ExprAST::LambdaAST(x) => x.pos,
            ExprAST::TuplePropertyAST(x) => x.pos
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumAST {
    pub num: i32,
    pub pos: SourcePosition,
}

impl NumAST {
    pub fn new(num: String, pos: SourcePosition) -> NumAST {
        NumAST {
            num: num.parse().unwrap(),
            pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoolAST {
    pub bool: bool,
    pub pos: SourcePosition,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfAST {
    pub cond: ExprAST,
    pub t_expr: ExprAST,
    pub f_expr: ExprAST,
    pub pos: SourcePosition,
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
            op,
            pos,
            l_expr,
            r_expr,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableAST {
    pub id: String,
    pub pos: SourcePosition,
}

impl VariableAST {
    pub fn new(id: String, pos: SourcePosition) -> VariableAST {
        VariableAST { id, pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParenAST {
    pub expr: ExprAST,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallAST {
    pub func: ExprAST,
    pub param: ExprAST,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedParamsConstructorCallAST {
    pub constructor_name: String,
    pub params: Vec<(String, ExprAST)>,
    pub pos: SourcePosition,

}

#[derive(Debug, Clone, PartialEq)]
pub struct DefFuncAST {
    pub name: String,
    pub params: Vec<VariableAST>,
    pub body: ExprAST,
    pub pos: SourcePosition,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecFuncAST {
    pub name: String,
    pub ty: FuncTypeAST,
    pub extern_flag: bool,
    pub pos: SourcePosition,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleAST {
    pub elements: Vec<ExprAST>,
    pub pos: SourcePosition,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleStructAST {
    pub tuple: TupleAST,
    pub ty: StructTypeAST,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePropertyAST {
    pub expr: ExprAST,
    pub index: u32,
    pub pos: SourcePosition,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaAST {
    pub env: Vec<VariableAST>,
    pub params: Vec<VariableAST>,
    pub body: ExprAST,
    pub pos: SourcePosition,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAST {
    Type(Type),
    IdTypeAST(String),
    FuncTypeAST(Box<FuncTypeAST>),
    TupleTypeAST(Box<TupleTypeAST>),
    TypeVarName(String),
    StructTypeAST(StructTypeAST),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncTypeAST {
    pub params_ty: Vec<TypeAST>,
    pub ret_ty: TypeAST,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleTypeAST {
    pub elements_ty: Vec<TypeAST>
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructTypeAST {
    pub ty: StructInternalTypeAST,
    pub name: String,
}


#[derive(Debug, Clone, PartialEq)]
pub enum StructInternalTypeAST {
    RecordTypeAST(RecordTypeAST),
    TupleTypeAST(TupleTypeAST),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordTypeAST {
    pub elements_ty: Vec<(String, TypeAST)>,
}



