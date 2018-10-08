use super::super::ast::*;
use super::super::error::Error;
use super::super::types::{FuncType, Type};
use super::ir_tree::*;
use std::collections::HashMap;

type ResultIr<T> = Result<T, Error>;

impl ProgramAST {
    pub fn to_ir(self) -> ProgramIr {
        let empty_ir =
            ProgramIr {
                dec_func_list: vec![],
                func_list: HashMap::new(),
                ex_dec_func_list: HashMap::new(),
            };
        self.stmt_list
            .into_iter()
            .fold(empty_ir, |acc, stmt| stmt.to_ir(acc))
    }
}

struct VariableTable(Vec<String>);

impl VariableTable {
    fn find_variable_id(&self, name: &String) -> Option<usize> {
        self.0
            .iter()
            .rev()
            .enumerate()
            .find(|(_, name2)| *name2 == name)
            .map(|(id, _)| id)
    }
}

impl StmtAST {
    fn to_ir(self, mut program_ir: ProgramIr) -> ProgramIr {
        match self {
            StmtAST::DefFuncAST(def_func_ast) => {
                let func_type = FuncType {
                    param_types: (0..def_func_ast.params.len())
                        .map(|_| Type::Unknown)
                        .collect(),
                    ret_type: Type::Unknown,
                };
                let var_table =
                    VariableTable(def_func_ast.params.into_iter().map(|x| x.id).collect());
                let body_ir = def_func_ast.body.to_ir(&var_table);
                let func_ir = FuncIr {
                    name: def_func_ast.func_name,
                    body: body_ir,
                    ty: func_type,
                    pos: def_func_ast.pos,
                };
                program_ir.func_list.insert(func_ir.name.clone(), func_ir);
            }
            StmtAST::DecFuncAST(x) => {
                if x.extern_flag {
                    program_ir.ex_dec_func_list.insert(
                        x.name.clone(),
                        DecFuncIr {
                            name: x.name,
                            ty: x.ty,
                        },
                    );
                } else {
                    program_ir.dec_func_list.push(DecFuncIr {
                        name: x.name,
                        ty: x.ty,
                    });
                }
            }
            _ => (),
        };
        program_ir
    }
}

impl ExprAST {
    fn to_ir(self, var_table: &VariableTable) -> ExprIr {
        let ir = match self {
            ExprAST::NumAST(x) => ExprIr::create_numir(x.num),
            ExprAST::OpAST(x) => {
                let x = *x;
                ExprIr::create_opir(
                    x.op,
                    x.l_expr.to_ir(var_table),
                    x.r_expr.to_ir(var_table),
                )
            }
            ExprAST::VariableAST(x) => match var_table.find_variable_id(&x.id) {
                Some(id) => ExprIr::create_variableir(id, x.pos),
                _ => ExprIr::create_global_variableir(x.id, x.pos),
            },
            ExprAST::ParenAST(x) => x.expr.to_ir(var_table),
            ExprAST::FuncCallAST(x) => {
                let x = *x;
                let func = x.func.to_ir(var_table);
                if x.params.len() == 0 {
                    func
                } else {
                    let params: Vec<ExprIr> =
                        x.params.into_iter().map(|x| x.to_ir(var_table)).collect();
                    ExprIr::create_callir(func, params)
                }
            }
        };
        ir
    }
}

#[test]
fn ast_to_ir_test() {
    use combine::stream::state::SourcePosition;
    let ast = ProgramAST {
        stmt_list: vec![StmtAST::DefFuncAST(DefFuncAST {
            func_name: "hoge".to_string(),
            params: vec![
                VariableAST::new("a".to_string(), SourcePosition { column: 0, line: 0 }),
                VariableAST::new("b".to_string(), SourcePosition { column: 0, line: 0 }),
            ],
            body: ExprAST::create_variable_ast(
                "b".to_string(),
                SourcePosition { column: 0, line: 0 },
            ),
            pos: SourcePosition { column: 0, line: 0 },
        })],
    };
    let mut func_list = HashMap::new();
    func_list.insert(
        "hoge".to_string(),
        FuncIr {
            name: "hoge".to_string(),
            body: ExprIr::create_variableir(0, SourcePosition { line: 0, column: 0 }),
            ty: FuncType {
                ret_type: Type::Unknown,
                param_types: vec![Type::Unknown, Type::Unknown],
            },
            pos: SourcePosition { column: 0, line: 0 },
        },
    );
    let ir = ProgramIr {
        dec_func_list: vec![],
        func_list,
        ex_dec_func_list: HashMap::new(),
    };
    assert_eq!(ast.to_ir(), ir);
}
