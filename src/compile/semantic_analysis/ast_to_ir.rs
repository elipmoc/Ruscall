use super::super::ast::*;
use super::ir::*;
use super::type_env::TypeInfo;

impl ProgramAST {
    pub fn to_ir(self) -> (ProgramIr, TypeInfo) {
        let mut lambda_count: usize = 0;
        let mut ty_info = TypeInfo::new();
        let program_ir = self.stmt_list
            .into_iter()
            .fold(
                ProgramIr::empty(),
                |acc, stmt| stmt.to_ir(acc, &mut lambda_count, &mut ty_info),
            );
        (program_ir, ty_info)
    }
}

#[derive(Clone)]
struct VariableTable(Vec<String>);

impl VariableTable {
    fn get_variable_ir(&self, var: VariableAST, ty_info: &mut TypeInfo) -> ExprIr {
        let a = self.0
            .iter().rev().enumerate()
            .find(|(_, name)| *name == &var.id)
            .map(|(id, _)| id);
        match a {
            Some(id) => ExprIr::create_variableir(id, var.pos, ty_info.no_name_get()),
            _ => ExprIr::GlobalVariableIr(var),
        }
    }
}

impl StmtAST {
    fn to_ir(self, program_ir: ProgramIr, lambda_count: &mut usize, ty_info: &mut TypeInfo) -> ProgramIr {
        match self {
            StmtAST::DefFuncAST(x) => x.to_ir(program_ir, lambda_count, ty_info),
            StmtAST::DecFuncAST(x) => x.to_ir(program_ir),
            _ => program_ir,
        }
    }
}

impl DefFuncAST {
    fn to_ir(self, mut program_ir: ProgramIr, lambda_count: &mut usize, ty_info: &mut TypeInfo) -> ProgramIr {
        let params_len: usize =
            if self.params.len() == 0 {
                1
            } else {
                self.params.len()
            };
        let var_table =
            VariableTable(self.params.into_iter().map(|x| x.id).collect());
        let func_ir = FuncIr {
            name: self.func_name,
            body: self.body.to_ir(&mut program_ir, &var_table, lambda_count, ty_info),
            params_len,
            pos: self.pos,
        };
        program_ir.func_list.push(func_ir);
        program_ir
    }
}

impl DecFuncAST {
    fn to_ir(self, mut program_ir: ProgramIr) -> ProgramIr {
        match self.extern_flag {
            true => {
                program_ir.ex_dec_func_list.push(self);
            }
            _ => program_ir.dec_func_list.push(self)
        }
        program_ir
    }
}

impl ExprAST {
    fn to_ir(self, program_ir: &mut ProgramIr, var_table: &VariableTable, lambda_count: &mut usize, ty_info: &mut TypeInfo) -> ExprIr {
        match self {
            ExprAST::NumAST(x) => ExprIr::NumIr(x),
            ExprAST::OpAST(x) => {
                let x = *x;
                ExprIr::create_opir(
                    x.op,
                    x.l_expr.to_ir(program_ir, var_table, lambda_count, ty_info),
                    x.r_expr.to_ir(program_ir, var_table, lambda_count, ty_info),
                )
            }
            ExprAST::VariableAST(x) => var_table.get_variable_ir(x, ty_info),
            ExprAST::ParenAST(x) => x.expr.to_ir(program_ir, var_table, lambda_count, ty_info),
            ExprAST::FuncCallAST(x) => {
                let x = *x;
                let func = x.func.to_ir(program_ir, var_table, lambda_count, ty_info);
                if x.params.len() == 0 {
                    func
                } else {
                    let params =
                        x.params.into_iter().map(|x|
                            x.to_ir(program_ir, var_table, lambda_count, ty_info)
                        ).collect();
                    ExprIr::create_callir(func, params, ty_info.no_name_get())
                }
            }
            ExprAST::TupleAST(x) => {
                let x = *x;
                ExprIr::create_tupleir(
                    x.elements.into_iter()
                        .map(|x|
                            x.to_ir(program_ir, var_table, lambda_count, ty_info)
                        ).collect(),
                    x.pos,
                    ty_info.no_name_get(),
                )
            }
            ExprAST::LambdaAST(x) => x.to_ir(program_ir, var_table, lambda_count, ty_info)
        }
    }
}

impl LambdaAST {
    fn to_ir(self, program_ir: &mut ProgramIr, var_table: &VariableTable, lambda_count: &mut usize, ty_info: &mut TypeInfo) -> ExprIr {
        let env =
            self.env.iter()
                .map(|x|
                    match var_table.get_variable_ir(x.clone(), ty_info) {
                        ExprIr::VariableIr(x) => x,
                        _ => panic!("not VariableIr")
                    }
                )
                .collect();
        let mut var_table = var_table.clone();
        var_table.0.append(&mut self.env.iter().map(|x| x.id.clone()).collect());
        let params_len: usize =
            if self.params.len() == 0 {
                1
            } else {
                self.params.len()
            };
        let params_len = params_len + self.env.len();
        var_table.0.append(&mut self.params.into_iter().map(|x| x.id).collect());
        let body = self.body.to_ir(program_ir, &var_table, lambda_count, ty_info);
        *lambda_count += 1;
        let lambda_name = "#".to_string() + &(*lambda_count - 1).to_string();
        program_ir.func_list.push(FuncIr { params_len, body, pos: self.pos, name: lambda_name.clone() });
        ExprIr::LambdaIr(Box::new(LambdaIr {
            env,
            func_name: lambda_name,
            ty_id: ty_info.no_name_get(),
            pos:self.pos,
            params_len
        }))
    }
}

#[test]
fn ast_to_ir_test() {
    use std::collections::HashMap;
    use combine::stream::state::SourcePosition;
    use compile::types::TypeId;
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
    let mut func_list = vec![];
    func_list.push(
        FuncIr {
            name: "hoge".to_string(),
            body: ExprIr::create_variableir(0, SourcePosition { line: 0, column: 0 }, TypeId::new(0)),
            params_len: 2,
            pos: SourcePosition { column: 0, line: 0 },
        },
    );
    let ir = ProgramIr {
        dec_func_list: vec![],
        func_list,
        ex_dec_func_list: vec![],
    };
    assert_eq!(ast.to_ir().0, ir);
}
