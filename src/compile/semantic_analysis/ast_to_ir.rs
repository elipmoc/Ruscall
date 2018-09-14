use super::super::ast;
use super::ir_tree as ir;
use super::super::error::Error;
use super::global_variable_table::GlobalVariableTable;

type ResultIr<T> = Result<T, Error>;

impl ast::ProgramAST {
    pub fn to_ir(self) -> ResultIr<(ir::ProgramIr)> {
        let mut func_list: Vec<ir::FuncIr> = vec![];
        let mut g_var_table = GlobalVariableTable::new();
        for stmt in self.stmt_list {
            if let Some(func_ir) = stmt.to_ir(&mut g_var_table)? {
                func_list.push(func_ir)
            }
        }
        match g_var_table.get_confirm() {
            None => Result::Err(Error::new(SourcePosition { line: 0, column: 0 }, "undefined function")),
            Some(g_var_table) => Result::Ok(ir::ProgramIr { func_list, g_var_table })
        }
    }
}

struct VariableTable(Vec<String>);

impl VariableTable {
    fn find_variable_id(&self, name: &String) -> Option<usize> {
        self.0.iter().rev().enumerate().find(|(_, name2)| {
            *name2 == name
        }).map(|(id, _)| id)
    }
    fn id_list(&self) -> Vec<usize> {
        (0..self.0.len()).rev().collect()
    }
}

impl ast::StmtAST {
    fn to_ir(self, g_var_table: &mut GlobalVariableTable) -> ResultIr<Option<ir::FuncIr>> {
        let option = match self {
            ast::StmtAST::DefFuncAST(def_func_ast) => {
                g_var_table.register_func(def_func_ast.func_name.clone(), def_func_ast.params.len());
                let var_table = VariableTable(def_func_ast.params.into_iter().map(|x| x.id).collect());
                let body_ir = def_func_ast.body.to_ir(&var_table, g_var_table);
                Option::Some(ir::FuncIr::new(def_func_ast.func_name, var_table.id_list(), body_ir?))
            }
            _ => Option::None
        };
        Result::Ok(option)
    }
}

impl ast::ExprAST {
    fn to_ir(self, var_table: &VariableTable, g_var_table: &mut GlobalVariableTable) -> ResultIr<ir::ExprIr> {
        let ir =
            match self {
                ast::ExprAST::NumAST(x) => ir::ExprIr::create_numir(x.num),
                ast::ExprAST::OpAST(x) => {
                    let x = *x;
                    ir::ExprIr::create_opir(x.op, x.l_expr.to_ir(var_table, g_var_table)?, x.r_expr.to_ir(var_table, g_var_table)?)
                }
                ast::ExprAST::VariableAST(x) =>
                    match var_table.find_variable_id(&x.id) {
                        Some(x) => ir::ExprIr::create_variableir(x),
                        _ => {
                            g_var_table.register_func_name(x.id.clone());
                            ir::ExprIr::create_global_variableir(x.id)
                        }
                        //return Result::Err(Error::new(x.pos, &("not found param ".to_owned() + &x.id)))
                    },
                ast::ExprAST::ParenAST(x) => x.expr.to_ir(var_table, g_var_table)?,
                ast::ExprAST::FuncCallAST(x) => {
                    let x = *x;
                    let func = x.func.to_ir(var_table, g_var_table)?;
                    if x.params.len() == 0 {
                        func
                    } else {
                        let params: ResultIr<Vec<ir::ExprIr>> =
                            x.params.into_iter()
                                .map(|x| x.to_ir(var_table, g_var_table)).collect();
                        ir::ExprIr::create_callir(func, params?)
                    }
                }
            };
        Result::Ok(ir)
    }
}

pub use combine::stream::state::SourcePosition;
pub use std::collections::HashMap;

#[test]
fn ast_to_ir_test() {
    let ast = ast::ProgramAST {
        stmt_list: vec![
            ast::StmtAST::DefFuncAST(
                ast::DefFuncAST::new(
                    "hoge".to_string(),
                    vec![
                        ast::VariableAST::new("a".to_string(), SourcePosition { column: 0, line: 0 }),
                        ast::VariableAST::new("b".to_string(), SourcePosition { column: 0, line: 0 })
                    ],
                    ast::ExprAST::create_variable_ast("b".to_string(), SourcePosition { column: 0, line: 0 }),
                )
            )
        ]
    };
    let mut g_var_table = HashMap::new();
    g_var_table.insert("hoge".to_string(), 2);
    let ir = ir::ProgramIr {
        func_list: vec![
            ir::FuncIr::new("hoge".to_string(), vec![1, 0], ir::ExprIr::create_variableir(0))
        ],
        g_var_table: ConfirmGlobalVariableTable(g_var_table),
    };
    assert_eq!(ast.to_ir().unwrap(), ir);
}