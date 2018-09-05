use super::super::ast;
use super::ir_tree as ir;

impl ast::ProgramAST {
    pub fn to_ir(self) -> ir::ProgramIr {
        let mut func_list: Vec<ir::FuncIr> = vec![];
        self.stmt_list
            .into_iter()
            .for_each(|stmt| {
                if let Some(func_ir) = stmt.to_ir() {
                    func_list.push(func_ir)
                }
            });
        ir::ProgramIr { func_list }
    }
}

struct VariableTable(Vec<String>);

impl VariableTable {
    fn find_variable_id(&self, name: String) -> Option<usize> {
        self.0.iter().rev().enumerate().find(|(id, name2)| {
            **name2 == name
        }).map(|(id, _)| id)
    }
    fn id_list(&self) -> Vec<usize> {
        (0..self.0.len()).rev().collect()
    }
}

impl ast::StmtAST {
    fn to_ir(self) -> Option<ir::FuncIr> {
        match self {
            ast::StmtAST::DefFuncAST(def_func_ast) => {
                let var_table = VariableTable(def_func_ast.params.into_iter().map(|x| x.id).collect());
                let body_ir = def_func_ast.body.to_ir(&var_table);
                Option::Some(ir::FuncIr::new(def_func_ast.func_name,var_table.id_list(), body_ir))
            }
            x => Option::None
        }
    }
}

impl ast::ExprAST {
    fn to_ir(self, var_table: &VariableTable) -> ir::ExprIr {
        match self {
            ast::ExprAST::NumAST(x) => ir::ExprIr::create_numir(x.num),
            ast::ExprAST::OpAST(x) => {
                let x = *x;
                ir::ExprIr::create_opir(x.op, x.l_expr.to_ir(var_table), x.r_expr.to_ir(var_table))
            }
            ast::ExprAST::VariableAST(x) =>
                ir::ExprIr::create_variableir(var_table.find_variable_id(x.id).unwrap()),
            ast::ExprAST::ParenAST(x) => x.expr.to_ir(var_table)
        }
    }
}

#[test]
fn ast_to_ir_test() {
    let ast = ast::ProgramAST {
        stmt_list: vec![
            ast::StmtAST::DefFuncAST(
                ast::DefFuncAST::new(
                    "hoge".to_string(),
                    vec![
                        ast::VariableAST::new("a".to_string()),
                        ast::VariableAST::new("b".to_string())
                    ],
                    ast::ExprAST::create_variable_ast("b".to_string()),
                )
            )
        ]
    };
    let ir = ir::ProgramIr {
        func_list: vec![
            ir::FuncIr::new("hoge".to_string(),vec![1, 0], ir::ExprIr::create_variableir(0))
        ]
    };
    assert_eq!(ast.to_ir(),ir);
}