use super::ast;
use std::collections::HashMap;

type InfixHash = HashMap<String, ast::InfixAST>;

//RawExprをOpASTに置き換えたProgramASTを得る
pub fn resolve_op(ast: ast::ProgramAST) -> ast::ProgramAST {
    let mut infix_hash: InfixHash = HashMap::new();
    ast::ProgramAST {
        stmt_list: ast
            .stmt_list
            .into_iter()
            .map(|stmt| stmt_resolve_op(&mut infix_hash, stmt))
            .collect(),
    }
}

fn stmt_resolve_op(infix_hash: &mut InfixHash, stmt: ast::StmtAST) -> ast::StmtAST {
    match stmt {
        ast::StmtAST::ExprAST(expr_ast) => ast::StmtAST::ExprAST(expr_ast.resolve_op(infix_hash).0),
        ast::StmtAST::InfixAST(infix) => {
            regist_infix(infix_hash, infix);
            ast::StmtAST::NoneAST
        }
        _ => panic!("bug!!!"),
    }
}

fn regist_infix(infix_hash: &mut InfixHash, infix: ast::InfixAST) {
    infix_hash.insert(infix.op.clone(), infix);
}

impl ast::ExprAST {
    pub fn resolve_op(self, infix_hash: &InfixHash) -> (ast::ExprAST, ast::Priority) {
        match self {
            ast::ExprAST::OpAST(op_ast) => {
                let mut op_ast = *op_ast;
                let self_priority = infix_hash.get(&op_ast.op).unwrap().priority;
                let (mut child_expr, child_priority) = op_ast.l_expr.resolve_op(infix_hash);
                if self_priority > child_priority {
                    match child_expr {
                        ast::ExprAST::OpAST(child_op_ast) => {
                            let mut child_op_ast = *child_op_ast;
                            op_ast.l_expr = child_op_ast.r_expr;
                            child_op_ast.r_expr = ast::ExprAST::OpAST(Box::new(op_ast));
                            (ast::ExprAST::OpAST(Box::new(child_op_ast)), child_priority)
                        }
                        _ => panic!("error"),
                    }
                } else {
                    op_ast.l_expr = child_expr;
                    (ast::ExprAST::OpAST(Box::new(op_ast)), self_priority)
                }
            }
            _ => (self, ast::Priority(100)),
        }
    }
}
