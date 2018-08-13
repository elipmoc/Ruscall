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
        ast::StmtAST::ExprAST(expr_ast) => {
            ast::StmtAST::ExprAST(expr_ast.resolve_op(infix_hash).get_expr_ast())
        }
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

pub enum Resolved {
    NumAST(ast::NumAST),
    OpAST(ast::OpAST, ast::InfixAST),
}

impl Resolved {
    pub fn get_expr_ast(self) -> ast::ExprAST {
        match self {
            Resolved::NumAST(x) => ast::ExprAST::NumAST(x),
            Resolved::OpAST(x, _) => ast::ExprAST::OpAST(Box::new(x)),
        }
    }
}

impl ast::InfixAST {
    //自分より左にある演算子と比べて優先順位が高かったらtrue
    fn is_priority_greater(&self, child: &ast::InfixAST) -> bool {
        if self.priority > child.priority {
            true
        } else if self.priority == child.priority {
            if self.ty == child.ty {
                match self.ty {
                    ast::InfixType::Left => false,
                    ast::InfixType::Right => true,
                }
            } else {
                false
            }
        } else {
            false
        }
    }
}

impl ast::ExprAST {
    pub fn resolve_op(self, infix_hash: &InfixHash) -> Resolved {
        match self {
            ast::ExprAST::OpAST(op_ast) => {
                let mut op_ast = *op_ast;
                let self_infix = infix_hash.get(&op_ast.op).unwrap().clone();

                match op_ast.l_expr.resolve_op(infix_hash) {
                    Resolved::OpAST(mut child_op_ast, child_infix) => {
                        if self_infix.is_priority_greater(&child_infix) {
                            op_ast.l_expr = child_op_ast.r_expr;
                            child_op_ast.r_expr = ast::ExprAST::OpAST(Box::new(op_ast))
                                .resolve_op(infix_hash)
                                .get_expr_ast();
                            Resolved::OpAST(child_op_ast, child_infix)
                        } else {
                            op_ast.l_expr = ast::ExprAST::OpAST(Box::new(child_op_ast));
                            Resolved::OpAST(op_ast, self_infix)
                        }
                    }
                    x => {
                        op_ast.l_expr = x.get_expr_ast();
                        Resolved::OpAST(op_ast, self_infix)
                    }
                }
            }
            ast::ExprAST::NumAST(num_ast) => Resolved::NumAST(num_ast),
        }
    }
}
