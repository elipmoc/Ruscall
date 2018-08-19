use super::ast;
use combine::stream::state::SourcePosition;
use std::collections::HashMap;

type InfixHash = HashMap<String, ast::InfixAST>;
type ResolveResult<T> = Result<T, SourcePosition>;

//RawExprをOpASTに置き換えたProgramASTを得る
pub fn resolve_op(ast: ast::ProgramAST) -> ResolveResult<ast::ProgramAST> {
    let mut infix_hash: InfixHash = HashMap::new();
    let mut new_stmt_list = Vec::with_capacity(ast.stmt_list.len());
    for stmt in ast.stmt_list {
        new_stmt_list.push(stmt_resolve_op(&mut infix_hash, stmt)?);
    }
    Result::Ok(ast::ProgramAST {
        stmt_list: new_stmt_list,
    })
}

fn stmt_resolve_op(infix_hash: &mut InfixHash, stmt: ast::StmtAST) -> ResolveResult<ast::StmtAST> {
    let new_stmt = match stmt {
        ast::StmtAST::ExprAST(expr_ast) => {
            ast::StmtAST::ExprAST(expr_ast.resolve_op(infix_hash)?.get_expr_ast())
        }
        ast::StmtAST::InfixAST(infix) => {
            regist_infix(infix_hash, infix);
            ast::StmtAST::NoneAST
        }
        _ => panic!("bug!!!"),
    };
    Result::Ok(new_stmt)
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
            return true;
        }
        if self.priority == child.priority {
            if self.ty == child.ty {
                return match self.ty {
                    ast::InfixType::Left => false,
                    ast::InfixType::Right => true,
                };
            }
        }
        return false;
    }
}

impl ast::ExprAST {
    pub fn resolve_op(self, infix_hash: &InfixHash) -> ResolveResult<Resolved> {
        let resolved = match self {
            ast::ExprAST::OpAST(op_ast) => {
                let mut op_ast = *op_ast;
                let self_infix = match infix_hash.get(&op_ast.op) {
                    None => return Result::Err(op_ast.pos),
                    Some(x) => x.clone(),
                };

                match op_ast.l_expr.resolve_op(infix_hash)? {
                    Resolved::OpAST(mut child_op_ast, child_infix) => {
                        if self_infix.is_priority_greater(&child_infix) {
                            op_ast.l_expr = child_op_ast.r_expr;
                            child_op_ast.r_expr = ast::ExprAST::OpAST(Box::new(op_ast))
                                .resolve_op(infix_hash)?
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
        };
        Result::Ok(resolved)
    }
}
