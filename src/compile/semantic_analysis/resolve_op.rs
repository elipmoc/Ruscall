use super::super::ast;
use super::super::Error;
use std::collections::HashMap;

type InfixHash = HashMap<String, ast::InfixAST>;
type ResolveResult<T> = Result<T, Error>;

//RawExprをOpASTに置き換えたProgramASTを得る
pub fn resolve_op(ast: ast::ProgramAST) -> ResolveResult<ast::ProgramAST> {
    let mut infix_hash: InfixHash = HashMap::new();
    let mut new_stmt_list = Vec::with_capacity(ast.stmt_list.len());
    for stmt in ast.stmt_list {
        new_stmt_list.push(stmt.resolve_op(&mut infix_hash)?);
    }
    Result::Ok(ast::ProgramAST {
        stmt_list: new_stmt_list,
    })
}

impl ast::StmtAST {
    fn resolve_op(self, infix_hash: &mut InfixHash) -> ResolveResult<ast::StmtAST> {
        let new_stmt = match self {
            ast::StmtAST::DefFuncAST(def_func_ast) => {
                ast::StmtAST::DefFuncAST(def_func_ast.resolve_op(infix_hash)?)
            }
            ast::StmtAST::InfixAST(infix) => {
                regist_infix(infix_hash, infix);
                ast::StmtAST::NoneAST
            }
            x => x,
        };
        Result::Ok(new_stmt)
    }
}

impl ast::DefFuncAST {
    fn resolve_op(self, infix_hash: &InfixHash) -> ResolveResult<ast::DefFuncAST> {
        Result::Ok(ast::DefFuncAST {
            body: self.body.resolve_op(infix_hash)?.get_expr_ast(),
            ..self
        })
    }
}

fn regist_infix(infix_hash: &mut InfixHash, infix: ast::InfixAST) {
    infix_hash.insert(infix.op.clone(), infix);
}

 enum Resolved {
    OtherExprAST(ast::ExprAST),
    OpAST(ast::OpAST, ast::InfixAST),
}

impl Resolved {
     fn get_expr_ast(self) -> ast::ExprAST {
        match self {
            Resolved::OpAST(x, _) => ast::ExprAST::OpAST(Box::new(x)),
            Resolved::OtherExprAST(x) => x,
        }
    }
}

impl ast::InfixAST {
    //自分より左にある演算子と比べて優先順位が高かったらtrue
    fn is_priority_greater(&self, child: &ast::InfixAST) -> bool {
        if self.priority > child.priority {
            return true;
        }
        if self.priority == child.priority && self.ty == child.ty {
            return match self.ty {
                ast::InfixType::Left => false,
                ast::InfixType::Right => true,
            };
        }
        return false;
    }
}

impl ast::OpAST {
    fn swap_op(mut self, infix_hash: &InfixHash) -> ResolveResult<Resolved> {
        let self_infix = match infix_hash.get(&self.op) {
            None => return Result::Err(Error::new(self.pos, "no declare op")),
            Some(x) => x.clone(),
        };

        let resolved = match self.l_expr.resolve_op(infix_hash)? {
            Resolved::OpAST(mut child_op_ast, child_infix) => {
                if self_infix.is_priority_greater(&child_infix) {
                    self.l_expr = child_op_ast.r_expr;
                    child_op_ast.r_expr = ast::ExprAST::OpAST(Box::new(self))
                        .resolve_op(infix_hash)?
                        .get_expr_ast();
                    Resolved::OpAST(child_op_ast, child_infix)
                } else {
                    self.l_expr = ast::ExprAST::OpAST(Box::new(child_op_ast));
                    Resolved::OpAST(self, self_infix)
                }
            }
            x => {
                self.l_expr = x.get_expr_ast();
                self.r_expr = self.r_expr.resolve_op(infix_hash)?.get_expr_ast();
                Resolved::OpAST(self, self_infix)
            }
        };
        Result::Ok(resolved)
    }
}

impl ast::ExprAST {
     fn resolve_op(self, infix_hash: &InfixHash) -> ResolveResult<Resolved> {
        let resolved = match self {
            ast::ExprAST::OpAST(op_ast) => op_ast.swap_op(infix_hash)?,
            ast::ExprAST::ParenAST(paren_ast) => {
                Resolved::OtherExprAST(ast::ExprAST::create_paren_ast(
                    paren_ast.expr.resolve_op(infix_hash)?.get_expr_ast(),
                ))
            }
            x => Resolved::OtherExprAST(x),
        };
        Result::Ok(resolved)
    }
}
