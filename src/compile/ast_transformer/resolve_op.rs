use super::super::ir::ast;
use super::super::Error;
use std::collections::HashMap;

type InfixHash = HashMap<String, ast::InfixAST>;
type ResolveResult<T> = Result<T, Error>;

impl ast::ProgramAST {
    //OpASTをinfixの定義によって優先順位を置き換えたProgramASTを得る
    pub fn resolve_op(self) -> ResolveResult<ast::ProgramAST> {
        let mut infix_hash: InfixHash = HashMap::new();
        let mut new_stmt_list = Vec::with_capacity(self.stmt_list.len());
        for stmt in self.stmt_list {
            new_stmt_list.push(stmt.resolve_op(&mut infix_hash)?);
        }
        Result::Ok(ast::ProgramAST {
            stmt_list: new_stmt_list,
        })
    }
}

impl ast::StmtAST {
    fn resolve_op(self, infix_hash: &mut InfixHash) -> ResolveResult<ast::StmtAST> {
        let new_stmt = match self {
            ast::StmtAST::DefFuncAST(def_func_ast) => {
                ast::StmtAST::DefFuncAST(def_func_ast.resolve_op(infix_hash)?)
            }
            ast::StmtAST::InfixAST(infix) => {
                register_infix(infix_hash, infix);
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

fn register_infix(infix_hash: &mut InfixHash, infix: ast::InfixAST) {
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
        use super::super::ir::ast::*;
        let resolved = match self {
            ExprAST::OpAST(op_ast) => op_ast.swap_op(infix_hash)?,
            ExprAST::ParenAST(paren_ast) => {
                Resolved::OtherExprAST(ExprAST::create_paren_ast(
                    paren_ast.expr.resolve_op(infix_hash)?.get_expr_ast(),
                ))
            }
            ExprAST::FuncCallAST(x) => {
                let mut x = *x;
                x.params =
                    x.params.into_iter()
                        .map(|e| e.resolve_op(infix_hash).map(|e| e.get_expr_ast()))
                        .collect::<ResolveResult<Vec<ExprAST>>>()?;
                x.func = x.func.resolve_op(infix_hash)?.get_expr_ast();
                Resolved::OtherExprAST(ExprAST::FuncCallAST(Box::new(x)))
            }
            ExprAST::LambdaAST(x) => {
                let mut x = *x;
                x.body = x.body.resolve_op(infix_hash)?.get_expr_ast();
                Resolved::OtherExprAST(ExprAST::LambdaAST(Box::new(x)))
            }
            ExprAST::TupleAST(x) => {
                let mut x = *x;
                x.elements =
                    x.elements.into_iter()
                        .map(|e| e.resolve_op(infix_hash).map(|e| e.get_expr_ast()))
                        .collect::<ResolveResult<Vec<ExprAST>>>()?;
                Resolved::OtherExprAST(ExprAST::TupleAST(Box::new(x)))
            }
            ExprAST::IfAST(x) => {
                let mut x = *x;
                x.t_expr = x.t_expr.resolve_op(infix_hash)?.get_expr_ast();
                x.f_expr = x.f_expr.resolve_op(infix_hash)?.get_expr_ast();
                x.cond = x.cond.resolve_op(infix_hash)?.get_expr_ast();
                Resolved::OtherExprAST(ExprAST::IfAST(Box::new(x)))
            }
            ExprAST::NumAST(_) | ExprAST::BoolAST(_) | ExprAST::VariableAST(_) => Resolved::OtherExprAST(self),
        };
        Result::Ok(resolved)
    }
}
