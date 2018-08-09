use super::ast;
use std::collections::HashMap;

type InfixesHash = HashMap<i8, ast::Infixes>;

pub fn resolve_op(ast: &mut ast::ProgramAST) -> ast::ProgramAST {
    let mut infixes_hash: InfixesHash = HashMap::new();
    ast::ProgramAST {
        stmt_list: ast
            .stmt_list
            .iter()
            .map(|stmt| stmt_resolve_op(&mut infixes_hash, stmt))
            .collect(),
    }
}

fn stmt_resolve_op(infixes_hash: &mut InfixesHash, stmt: &ast::StmtAST) -> ast::StmtAST {
    match stmt {
        ast::StmtAST::ExprAST(expr_ast) => match expr_ast {
            ast::ExprAST::OpTokenListAST(expr) => stmt.clone(),
            _ => stmt.clone(),
        },
        ast::StmtAST::InfixAST(infix) => {
            regist_infix(infixes_hash, infix);
            stmt.clone()
        }
    }
}

fn regist_infix(infixes_hash: &mut InfixesHash, infix: &ast::InfixAST) {}
