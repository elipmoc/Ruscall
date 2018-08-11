use super::ast;
use super::infixes::Infixes;
use std::collections::HashMap;

type InfixesHash = HashMap<i8, Infixes>;

//RawExprをOpASTに置き換えたProgramASTを得る
pub fn resolve_op(ast: ast::ProgramAST) -> ast::ProgramAST {
    let mut infixes_hash: InfixesHash = HashMap::new();
    ast::ProgramAST {
        stmt_list: ast
            .stmt_list
            .into_iter()
            .map(|stmt| stmt_resolve_op(&mut infixes_hash, stmt))
            .collect(),
    }
}

fn stmt_resolve_op(infixes_hash: &mut InfixesHash, stmt: ast::StmtAST) -> ast::StmtAST {
    match stmt {
        ast::StmtAST::RawExpr(raw) => {
            let infix_list = infixes_hash.iter().map(|(_, v)| v).collect();
            ast::StmtAST::ExprAST(op_parse(infix_list, &raw))
        }
        ast::StmtAST::InfixAST(infix) => {
            regist_infix(infixes_hash, infix);
            ast::StmtAST::NoneAST
        }
        _ => panic!("bug!!!"),
    }
}

fn regist_infix(infixes_hash: &mut InfixesHash, infix: ast::InfixAST) {
    match infixes_hash.get(&infix.priority) {
        Some(_) => (),
        None => {
            infixes_hash.insert(infix.priority, Infixes::new(infix.priority));
        }
    };
    infixes_hash
        .get_mut(&infix.priority)
        .unwrap()
        .list
        .push(infix);
}

use combine::char::{digit, string};
use combine::parser::combinator::try;
use combine::{eof, look_ahead, many, many1, satisfy, Parser, Stream};

fn op_parse(mut infix_list: Vec<&Infixes>, s: &str) -> ast::ExprAST {
    infix_list.sort();
    match op_parser(0, &infix_list).skip(eof()).parse(s) {
        Ok(result) => result.0,
        Err(err) => panic!(format!(
            "error resolve_op parse \n result:{:?}\n infix{:?}\n",
            err, infix_list
        )),
    }
}

parser!{
    fn op_parser['a,I](index: usize, infix_list:  &'a Vec<&'a Infixes>)(I)->ast::ExprAST
        where [I: Stream<Item=char>] {
            choice!{
                try(look_ahead(satisfy(|_|infix_list.len() > *index)).with(op_parser2(*index,infix_list))),
                num_parser()
            }
    }
}

parser!{
    fn op_parser2['a,I] (index: usize, infix_list:&'a Vec<&'a Infixes>)(I)->ast::ExprAST
    where [I: Stream<Item=char>] {
        op_parser(*index+1,infix_list).and(
            many(
                try((
                    op_symbol_parser(infix_list[*index]),
                    op_parser(*index+1,infix_list)
                ))
            )
        ).map(|(fst_expr,list):(ast::ExprAST,Vec<(String,ast::ExprAST)>)|{
            list.iter().fold(fst_expr,|acc:ast::ExprAST,(op,expr)|{
                ast::ExprAST::OpAST(Box::new(ast::OpAST::new(op.clone(),acc,expr.clone())))
            })
        })
    }
}

parser!{
    fn num_parser[I] ()(I)->ast::ExprAST
    where [I: Stream<Item=char>] {
        many1(digit()).map(|x:String|{
            ast::ExprAST::NumAST(ast::NumAST{num: x.parse::<i32>().unwrap() })
        })
    }
}

parser!{
    fn op_symbol_parser['a,I](infixes: &'a Infixes)(I)->String
    where [I: Stream<Item=char>] {
        string("+").
        or(string("-")).
        or(string("/")).
        or(string("*")).then(move |op|{
            let infixes=infixes.clone();
            look_ahead(satisfy(move |_|{
                infixes.list.iter().any(|infix|infix.op==op)
            })).map(move |_|op.to_string())
        })
    }
}
