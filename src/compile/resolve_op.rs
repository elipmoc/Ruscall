use super::ast;
use std::collections::HashMap;

type InfixesHash<'a> = HashMap<i8, ast::Infixes<'a>>;

pub fn resolve_op(ast: ast::ProgramAST) -> ast::ProgramAST {
    let mut infixes_hash: InfixesHash = HashMap::new();
    ast::ProgramAST {
        stmt_list: ast
            .stmt_list
            .iter()
            .map(|stmt| stmt_resolve_op(&mut infixes_hash, stmt))
            .collect(),
    }
}

fn stmt_resolve_op<'a>(infixes_hash: &mut InfixesHash<'a>, stmt: &'a ast::StmtAST) -> ast::StmtAST {
    match stmt {
        ast::StmtAST::RawExpr(raw) => {
            let infix_list = infixes_hash.iter().map(|(_, v)| v).collect();
            ast::StmtAST::ExprAST(op_parse(infix_list, raw))
        }
        ast::StmtAST::InfixAST(infix) => {
            regist_infix(infixes_hash, infix);
            stmt.clone()
        }
        _ => panic!("bug!!!"),
    }
}

fn regist_infix<'a>(infixes_hash: &mut InfixesHash<'a>, infix: &'a ast::InfixAST) {
    match infixes_hash.get(&infix.priority) {
        Some(_) => (),
        None => {
            infixes_hash.insert(infix.priority, ast::Infixes::new(infix.priority));
        }
    };
    infixes_hash
        .get_mut(&infix.priority)
        .unwrap()
        .list
        .push(infix);
}

use combine::char::{digit, newline, space, string, tab};
use combine::parser::combinator::try;
use combine::{eof, error, look_ahead, many, many1, satisfy, Parser, Stream};

fn op_parse<'a>(mut infix_list: Vec<&'a ast::Infixes>, s: &str) -> ast::ExprAST {
    infix_list.sort();
    match op_parser2(0, &infix_list).skip(eof()).parse(s) {
        Ok(result) => result.0,
        Err(err) => panic!(format!(
            "error resolve_op parse \n result:{:?}\n infix{:?}\n",
            err, infix_list
        )),
    }
}

parser!{
    fn op_parser2['a,I](index: usize, infix_list:  &'a Vec<&'a ast::Infixes<'a>>)(I)->ast::ExprAST
        where [I: Stream<Item=char>] {
            choice!{
                try(look_ahead(satisfy(|_|infix_list.len() > *index)).with(op_parser3(*index,infix_list))),
                num_parser()
            }
    }
}

parser!{
    fn op_parser3['a,I] (index: usize, infix_list:&'a Vec<&'a ast::Infixes<'a>>)(I)->ast::ExprAST
    where [I: Stream<Item=char>] {
        op_parser2(*index+1,infix_list).and(
            many(
                try((
                    op_symbol_parser(infix_list[*index]),
                    op_parser2(*index+1,infix_list)
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
    fn op_symbol_parser['a,I](infixes: &'a ast::Infixes<'a>)(I)->String
    where [I: Stream<Item=char>] {
        let infixes=infixes.clone();
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
