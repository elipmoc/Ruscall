use super::super::types::types::*;
use super::super::ir::ast::*;
use super::parser::MyStream;
use super::skipper::skip_many_parser;
use combine::char::{char, string};
use combine::optional;
use combine::parser::char::{alpha_num, lower};
use combine::parser::combinator::try;
use combine::many;

//<ty_term>
parser! {
   fn ty_term_parser['a]()(MyStream<'a>) ->TypeAST
    {
       string("Int32").map(|_|TypeAST::Type(Type::Int32) )
       .or((
            lower(),many(alpha_num())
        ).map(|(x,xs):(char,String)|TypeAST::TypeVarName(x.to_string()+&xs)))
       .or(try(ty_paren_parser()))
       .or(ty_tuple_parser())
    }
}

//<ty_term_with_func>
parser! {
   fn ty_term_with_func_parser['a]()(MyStream<'a>) ->TypeAST
    {
        try(ty_func_parser().map(|f|TypeAST::FuncTypeAST(Box::new(f) )))
        .or(ty_term_parser())
    }
}

//<ty_paren>
parser! {
   fn ty_paren_parser['a]()(MyStream<'a>) ->TypeAST
    {
        char('(')
        .with(skip_many_parser())
        .with(ty_term_with_func_parser())
        .skip(skip_many_parser())
        .skip(char(')'))
    }
}

//<ty_tuple>
parser! {
   fn ty_tuple_parser['a]()(MyStream<'a>) ->TypeAST
    {
        char('(')
            .with(skip_many_parser())
            .with(
                optional((
                    ty_term_parser()
                    .skip(skip_many_parser()),
                    many(try(
                        char(',')
                        .with(skip_many_parser())
                        .with(ty_term_with_func_parser())
                        .skip(skip_many_parser())
                    ))
                    .skip(optional((
                        char(','),
                        skip_many_parser()
                    )))
                ))
            )
            .skip(char(')'))
            .map(move|x:Option<(TypeAST,Vec<TypeAST>)>|{
                let mut elements=vec![];
                match x{
                    Some((x,mut xs))=>{
                        elements.push(x);
                        elements.append(&mut xs);
                    }
                    _=>()
                }
                TypeAST::TupleTypeAST(Box::new( TupleTypeAST{ elements_ty: elements }) )
            })
    }
}

//<ty_func>
parser! {
   pub fn ty_func_parser['a]()(MyStream<'a>) ->FuncTypeAST
    {
        (
            ty_term_parser(),
            skip_many_parser()
            .with(string("->"))
            .with(skip_many_parser())
            .with(
                try(ty_func_parser().map(|f|TypeAST::FuncTypeAST(Box::new(f) )))
                .or(ty_term_parser())
            )
        )
        .map(|(x,e)|{
            FuncTypeAST{
                ret_ty: e,
                params_ty: vec![x]
            }
        })
    }
}
