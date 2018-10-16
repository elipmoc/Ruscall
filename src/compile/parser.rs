use super::ast;
use combine::char::{alpha_num, char, digit, letter, newline, space, string, tab};
use combine::{easy, optional};
use combine::parser::combinator::try;
use combine::stream;
use combine::stream::state::{DefaultPositioned, SourcePosition, State};
use combine::{eof, many, many1, position, unexpected, value, Parser};
/*
BNF
<program>       := {<stmt>} <skip_many>
<stmt>          := <skip_many> (
                        <infix> |
                        <def_func> |
                        <dec_func> |
                        <exturn_dec_func>
                   ) <skip_many> ';'
<def_func>      := <id> {<skip_many> <id>} <skip_many> '=' <skip_many> <expr>
<id>            := [a-z]{ [a-z] | [0-9] | '_' }
<expr>          := <expr_app> <skip_many> { <op> <skip_many> <expr_app> <skip_many> }
<expr_app>      := <term> {<skip_many> <term> }
<infix>         := ('infixr' | 'infixl') <space>+ <num> <space>+ <op>
<op>            := '+' | '-' | '/' | '*'
<term>          := <num> | <id> | <paren> | <tuple>
<paren>         := '(' <skip_many> <expr> ')'
<num>           := [0-9]+
<tuple>         := '(' <skip_many> [<expr> {',' <skip_many> <expr>} [',' <skip_many>]] ')'
<skip>          := '\n' | <space>
<skip_many>     := {<skip>}
<space>         := ' ' | '\t'
<ty_term>       := 'Int32'| <ty_paren> |<ty_tuple>| <ty_func_pointer>
<ty_func_pointer>
                := 'Fn' <skip_many> <ty_func>
<ty_paren>      := '(' <skip_many> <ty_term> <skip_many> ')'
<ty_tuple>      := '(' <skip_many> [<ty_term> <skip_many> {',' <skip_many> <ty_term> <skip_many>} [',' <skip_many>]] ')'
<ty_func>       := <ty_term> ( <skip_many> '->' <skip_many> <ty_term> )+
<dec_func>      := <id> <skip_many> '::' <skip_many> <ty_func>
<exturn_dec_func>
                := 'ex' <skip_many> <dec_func>
*/

pub fn parse(
    s: &str,
) -> Result<
    (
        ast::ProgramAST,
        stream::state::State<&str, stream::state::SourcePosition>,
    ),
    easy::Errors<char, &str, stream::state::SourcePosition>,
> {
    program_parser().easy_parse(State::new(s))
}

type MyStream<'a> = easy::Stream<State<&'a str, <&'a str as DefaultPositioned>::Positioner>>;

//<program>
parser! {
   fn program_parser['a]()(MyStream<'a>) ->ast::ProgramAST
    {
        many(stmt_parser()).skip(skip_many_parser()).skip(eof().expected("statement or infix")).map(|x|ast::ProgramAST{stmt_list:x})
    }
}

//<stmt>
parser! {
   fn stmt_parser['a]()(MyStream<'a>) ->ast::StmtAST
    {
        try(
            skip_many_parser().
            with (
                try(infix_parser().map(|x|ast::StmtAST::InfixAST(x)))
                .or(try(def_func_parser().map(|x|ast::StmtAST::DefFuncAST(x))))
                .or(try(dec_func_parser().map(|x|ast::StmtAST::DecFuncAST(x))))
                .or(extern_dec_func_parser().map(|x|ast::StmtAST::DecFuncAST(x)))
            ).
            skip(
                skip_many_parser()
            ).skip(char(';'))
        )
    }
}

//<def_func>
parser! {
   fn def_func_parser['a]()(MyStream<'a>) ->ast::DefFuncAST
    {
        (
            position(),
            id_parser().skip(skip_many_parser()),
            many((position(),id_parser()).skip(skip_many_parser()).map(|(pos,id)|ast::VariableAST::new(id,pos))),
            char('=').with(skip_many_parser()).with(expr_parser())
        ).map(|(pos,func_name,params,body)|{ast::DefFuncAST{func_name,params,body,pos}})
    }
}

//<id>
parser! {
   fn id_parser['a]()(MyStream<'a>) ->String
    {
        (letter(),many(alpha_num().or(char('_')))).map(|(x,y):(char,String)|x.to_string()+&y)
    }
}

//<expr_app>
parser! {
    fn expr_app_parser['a]()(MyStream<'a>)->ast::ExprAST{
        (term_parser(),many(
            try(skip_many_parser().with(term_parser()))
        ))
        .map(|(x,xs):(ast::ExprAST,Vec<ast::ExprAST>)|{
            if xs.len()==0 {
                x
            }else{
                ast::ExprAST::create_func_call_ast(x,xs)
            }
        })
    }
}

//<expr>
parser! {
    fn expr_parser['a]()(MyStream<'a>)->ast::ExprAST
    {
        (
           expr_app_parser().skip(skip_many_parser()),
           many((
                    position(),
                    op_parser().skip(skip_many_parser()),
                    expr_app_parser().skip(skip_many_parser())
               ))
        )
       .map(|(e,y):(ast::ExprAST,Vec<(SourcePosition,String,ast::ExprAST)>)|{
            y.into_iter().fold(e,move|acc,(pos,op,term)|{
                ast::ExprAST::create_op_ast(op,pos, acc, term)
            })
       })
    }
}

//<infix>
parser! {
    fn infix_parser['a]()(MyStream<'a>)->ast::InfixAST
    {
        (try(string("infixr")).
        or(string("infixl")).
        map(|s|{
            if s=="infixr"{
                ast::InfixType::Right
            }
            else{
                ast::InfixType::Left
            }
        }).
        skip(many1::<Vec<_>,_>(space_parser())),
        num_parser().
        then(|num_s|
            match num_s.parse::<i8>(){
                Ok(num) if (0<=num && num<=30) =>value(num).left(),
                _=>unexpected("not 0 <= number <= 30 ").map(|_|0).right()
            }
        ).
        skip(many1::<Vec<_>,_>(space_parser())),
        op_parser()
        ).
        map(|(ty,priority,op)|{
            ast::InfixAST{
                ty: ty,
                priority:ast::Priority(priority),
                op:op
            }
        })
    }
}

//<op>
parser! {
    fn op_parser['a]()(MyStream<'a>)->String
    {
        string("+").
        or(string("-")).
        or(string("/")).
        or(string("*")).map(|s|s.to_string())
    }
}

//<term>
parser! {
    fn term_parser['a]()(MyStream<'a>)->ast::ExprAST
    {
        try(paren_parser())
        .or(tuple_parser())
        .or(
            num_parser()
            .map(ast::ExprAST::create_num_ast)
        )
        .or(
            (position(),id_parser())
            .skip(skip_many_parser())
            .map(|(pos,id)|ast::ExprAST::VariableAST(ast::VariableAST::new(id,pos)))
        )
    }
}

//<paren>
parser! {
    fn paren_parser['a]()(MyStream<'a>)->ast::ExprAST
    {
        char('(')
        .with(skip_many_parser())
        .with(expr_parser())
        .map(ast::ExprAST::create_paren_ast)
        .skip(char(')'))
    }
}

//<num>
parser! {
    fn num_parser['a]()(MyStream<'a>)->String
    {
        many1(digit())
    }
}

//<tuple>
parser! {
    fn tuple_parser['a]()(MyStream<'a>)->ast::ExprAST
    {
        (
            position(),
            char('(')
            .with(skip_many_parser())
            .with(
                optional((
                    expr_parser(),
                    many(try(
                        char(',')
                        .with(skip_many_parser())
                        .with(expr_parser())
                    ))
                    .skip(optional((
                        char(','),
                        skip_many_parser()
                    )))
                ))
            )
        )
        .skip(char(')'))
        .map(move|(pos,x):(_,Option<(ast::ExprAST,Vec<ast::ExprAST>)>)|{
            let mut elements=vec![];
            match x{
                Some((x,mut xs))=>{
                    elements.push(x);
                    elements.append(&mut xs);
                }
                _=>()
            }
            ast::ExprAST::create_tuple_ast(elements,pos)
        })
    }
}

//<skip>
parser! {
    fn skip_parser['a]()(MyStream<'a>)->()
    {
        newline().map(|_|()).or(space_parser())
    }
}

//<skip_many>
parser! {
    fn skip_many_parser['a]()(MyStream<'a>)->()

    {
        many::<Vec<_>,_>(skip_parser()).map(|_|())
    }
}

//<space>
parser! {
   fn space_parser['a]()(MyStream<'a>) ->()
    {
        space().or(tab()).map(|_|())
    }
}

use super::types::*;

//<ty_term>
parser! {
   fn ty_term_parser['a]()(MyStream<'a>) ->Type
    {
       string("Int32").map(|_|Type::Int32)
       .or(try(ty_paren_parser()))
       .or(ty_tuple_parser())
       .or(ty_func_pointer_parser())
    }
}
//<ty_func_pointer>
parser! {
   fn ty_func_pointer_parser['a]()(MyStream<'a>) ->Type
    {
        string("Fn")
        .with(skip_many_parser())
        .with(ty_func_parser())
        .map(|x|Type::Fn(Box::new(x)))
    }
}

//<ty_paren>
parser! {
   fn ty_paren_parser['a]()(MyStream<'a>) ->Type
    {
        char('(')
        .with(skip_many_parser())
        .with(ty_term_parser())
        .skip(skip_many_parser())
        .skip(char(')'))
    }
}

//<ty_tuple>
parser! {
   fn ty_tuple_parser['a]()(MyStream<'a>) ->Type
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
                        .with(ty_term_parser())
                        .skip(skip_many_parser())
                    ))
                    .skip(optional((
                        char(','),
                        skip_many_parser()
                    )))
                ))
            )
            .skip(char(')'))
            .map(move|x:Option<(Type,Vec<Type>)>|{
                let mut elements=vec![];
                match x{
                    Some((x,mut xs))=>{
                        elements.push(x);
                        elements.append(&mut xs);
                    }
                    _=>()
                }
                Type::TupleType(Box::new( TupleType{ element_tys: elements }) )
            })
    }
}

//<ty_func>
parser! {
   fn ty_func_parser['a]()(MyStream<'a>) ->FuncType
    {
        (
            ty_term_parser(),
            many1(
                skip_many_parser()
                .with(string("->"))
                .with(skip_many_parser())
                .with(ty_term_parser())
            )
        )
        .map(|(x,mut xs):(Type,Vec<Type>)|{
            let ret_type = xs.pop().unwrap();
            let mut param_types=vec![x];
            param_types.append(&mut xs);
            FuncType{
                ret_type: ret_type,
                param_types: param_types
            }
        })
    }
}

//<dec_func>
parser! {
   fn dec_func_parser['a]()(MyStream<'a>) ->ast::DecFuncAST
    {
        (
            position(),
            id_parser()
            .skip(skip_many_parser())
            .skip(string("::"))
            .skip(skip_many_parser())
            .and(ty_func_parser())
        )
        .map(|(pos,(name,ty))|{
            ast::DecFuncAST{
                name: name,
                ty: ty,
                extern_flag: false,
                pos
            }
        })
    }
}

//<extern_dec_func>
parser! {
   fn extern_dec_func_parser['a]()(MyStream<'a>) ->ast::DecFuncAST
    {
        string("ex")
        .with(skip_many_parser())
        .with(dec_func_parser())
        .map(|mut x|{
          x.extern_flag=true;
          x
        })
    }
}
