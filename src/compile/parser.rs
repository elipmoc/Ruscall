use super::ast;
use combine::char::{char, digit, newline, space, string, tab};
use combine::easy;
use combine::parser::combinator::try;
use combine::stream;
use combine::stream::state::{DefaultPositioned, SourcePosition, State};
use combine::{eof, many, many1, optional, position, unexpected, value, Parser};
/*
BNF
<program>  := {<stmt>}
<stmt>     := <skip_many> ( <infix> | <expr_app> | <def_func>) <skip_many> ';'
<def_func> := <id> {<skip_many> <id>} <skip_many> '=' <skip_many> <expr_app>
<id>       := [a-z]{ [a-z] | [0-9] | '_' }
<expr_app> := <expr> { <expr> }
<expr>     := <term> <skip_many> { <op> <skip_many> <term> <skip_many> } 
<infix>    := ('infixr' | 'infixl') <space>+ <num> <space>+ <op>
<op>       := '+' | '-' | '/' | '*'
<term>     := <num> | <id> | <paren>
<paren>    := '(' <expr_app> ')'
<num>      := [0-9]+
<skip>     := '\n' | <space>
<skip_many>:= {<skip>}
<space>    := ' ' | '\t'

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
parser!{
   fn program_parser['a]()(MyStream<'a>) ->ast::ProgramAST
    {
        many(stmt_parser()).skip(eof().expected("statement or infix")).map(|x|ast::ProgramAST{stmt_list:x})
    }
}

//<stmt>
parser!{
   fn stmt_parser['a]()(MyStream<'a>) ->ast::StmtAST
    {
        skip_many_parser().
        with (
            infix_parser().map(|x|ast::StmtAST::InfixAST(x)).
            or(expr_parser().map(|x|ast::StmtAST::ExprAST(x)))
        ).
        skip(
            skip_many_parser()
        ).skip(char(';'))
    }
}

//<expr>
parser!{
    fn expr_parser['a]()(MyStream<'a>)->ast::ExprAST
    {
        (
           num_parser().skip(skip_many_parser()),
           many((
                    position(),
                    op_parser().skip(skip_many_parser()),
                    num_parser().skip(skip_many_parser())
               ))
        )
       .map(|(x,y):(String,Vec<(SourcePosition,String,String)>)|{
           let  e=ast::ExprAST::create_num_ast(x);
            y.into_iter().fold(e,move|acc,(pos,op,num)|{
                ast::ExprAST::create_op_ast(op,pos, acc, ast::ExprAST::create_num_ast(num))
            })
       })
    }
}

//<infix>
parser!{
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
parser!{
    fn op_parser['a]()(MyStream<'a>)->String
    {
        string("+").
        or(string("-")).
        or(string("/")).
        or(string("*")).map(|s|s.to_string())
    }
}

//<num>
parser!{
    fn num_parser['a]()(MyStream<'a>)->String
    {
        many1(digit())
    }
}

//<skip>
parser!{
    fn skip_parser['a]()(MyStream<'a>)->()
    {
        newline().map(|_|()).or(space_parser())
    }
}

//<skip_many>
parser!{
    fn skip_many_parser['a]()(MyStream<'a>)->()

    {
        many::<Vec<_>,_>(skip_parser()).map(|_|())
    }
}

//<space>
parser!{
   fn space_parser['a]()(MyStream<'a>) ->()

    {
        space().or(tab()).map(|_|())
    }
}
