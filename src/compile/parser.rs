use super::ast;
use combine::char::{digit, newline, space, string, tab};
use combine::easy;
use combine::parser::combinator::try;
use combine::stream;
use combine::stream::state::{DefaultPositioned, SourcePosition, State};
use combine::{eof, error, many, many1, ParseError, Parser, Positioned, Stream, StreamOnce};
/*
BNF
<program>  := {<stmt>}
<stmt>     := <skip_many> ( <infix> | <expr> ) <skip_many>
<expr>     := <num> <skip_many> { <op> <skip_many> <num> <skip_many> }
<infix>    := ('infixr' | 'infixl') <space>+ <num> <space>+ <op>
<op>       := '+' | '-' | '/' | '*'
<num>      := [0-9]+
<skip>     := '\n' | <space>
<skip_many>:= {<skip>}
<space>    := ' ' | '\t'

*/

#[derive(Debug)]
struct Position {}

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

//<program>
parser!{
   fn program_parser[I]()(I) ->ast::ProgramAST
   where[ I:Stream<Item=char>]
    {
        many(stmt_parser()).skip(eof().expected("statement or infix")).map(|x|ast::ProgramAST{stmt_list:x})
    }
}

//<stmt>
parser!{
   fn stmt_parser[I]()(I) ->ast::StmtAST
   where[ I:Stream<Item=char>]
    {
        skip_many_parser().
        with (
            infix_parser().map(|x|ast::StmtAST::InfixAST(x)).
            or(expr_parser().map(|x|ast::StmtAST::ExprAST(x)))
        ).
        skip(
            skip_many_parser()
        )
    }
}

//<expr>
parser!{
    fn expr_parser[I]()(I)->ast::ExprAST
   where[ I:Stream<Item=char>]

    {
        (
           num_parser().skip(skip_many_parser()),
           many((
                    op_parser().skip(skip_many_parser()),num_parser().skip(skip_many_parser())
               ))
        ).
       map(|(x,y):(String,Vec<(String,String)>)|{
           let  e=ast::ExprAST::create_num_ast(x);
            y.into_iter().fold(e,move|acc,(op,num)|{
                ast::ExprAST::create_op_ast(op, acc, ast::ExprAST::create_num_ast(num))
            })
       })
    }
}

//<infix>
parser!{
    fn infix_parser[I]()(I)->ast::InfixAST
   where[ I:Stream<Item=char>]

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
        skip(many1::<Vec<_>,_>(space_parser())),
        op_parser()
        ).
        map(|(ty,priority,op)|{
            ast::InfixAST{
                ty: ty,
                priority:ast::Priority(priority.parse::<i8>().unwrap()),
                op:op
            }
        })
    }
}

//<op>
parser!{
    fn op_parser[I]()(I)->String
   where[ I:Stream<Item=char>]

    {
        string("+").
        or(string("-")).
        or(string("/")).
        or(string("*")).map(|s|s.to_string())
    }
}

//<num>
parser!{
    fn num_parser[I]()(I)->String
   where[ I:Stream<Item=char>]

    {
        many1(digit())
    }
}

//<skip>
parser!{
    fn skip_parser[I]()(I)->()
   where[ I:Stream<Item=char>]

    {
        newline().map(|_|()).or(space_parser())
    }
}

//<skip_many>
parser!{
    fn skip_many_parser[I]()(I)->()
   where[ I:Stream<Item=char>]

    {
        many::<Vec<_>,_>(skip_parser()).map(|_|())
    }
}

//<space>
parser!{
   fn space_parser[I]()(I) ->()
   where[ I:Stream<Item=char>]

    {
        space().or(tab()).map(|_|())
    }
}
