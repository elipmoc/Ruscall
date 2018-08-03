use super::ast;
use combine::char::{char, digit, letter, newline, space, string, tab};
use combine::parser::combinator::try;
use combine::{error, many, many1, value, Parser, Stream};
/*
BNF
<program> := {<stmt>}
<stmt>    := {<skip>} ( <infix> | <expr> ) {<skip>}
<expr>    := <num> { <op> <num> }
<infix>   := ('infixr' | 'infixl') <space>+ <num> <space>+ <op>
<op>      := '+' | '-' | '/' | '*'
<num>     := [0-9]+
<skip>    := '\n' | <space>
<space>   := ' ' | '\t'

*/

pub fn parse(s: &str) -> Result<(ast::ProgramAST, &str), error::StringStreamError> {
    program_parser().parse(s)
}

//<program>
parser!{
   fn program_parser[I]()(I) ->ast::ProgramAST
    where [I: Stream<Item=char>]
    {
        many(stmt_parser()).map(|x|ast::ProgramAST{stmt_list:x})
    }
}

//<stmt>
parser!{
   fn stmt_parser[I]()(I) ->ast::StmtAST
    where [I: Stream<Item=char>]
    {
        many::<Vec<_>,_>(skip_parser()).
        with (
            infix_parser().map(|x|ast::StmtAST::InfixAST(x))
        ).
        skip(
            many::<Vec<_>,_>(skip_parser())
        )
    }
}

//<infix>
parser!{
    fn infix_parser[I]()(I)->ast::InfixAST
    where[I:Stream<Item=char>]
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
                priority:priority.parse::<i8>().unwrap(),
                op:op
            }
        })
    }
}

//<op>
parser!{
    fn op_parser[I]()(I)->String
    where[I:Stream<Item=char>]
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
    where[I:Stream<Item=char>]
    {
        many1(digit())
    }
}

//<skip>
parser!{
    fn skip_parser[I]()(I)->()
    where[I:Stream<Item=char>]
    {
        newline().map(|_|()).or(space_parser())
    }
}

//<space>
parser!{
   fn space_parser[I]()(I) ->()
    where [I: Stream<Item=char>]
    {
        space().or(tab()).map(|_|())
    }
}
