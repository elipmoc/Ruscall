use super::ast;
//use combine;
use combine::char::{char, digit, letter, space, tab};
use combine::parser::choice::or;
use combine::parser::sequence::then;
use combine::{many, value, Parser, Stream};

/*
BNF
<program> := {<stmt>}
<stmt>    := <infix> | <expr> |<skip>
<expr>    := <num> { <op> <num> }
<infix>   := ('infixr' | 'infixl') <space>+ <num> <space>+ <op>
<op>      := '+' | '-' | '/' | '*'
<num>     := [0-9]+
<skip>    := '\n' | <space>
<space>   := ' ' | '\t'

*/

pub fn parse(s: &str) {}

//<program>
/*parser!{
   fn program_parser[I]()(I) ->ast::ProgramAST
    where [I: Stream<Item=char>]
    {
    }
}*/
//<space>
parser!{
   fn space_parser[I]()(I) ->()
    where [I: Stream<Item=char>]
    {
        space().or(tab()).then(|_|value(()))
    }
}
