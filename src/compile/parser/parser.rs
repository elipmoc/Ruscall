use super::super::ast;
use super::skipper::*;
use combine::char::{alpha_num, char, digit, letter, string};
use combine::parser::combinator::try;
use combine::stream::state::{DefaultPositioned, SourcePosition, State};
use combine::{easy, optional, sep_by};
use combine::{eof, many, many1, position, unexpected, value};

/*
BNF
:program       := {:stmt} :skip_many
:stmt          := :skip_many (
                        :infix |
                        :def_func |
                        :dec_func |
                        :exturn_dec_func
                   ) :skip_many ';'
:def_func      := :id {:skip_many :id} :skip_many '=' :skip_many :expr
:id            := [a-z]{ [a-z] | [0-9] | '_' }
:expr          := :expr_app :skip_many { :op :skip_many :expr_app :skip_many }
:expr_app      := :term {:skip_many :term }
:infix         := ('infixr' | 'infixl') :space + :num :space + :op
:op            := '+' | '-' | '/' | '*'
:term          := :num | :id | :paren | :tuple | :lambda
:paren         := '(' :skip_many :expr ')'
:num           := [0-9]+
:tuple         := '(' :skip_many [ :expr {',' :skip_many :expr} [',' :skip_many]] ')'
:lambda        := '\' :skip_many [ '[' :lambda_params ']' ] :lambda_params '->' :skip_many :expr
:lambda_params := :skip_many [ :id { :skip_many ',' :skip_many :id } :skip_many ]
:skip          := '\n' | :space
:skip_many     := {:skip}
:space         := ' ' | '\t'
:ty_term       := 'Int32'| :ty_paren | :ty_tuple
:ty_term_with_func
               := :ty_term | :ty_func
:ty_paren      := '(' :skip_many :ty_term_with_func :skip_many ')'
:ty_tuple      := '(' :skip_many [  :ty_term_with_func :skip_many {',' :skip_many :ty_term :skip_many} [',' :skip_many]] ')'
:ty_func       := :ty_term :skip_many '->' :skip_many  ( :ty_func | :ty_term )
:dec_func      := :id :skip_many '::' :skip_many :ty_func
:exturn_dec_func
                := 'ex' :skip_many :dec_func
*/

pub type MyStream<'a> = easy::Stream<State<&'a str, <&'a str as DefaultPositioned>::Positioner>>;

//<program>
parser! {
   pub fn program_parser['a]()(MyStream<'a>) ->ast::ProgramAST
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
        ).map(|(pos,name,params,body)|{ast::DefFuncAST{name,params,body,pos}})
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
            xs.into_iter()
            .fold(x,|acc,param|
                ast::ExprAST::create_func_call_ast(acc,vec![param])
            )
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
            (position(),num_parser())
            .map(|(pos,num)|ast::ExprAST::create_num_ast(num,pos))
        )
        .or(
            (position(),id_parser())
            .skip(skip_many_parser())
            .map(|(pos,id)|ast::ExprAST::VariableAST(ast::VariableAST::new(id,pos)))
        )
        .or(lambda_parser())
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

parser! {
    fn lambda_parser['a]()(MyStream<'a>)->ast::ExprAST
    {
        (
            position(),
            char('\\')
            .with(skip_many_parser())
            .with(
                optional(
                    char('[')
                    .with(lambda_params_parser())
                    .skip(char(']'))
                ),
            ),
            lambda_params_parser()
            .skip(string("->"))
            .skip(skip_many_parser()),
            expr_parser()
        )
        .map(move|(pos,env,params,body)|
            ast::ExprAST::create_lambda_ast(env.unwrap_or(vec![]),params,body,pos)
        )
    }
}

parser! {
    fn lambda_params_parser['a]()(MyStream<'a>)->Vec<ast::VariableAST>
    {
        skip_many_parser()
        .with(
            try(
                sep_by(
                    (position(),id_parser()),
                    try((skip_many_parser(),char(','),skip_many_parser()))
                )
            )
            .skip(skip_many_parser())
        )
        .map(|x:Vec<_>|
                x.into_iter().map(|(pos,id)|ast::VariableAST{id: id,pos: pos}).collect()
        )
    }
}

//<dec_func>
parser! {
   fn dec_func_parser['a]()(MyStream<'a>) ->ast::DecFuncAST
    {
        use super::types::ty_func_parser;
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
