use super::super::ir::ast;
use super::skipper::*;
use super::types::{struct_record_parser, ty_tuple_parser};
use combine::char::{alpha_num, char, digit, lower, string, upper};
use combine::parser::combinator::try;
use combine::stream::state::{DefaultPositioned, SourcePosition, State};
use combine::{easy, optional, sep_by, sep_end_by};
use combine::{eof, many, many1, position, unexpected, value};

/*
BNF
:program       := {:stmt} :skip_many
:stmt          := :skip_many (
                        :infix |
                        :def_func |
                        :dec_func |
                        :exturn_dec_func |
                        :struct
                   ) :skip_many ';'
:struct        := 'struct' :skip_many :upper_id :skip_many :ty_tuple | :struct_record
:struct_record := '{' :skip_many :struct_record_part { :skip_many ',' :skip_many :struct_record_part} [:skip_many,','] :skip_many '}'
:struct_record_part
               := :id :skip_many ':' :skip_many :ty_term_with_func
:upper_id      := [A-Z]{ [a-z] | [0-9] | '_' }
:def_func      := :id {:skip_many :id} :skip_many '=' :skip_many :expr
:id            := [a-z]{ [a-z] | [0-9] | '_' }
:expr          := :expr_app :skip_many { :op :skip_many :expr_app :skip_many }
:expr_app      := :term { :skip_many :term }
:named_params_constructor_call
               := :upper_id  :skip_many :named_params
:named_params  := '{' :skip_many :named_param { :skip_many ',' :skip_many :named_param } [:skip_many,','] :skip_many  '}'
:named_param   := :id :skip_many '=' :skip_many :expr
:infix         := ('infixr' | 'infixl') :skip_many1 :num :skip_many1 :op
:op            := '+' | '-' | '/' | '*' | '=='
:term          :=
                    (
                        :num |
                        :bool |
                        :if |
                        :named_params_constructor_call |
                        :id  |
                        :upper_id |
                        :paren |
                        :tuple |
                        :lambda
                    ){'.' :id}
:paren         := '(' :skip_many :expr ')'
:num           := [0-9]+
:bool          := 'true' | 'false'
:if            := 'if' :skip_many :expr '{' :skip_many :expr '}' :skip_many 'else' :skip_many '{' :skip_many :expr '}'
:tuple         := '(' :skip_many [ :expr {',' :skip_many :expr} [',' :skip_many]] ')'
:lambda        := '\' :skip_many [ '[' :lambda_params ']' ] :lambda_params '->' :skip_many :expr
:lambda_params := :skip_many [ :id { :skip_many ',' :skip_many :id } :skip_many ]
:skip          := '\n' | :space | ' ' | '\t'
:comment       := '//' [^ \n ]*
:comments      := '/*' {:comments |[^(/*)(*/)]} '*/'
:skip_many     := {:skip}
:skip_many1    := :skip +
:ty_term       := 'Int32'| :id | :ty_paren | :ty_tuple | :upper_id
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
                try(infix_parser().map(ast::StmtAST::InfixAST))
                .or(try(def_func_parser().map(ast::StmtAST::DefFuncAST)))
                .or(try(dec_func_parser().map(ast::StmtAST::DecFuncAST)))
                .or(extern_dec_func_parser().map(ast::StmtAST::DecFuncAST))
                .or(struct_parser().map(ast::StmtAST::DecStructAST))
            ).
            skip(
                skip_many_parser()
            ).skip(char(';'))
        )
    }
}

//<struct>
parser! {
    fn struct_parser['a]()(MyStream<'a>)->ast::DecStructAST{
        (
            position(),
            string("struct")
            .with(skip_many_parser())
            .with(upper_id_parser()),
            skip_many_parser()
            .with(
                ty_tuple_parser().map(|tuple|ast::StructInternalTypeAST::TupleTypeAST(tuple) )
                .or(struct_record_parser())
            )
        )
        .map(|(pos,name,ty)|
            ast::DecStructAST{
                ty: ast::StructTypeAST{ty,name},
                pos,
            }
        )

    }
}

//<upper_id>
parser! {
    pub fn upper_id_parser['a]()(MyStream<'a>)->String{
        (upper(),many(alpha_num().or(char('_')))).map(|(x,y):(char,String)|x.to_string()+&y)
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
   pub fn id_parser['a]()(MyStream<'a>) ->String
    {
        (lower(),many(alpha_num().or(char('_')))).map(|(x,y):(char,String)|x.to_string()+&y)
    }
}

//<expr_app>
parser! {
    fn expr_app_parser['a]()(MyStream<'a>)->ast::ExprAST{
        (term_parser(),many(
            try(
                skip_many_parser()
                .with(
                    term_parser()
                )
            )
        ))
        .map(|(x,xs):(ast::ExprAST,Vec<ast::ExprAST>)|{
            xs.into_iter()
            .fold(x,|acc,param|
                ast::ExprAST::create_func_call_ast(acc,param)
            )
        })
    }
}

//<named_params_constructor_call>
parser! {
    fn named_params_constructor_call_parser['a]()(MyStream<'a>)->ast::ExprAST {
        (
            position(),
            upper_id_parser().skip(skip_many_parser()),
            named_params_parser()
        )
        .map(|(pos,constructor_name,params)| ast::ExprAST::create_named_params_constructor_call_ast(constructor_name,params,pos) )
    }
}
//<named_params>
parser! {
    fn named_params_parser['a]()(MyStream<'a>)->Vec<(String,ast::ExprAST)>{
        char('{')
        .with(skip_many_parser())
        .with(
            sep_end_by(
                named_param_parser(),
                skip_many_parser()
                    .with(char(','))
                    .skip(skip_many_parser())
            )
                .skip(skip_many_parser())
        )
        .skip(char('}'))
    }
}

//<named_param>
parser! {
    fn named_param_parser['a]()(MyStream<'a>)->(String,ast::ExprAST){
        (
            id_parser()
            .skip(skip_many_parser())
            .skip(char('='))
            .skip(skip_many_parser()),
            expr_parser()
        )
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
        skip(many1::<Vec<_>,_>(skip_many1_parser())),
        num_parser().
        then(|num_s|
            match num_s.parse::<i8>(){
                Ok(num) if (0<=num && num<=30) =>value(num).left(),
                _=>unexpected("not 0 <= number <= 30 ").map(|_|0).right()
            }
        ).
        skip(many1::<Vec<_>,_>(skip_many1_parser())),
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
        or(string("*")).
        or(string("=="))
        .map(|s|s.to_string())
    }
}

//<term>
parser! {
    fn term_parser['a]()(MyStream<'a>)->ast::ExprAST
    {
        (
            try(paren_parser())
            .or(tuple_parser())
            .or(
                (position(),num_parser())
                .map(|(pos,num)|ast::ExprAST::create_num_ast(num,pos))
            )
            .or(try(bool_parser()))
            .or(try(if_parser()))
            .or(try(named_params_constructor_call_parser()))
            .or(
                (position(),id_parser().or(upper_id_parser()))
                .skip(skip_many_parser())
                .map(|(pos,id)|ast::ExprAST::VariableAST(ast::VariableAST::new(id,pos)))
            )
            .or(lambda_parser()),
            many(char('.').with(( position(), num_parser())))
        ).map(|(expr,prop ):(_,Vec<_>)|{
            prop.into_iter().fold(expr,|acc,(pos,index) |
                ast::ExprAST::create_tuple_property_ast(index ,acc, pos)
            )
        })
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

//<bool>
parser! {
    fn bool_parser['a]()(MyStream<'a>)->ast::ExprAST
    {
        (
            position(),
            string("true").with(value(true) )
            .or(string("false").with(value(false)))
        )
        .map(|(pos,b)|ast::ExprAST::create_bool_ast(b,pos))
    }
}

//<if>
parser! {
    fn if_parser['a]()(MyStream<'a>)->ast::ExprAST
    {
        (
            position(),
            string("if")
            .with(skip_many_parser())
            .with(expr_parser()),
            char('{')
            .with(skip_many_parser())
            .with(expr_parser())
            .skip(char('}'))
            .skip(skip_many_parser()),
            string("else")
            .with(skip_many_parser())
            .with(char('{'))
            .with(skip_many_parser())
            .with(expr_parser())
            .skip(char('}'))
        )
        .map(|(pos,cond,t_expr,f_expr)|
            ast::ExprAST::create_if_ast(cond,t_expr,f_expr,pos)
        )
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
