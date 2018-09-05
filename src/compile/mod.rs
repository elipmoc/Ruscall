pub mod ast;
pub mod code_gen;
pub mod error;
pub mod parser;
pub mod semantic_analysis;
pub mod types;

use self::error::Error;
use std::fs;
use std::io::{BufReader, Read};

pub fn compile(file_name: &str) {
    println!("input:{}", file_name);
    let mut f = BufReader::new(fs::File::open(file_name).unwrap());
    let mut src_str: String = "".to_string();
    f.read_to_string(&mut src_str).unwrap();
    let src_str: &str = &src_str;
    match parse(src_str) {
        Ok(ast) => ast.to_ir().code_gen("compiled"),
        Err(err) => println!("{}", err),
    };
}

fn parse(src_str: &str) -> Result<ast::ProgramAST, String> {
    match parser::parse(src_str) {
        Ok(ast) => {
            println!("\nparse\n{:?}\n", ast);
            let result_ast = semantic_analysis::analysis(ast.0);
            match result_ast {
                Ok(ast) => {
                    println!("resolve_op\n{:?}\n", ast);
                    Result::Ok(ast)
                }
                Err(err) => Result::Err(err.to_string()),
            }
        }
        Err(err) => Result::Err(Error::from_parse_error(err).to_string()),
    }
}
