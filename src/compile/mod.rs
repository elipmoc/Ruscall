pub mod ast;
pub mod code_gen;
pub mod error;
pub mod parser;
pub mod semantic_analysis;
pub mod types;

use self::error::Error;
use std::fs;
use std::io::{BufReader, Read};
use self::semantic_analysis::ir_tree as ir;
use self::semantic_analysis::global_variable_table::ConfirmGlobalVariableTable;

pub fn compile(file_name: &str) {
    println!("input:{}", file_name);
    let mut f = BufReader::new(fs::File::open(file_name).unwrap());
    let mut src_str: String = "".to_string();
    f.read_to_string(&mut src_str).unwrap();
    let src_str: &str = &src_str;
    match parse(src_str) {
        Ok(x) => x.code_gen("compiled"),
        Err(err) => println!("{}", err),
    };
}

fn parse(src_str: &str) -> Result<(ir::ProgramIr), String> {
    match parser::parse(src_str) {
        Ok(ast) => {
            println!("\nparse\n{:?}\n", ast);
            let result = semantic_analysis::analysis(ast.0);
            match result{
                Ok(x) => {
                    println!("resolve_op\n{:?}\n", x);
                    Result::Ok(x)
                }
                Err(err) => Result::Err(err.to_string()),
            }
        }
        Err(err) => Result::Err(Error::from_parse_error(err).to_string()),
    }
}
