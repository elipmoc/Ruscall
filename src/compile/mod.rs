pub mod ast;
pub mod code_gen;
pub mod error;
pub mod output_file;
pub mod semantic_analysis;
pub mod types;
pub mod show_type;
pub mod parser;

use self::error::Error;
use self::output_file::output_file;
use self::semantic_analysis::ir as ir;
use std::fs;
use std::io::{BufReader, Read};
use self::semantic_analysis::type_env::TypeResolved;

pub fn compile(file_name: &str) {
    println!("input:{}", file_name);
    match parse(&src_file_to_str(file_name)) {
        Ok((program_ir, ty_resolved)) => output_file(program_ir.code_gen("compiled", ty_resolved)),
        Err(err) => println!("{}", err),
    };
}

pub fn src_file_to_str(file_name: &str) -> String {
    let mut f = BufReader::new(fs::File::open(file_name).unwrap());
    let mut src_str: String = "".to_string();
    f.read_to_string(&mut src_str).unwrap();
    src_str
}

pub fn parse(src_str: &str) -> Result<(ir::ProgramIr, TypeResolved), String> {
    match parser::parse(src_str) {
        Ok(ast) => {
            println!("\nparse\n{:?}\n", ast);
            let result = semantic_analysis::analysis(ast.0);
            match result {
                Ok(x) => {
                    println!("resolve_op\n{:?}\n", x.0);
                    Result::Ok(x)
                }
                Err(err) => Result::Err(err.to_string()),
            }
        }
        Err(err) => Result::Err(Error::from_parse_error(err).to_string()),
    }
}
