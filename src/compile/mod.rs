pub mod code_gen;
pub mod error;
pub mod output_file;
pub mod parser;
pub mod semantic_analysis;
pub mod types;
pub mod ast_transformer;
pub mod mangling;
pub mod ir;
pub mod to_llvm_type;

use super::compile::semantic_analysis::type_inference::assump_env::AssumpEnv;
use self::error::Error;
use self::output_file::output_file;
use self::ir::mir;
use std::fs;
use std::io::{BufReader, Read};

pub fn compile(input_file_name: &str, output_file_name: &str) -> Result<(), String> {
    println!("input:{}", input_file_name);
    compile_from_str(&src_file_to_str(input_file_name), output_file_name)
}

pub fn compile_from_str(str: &str, output_file_name: &str) -> Result<(), String> {
    /*  match parse(str) {
          Ok((program_ir, assump)) => Ok(output_file(program_ir.code_gen(output_file_name, assump))),
          Err(err) => Err(err),
      }*/
    match parse(str) {
        Ok((program_ir, assump)) => panic!("stop"),
        Err(err) => Err(err),
    }
}

pub fn src_file_to_str(file_name: &str) -> String {
    let mut f = BufReader::new(fs::File::open(file_name).unwrap());
    let mut src_str: String = "".to_string();
    f.read_to_string(&mut src_str).unwrap();
    src_str
}

pub fn parse(src_str: &str) -> Result<(mir::ProgramMir, AssumpEnv), String> {
    match parser::parse(src_str) {
        Ok(ast) => {
            println!("\nparse\n{:?}\n", ast);
            let result = semantic_analysis::analysis(ast.0);
            match result {
                Ok((ir, assump)) => {
                    println!("resolve_op\n\n{:?}\n", ir);
                    Result::Ok((ir, assump))
                }
                Err(err) => Result::Err(err.to_string()),
            }
        }
        Err(err) => Result::Err(Error::from_parse_error(err).to_string()),
    }
}
