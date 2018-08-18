pub mod ast;
pub mod code_gen;
pub mod parser;
pub mod resolve_op;

use std::fs;
use std::io::{BufReader, Read};

pub fn compile(file_name: &str) {
    println!("input:{}", file_name);
    let mut f = BufReader::new(fs::File::open(file_name).unwrap());
    let mut src_str: String = "".to_string();
    f.read_to_string(&mut src_str).unwrap();
    let src_str: &str = &src_str;
    match parser::parse(src_str) {
        Ok(ast) => {
            println!("\nparse\n{:?}\n", ast);
            let result_ast = resolve_op::resolve_op(ast.0);
            match result_ast {
                Ok(ast) => {
                    println!("resolve_op\n{:?}\n", ast);
                    code_gen::code_gen(ast, "compiled");
                }
                _ => println!("compile error!\n resolve_op\n"),
            }
        }
        Err(err) => println!(
            "compile error!\nposition:\nline:{} column:{}\n\nmessage:\n{:?}",
            err.position.line, err.position.column, err.errors
        ),
    }
}
