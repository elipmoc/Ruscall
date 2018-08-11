pub mod ast;
pub mod infixes;
pub mod parser;
pub mod resolve_op;

use std::fs;
use std::io::{BufReader, Read};

pub fn compile(file_name: &str) {
    println!("input:{}", file_name);
    let mut f = BufReader::new(fs::File::open(file_name).unwrap());
    let mut src_str: String = "".to_string();
    f.read_to_string(&mut src_str).unwrap();
    match parser::parse(&src_str) {
        Ok(ast) => {
            println!("{:?}", ast);
            println!("resolve_op {:?}", resolve_op::resolve_op(ast.0));
        }
        Err(_) => println!("err!!"),
    }
}
