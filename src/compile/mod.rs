mod ast;
mod parser;

use std::io::{BufReader, Read};
use std::{fs, mem};

pub fn compile(file_name: &str) {
    println!("input:{}", file_name);
    let mut f = BufReader::new(fs::File::open(file_name).unwrap());
    let mut src_str: String = "".to_string();
    f.read_to_string(&mut src_str).unwrap();
    println!("{:?}", parser::parse(&src_str));
}
