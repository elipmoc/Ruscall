extern crate cc;

use std::env;
use std::fs;

fn main() {
    cc::Build::new()
        .out_dir("./")
        .file("src/stdlib/c/hello.c")
        .compile("hello");
    cc::Build::new()
        .out_dir("./")
        .file("src/stdlib/c/test.c")
        .compile("test");
}
