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
    if cfg!(target_os = "windows") {
        let lib_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        let current_dir = env::current_dir().unwrap().to_str().unwrap().to_string();
        fs::copy(lib_dir + "\\compile.bat", current_dir + "\\tests\\compile.bat")
            .expect("compile.batのコピーに失敗しました");
    }
}
