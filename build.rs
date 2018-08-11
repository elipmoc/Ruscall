extern crate cc;

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
