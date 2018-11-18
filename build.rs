extern crate cc;


fn main() {
    use std::env;
    use std::path::Path;

    //親プロジェクトのpathを取得
    let current_dir = Path::new(&env::var("OUT_DIR").unwrap())
        .parent().unwrap().parent().unwrap().parent().unwrap().parent().unwrap().parent().unwrap().to_str().unwrap().to_string();

    //cのライブラリを作成
    cc::Build::new()
        .out_dir(current_dir.clone())
        .file("src/stdlib/c/hello.c")
        .compile("hello");
    cc::Build::new()
        .out_dir(current_dir.clone())
        .file("src/stdlib/c/test.c")
        .compile("test");

    //windowsのみcompile.batファイルを親プロジェクトにコピー
    if cfg!(target_os = "windows") {
        use std::fs;
        let lib_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        if lib_dir != current_dir {
            fs::copy(lib_dir + "\\compile.bat", current_dir + "\\compile.bat")
                .expect("compile.batのコピーに失敗しました");
        }
    }
}
