extern crate inkwell;

use self::inkwell::*;
use super::code_gen::CodeGenResult;
use std::fs::File;
use std::path::Path;


//コードを実行形式で出力
pub fn output_file(code_gen_result: CodeGenResult) {
    let CodeGenResult { module, file_name, .. } = code_gen_result;

    use std::env;
    let triple = &targets::TargetMachine::get_default_triple().to_string();
    let target_machine =
        targets::Target::from_triple(triple).unwrap().create_target_machine(
            triple,
            "generic",
            "",
            OptimizationLevel::Default,
            targets::RelocMode::Default,
            targets::CodeModel::Default,
        ).unwrap();
    module.write_bitcode_to_file(&File::create(file_name.to_string() + ".bc").unwrap(), true, true);
    target_machine.write_to_file(
        &module,
        targets::FileType::Object,
        &Path::new(&(file_name.to_string() + ".obj")),
    ).unwrap();

    let current_dir = env::current_dir().unwrap().to_str().unwrap().to_string();

    if cfg!(target_os = "windows") {
        let compile_bat_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
        command_exec(
            "cmd",
            &[
                "/C",
                &(compile_bat_dir + "\\compile.bat"),
                &(current_dir + "\\" + file_name + ".obj"),
            ],
        );
    } else {
        command_exec(
            "sh",
            &[
                "-c",
                &("g++ ".to_owned()
                    + &(current_dir.clone() + "/" + file_name + ".obj ")
                    + &(current_dir.clone() + "/" + "libtest.a ")
                    + "-o "
                    + &(current_dir + "/" + file_name + ".out")),
            ],
        );
    };
}

use std::ffi::OsStr;

fn command_exec<I, S>(terminal: &str, args: I)
    where
        I: IntoIterator<Item=S> + Clone,
        S: AsRef<OsStr>,
{
    use std::process::Command;

    let output = Command::new(terminal)
        .args(args.clone())
        .output()
        .expect("failed to execute process");
    println!("status: {}", output.status);
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    println!(
        "command: {}",
        args.into_iter()
            .fold("".to_string(), |acc, x| acc.to_owned()
                + " "
                + x.as_ref().to_str().unwrap())
    );
}