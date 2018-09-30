use super::super::my_llvm::easy::*;
use super::code_gen::CodeGenResult;

//コードを実行形式で出力
pub fn output_file(code_gen_result:CodeGenResult) {
    let module=code_gen_result.module;
    let file_name=code_gen_result.file_name;
    let code_gen=code_gen_result.code_gen;

    use std::env;
    let target_machine = TargetMachine::create(
        "generic",
        "",
        LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
        LLVMRelocMode::LLVMRelocDefault,
        LLVMCodeModel::LLVMCodeModelDefault,
    ).unwrap();
    module.set_data_layout(target_machine.create_data_layout());
    module.set_target_triple(target_machine.target_triple);
    module.write_bitcode_to_file(&(file_name.to_string() + ".bc"));
    target_machine.emit_to_file(
        &module,
        &(file_name.to_string() + ".obj"),
        LLVMCodeGenFileType::LLVMObjectFile,
    );
    module.dispose_module();
    code_gen.dispose();
    target_machine.dispose();

    let current_dir = env::current_dir().unwrap().to_str().unwrap().to_string();
    if cfg!(target_os = "windows") {
        command_exec(
            "cmd",
            &[
                "/C",
                &(current_dir.clone() + "\\compile.bat"),
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