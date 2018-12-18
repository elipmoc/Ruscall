extern crate inkwell;

use self::inkwell::{targets, types, OptimizationLevel, passes, builder, module, values, context};
use std::env;
use std::process::Command;
use std::fs::File;
use std::path::Path;

fn extern_foo(module: &module::Module) -> values::FunctionValue {
    module.add_function(
        "foo",
        types::VoidType::void_type().fn_type(&vec![], false),
        Some(module::Linkage::External),
    )
}

pub fn hello() {
    targets::Target::initialize_all(&targets::InitializationConfig::default());
    let module = module::Module::create("my_module");
    let builder = builder::Builder::create();
    let foo_func = extern_foo(&module);
    let function =
        module.add_function(
            "main",
            types::IntType::i32_type().fn_type(&vec![], false),
            None,
        );

    let entry_block = function.append_basic_block("entry");
    builder.position_at_end(&entry_block);
    builder.build_call(foo_func, &vec![], "");
    builder.build_return(Some(
        &types::IntType::i32_type().const_int(0, false)
    ));
    let fpm = passes::PassManager::create_for_function(&module);
    fpm.add_promote_memory_to_register_pass();
    fpm.run_on_function(&function);
    if let Err(err_msg) = module.verify() {
        panic!("llvm error:{}", err_msg);
    }
    module.print_to_stderr();
    let triple = &targets::TargetMachine::get_default_triple().to_string();
    //   module.get_target().unwrap();
    let target_machine =
        targets::Target::from_triple(triple).unwrap().create_target_machine(
            triple,
            "generic",
            "",
            OptimizationLevel::Default,
            targets::RelocMode::Default,
            targets::CodeModel::Default,
        ).unwrap();
    module.write_bitcode_to_file(&File::create("hoge.bc").unwrap(), true, true);
    target_machine.write_to_file(&module, targets::FileType::Object, Path::new("hoge.obj")).unwrap();

    let current_dir = env::current_dir().unwrap().to_str().unwrap().to_string();
    if cfg!(target_os = "windows") {
        Command::new("cmd")
            .args(&[
                "/C",
                &(current_dir.clone() + "\\compile.bat"),
                &(current_dir + "\\hoge.obj"),
            ])
            .output()
            .expect("failed to execute process")
    } else {
        panic!("support windows only!!")
    };
}
