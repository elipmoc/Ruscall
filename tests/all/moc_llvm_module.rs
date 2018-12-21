extern crate inkwell;

use self::inkwell::*;
use self::inkwell::types::*;

pub fn create_moc_llvm_module() -> module::Module {
    let module = module::Module::create("moc_llvm_module");
    let builder = builder::Builder::create();
    create_func("print", &module, &builder);
    create_func("scan", &module, &builder);
    module
}

fn create_func(name: &str, module: &module::Module, builder: &builder::Builder) {
    let func = module.add_function(
        name,
        IntType::i32_type().fn_type(
            &[IntType::i32_type().as_basic_type_enum()],
            false,
        ),
        None,
    );
    let block = func.append_basic_block("entry");
    builder.position_at_end(&block);
    builder.build_return(Some(&func.get_params()[0]));
}