extern crate ruscall;
extern crate inkwell;

use self::inkwell::OptimizationLevel;
use self::inkwell::execution_engine::JitFunction;
use self::ruscall::compile::parse;
use self::ruscall::compile::code_gen::CodeGenResult;
use super::moc_llvm_module::create_moc_llvm_module;

type MainFunc = unsafe extern "C" fn() -> i32;

macro_rules! helper {
    ($file_name:ident,$expect:expr) => {
        #[test]
        fn $file_name(){
            match parse(include_str!(concat!("test_data/", stringify!($file_name), ".rsc"))) {
                Ok((ir,assump)) => {
                    let CodeGenResult{ module , .. } = ir.code_gen(stringify!($file_name),assump);
                    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
                    assert!(execution_engine.add_module(&create_moc_llvm_module()).is_ok(),"add_module error!");
                    unsafe {
                        let funcion_in_rust: JitFunction<MainFunc> = execution_engine.get_function("main").unwrap();
                            assert_eq!(
                                $expect, funcion_in_rust.call()
                            );
                    };
                }
                Err(err) => assert!(false, "error!".to_string() + &err),
            };
        }
    };
}

helper!(test1,4);
helper!(test2,76);
helper!(test3,7);
helper!(test4,6);
helper!(test5,0);
helper!(test6,4);
helper!(test7,6);
helper!(test8,7);
helper!(test9,9);
helper!(test10,15);
helper!(test11,12);
helper!(test12,65);
helper!(fact,120);
helper!(tuple_property,14);
helper!(record_property,20);
helper!(record_name_property,11);
helper!(quantify,25);
helper!(explicit_quantify,7);