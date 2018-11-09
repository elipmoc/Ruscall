extern crate ruscall;

use ruscall::compile::parse;

macro_rules! helper {
    ($file_name:ident) => {
        #[test]
        fn $file_name(){
            match parse(include_str!(concat!("test_data/", stringify!($file_name), ".rsc"))) {
                Ok(_ir) => {
                    //ir.code_gen(stringify!($file_name));
                }
                Err(err) => assert!(false, "error!".to_string() + &err),
            };
        }
    };
}

helper!(test1);
helper!(test2);
helper!(test3);
helper!(test4);
helper!(test5);
helper!(test6);
helper!(test7);
helper!(test8);
helper!(test9);