extern crate ruscall;

use ruscall::compile::parse;

macro_rules! helper {
    ($file_name:expr) => {
        match parse(include_str!(concat!("test_data/",$file_name,".rsc")))
            {
                Ok(ir) => {ir.code_gen($file_name);},
                Err(err) => assert!(false, "error!".to_string() + &err)
            };
    };
}
#[test]
fn code_gen_test() {
   helper!("test1");
   helper!("test2");
   helper!("test3");
   helper!("test4");
   helper!("test5");
}