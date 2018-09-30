use super::ir_tree::DecFuncIr;
use std::collections::HashMap;
use super::super::types::{Type, FuncType};

pub fn get_buildin_func_list() -> HashMap<String, DecFuncIr> {
    let print = DecFuncIr {
        name: "print".to_string(),
        ty: FuncType { param_types: vec![Type::Int32], ret_type: Type::Int32 },
    };
    let scan = DecFuncIr {
        name: "scan".to_string(),
        ty: FuncType { param_types: vec![Type::Int32], ret_type: Type::Int32 },
    };
    let mut func_list = HashMap::new();
    func_list.insert("print".to_string(), print);
    func_list.insert("scan".to_string(), scan);
    func_list
}