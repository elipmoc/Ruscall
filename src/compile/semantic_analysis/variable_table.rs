use std::collections::HashMap;

#[derive(Clone)]
pub struct VariableTable {
    local_var_names: Vec<Vec<String>>,
    global_var_names: HashMap<String, ()>,
    nest_level: usize,
}

use super::super::ast::VariableAST;
use super::type_env::TypeInfo;
use super::ir::ExprIr;

//変数の管理
impl VariableTable {
    pub fn new(global_var_names: HashMap<String, ()>) -> VariableTable {
        VariableTable { global_var_names, local_var_names: vec![], nest_level: 0 }
    }

    //de bruijn indexを割り当てたVariableIrの生成
    //またはGlobalVariableIrの生成
    pub fn get_variable_ir(&self, var: VariableAST, ty_info: &mut TypeInfo) -> Option<ExprIr> {
        let a = self.local_var_names[self.nest_level - 1]
            .iter().rev().enumerate()
            .find(|(_, name)| *name == &var.id)
            .map(|(id, _)| id);
        match a {
            Some(id) => Some(ExprIr::create_variableir(id, var.pos, ty_info.no_name_get())),
            _ => {
                if self.global_var_names.contains_key(&var.id){
                    Some(ExprIr::GlobalVariableIr(var))
                } else{
                    None
                }
            }
        }
    }
    pub fn in_nest<T>(&mut self, iter: T)
        where T: IntoIterator<Item=String> {
        if self.local_var_names.len() <= self.nest_level {
            self.local_var_names.push(iter.into_iter().collect());
        } else {
            self.local_var_names[self.nest_level].clear();
            self.local_var_names[self.nest_level].extend(iter);
        }
        self.nest_level += 1;
    }
    pub fn out_nest(&mut self) {
        self.nest_level -= 1;
    }
}