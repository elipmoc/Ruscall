use std::collections::HashMap;

#[derive(Clone)]
pub struct VariableTable {
    local_var_names: Vec<Vec<String>>,
    global_var_names: HashMap<String, ()>,
    nest_level: usize,
}

use super::super::ir::ast::VariableAST;
use super::type_inference::type_env::TypeEnv;
use super::mir::ExprMir;

//変数の管理
impl VariableTable {
    pub fn new(global_var_names: HashMap<String, ()>) -> VariableTable {
        VariableTable { global_var_names, local_var_names: vec![], nest_level: 0 }
    }

    //de bruijn indexを割り当てたVariableIrの生成
    //またはGlobalVariableIrの生成
    pub fn get_variable_ir(&self, var: VariableAST, ty_env: &mut TypeEnv) -> Option<ExprMir> {
        let a = self.local_var_names[self.nest_level - 1]
            .iter().rev().enumerate()
            .find(|(_, name)| *name == &var.id)
            .map(|(id, _)| id);
        match a {
            Some(id) => Some(ExprMir::create_variable_mir(id, var.pos, ty_env.fresh_type_id())),
            _ => {
                if self.global_var_names.contains_key(&var.id) {
                    Some(ExprMir::create_global_variable_mir(var.id, var.pos, ty_env.fresh_type_id()))
                } else {
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