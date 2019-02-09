use std::collections::HashMap;
use super::super::types::types::Type;
use super::type_env::TypeInfo;

pub struct TypeVariableTable(HashMap<String, Type>);

impl TypeVariableTable {
    pub fn new() -> TypeVariableTable {
        TypeVariableTable(HashMap::new())
    }

    //型変数の名前からTypeを取得する
    pub fn get_ty(&mut self, ty_var_name: String, ty_info: &mut TypeInfo) -> Type {
        if self.0.contains_key(&ty_var_name) {
            self.0[&ty_var_name].clone()
        } else {
            let ty_var = ty_info.no_name_get();
            self.0.insert(ty_var_name, ty_var.clone());
            ty_var
        }
    }
}