use std::collections::HashMap;


pub struct GlobalVariableTable(HashMap<String, Option<usize>>);

impl GlobalVariableTable {
    pub fn new() -> GlobalVariableTable {
        GlobalVariableTable(HashMap::new())
    }

    pub fn register_func_name(&mut self, name: String) {
        if self.0.contains_key(&name) == false {
            self.0.insert(name, None);
        };
    }

    pub fn register_func(&mut self, name: String, param_size: usize) {
        self.0.insert(name, Some(param_size));
    }

    pub fn get_confirm(self) -> Option<ConfirmGlobalVariableTable> {
        let mut confirm_list: Vec<(String, usize)> = Vec::with_capacity(self.0.len());
        for (k, v) in self.0 {
            match v {
                None => return None,
                Some(x) => confirm_list.push((k, x))
            }
        };
        Some(ConfirmGlobalVariableTable(confirm_list.into_iter().collect()))
    }
}

#[derive(Debug, PartialEq)]
pub struct ConfirmGlobalVariableTable(pub HashMap<String, usize>);
