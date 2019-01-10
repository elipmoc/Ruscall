extern crate indexmap;

use crate::compile::ir::mir::*;
use std::collections::HashMap;
use self::indexmap::set::IndexSet;

pub struct Binding<'a> {
    bindings: IndexSet<String>,
    func_mir_list: HashMap<String, &'a FuncMir>,
}

impl<'a> Binding<'a> {
    // 関数の依存度を解析し、依存していない順に並び変えた関数名リストを返す
    pub fn create_binding_group(func_mir_list: HashMap<String, &FuncMir>) -> IndexSet<String> {
        let mut binding = Binding {
            bindings: IndexSet::new(),
            func_mir_list,
        };
        loop {
            let key = binding.func_mir_list.keys().nth(0).cloned();
            match key {
                Some(key) => {
                    binding = binding.get_func_binding_group(&key);
                }
                None => { return binding.bindings; }
            }
        }
    }

    fn get_func_binding_group(mut self, func_name: &String) -> Binding<'a> {
        if self.bindings.contains(func_name) || self.func_mir_list.contains_key(func_name)==false {
            return self;
        }
        let func = self.func_mir_list.remove(func_name).unwrap();
        let mut binding = self.get_expr_binding_group(&func.body);
        binding.bindings.insert(func_name.clone());
        binding
    }

    fn get_expr_binding_group(self, expr_mir: &ExprMir) -> Binding<'a> {
        use self::ExprMir::*;
        match expr_mir {
            BoolMir(_) | NumMir(_) | VariableMir(_) => self,
            OpMir(x) => {
                let binding = self.get_expr_binding_group(&x.r_expr);
                binding.get_expr_binding_group(&x.l_expr)
            }
            GlobalVariableMir(x) => {
                self.get_func_binding_group(&x.id)
            }
            IfMir(x) => {
                let binding = self.get_expr_binding_group(&x.t_expr);
                binding.get_expr_binding_group(&x.f_expr)
            }
            IndexPropertyMir(x) => {
                self.get_expr_binding_group(&x.expr)
            }
            CallMir(x) => {
                let binding = self.get_expr_binding_group(&x.func);
                x.params.iter().fold(binding, |acc, x| {
                    acc.get_expr_binding_group(x)
                })
            }
            TupleStructMir(x) => {
                let binding = self;
                x.tuple.elements.iter().fold(binding, |acc, x| {
                    acc.get_expr_binding_group(x)
                })
            }
            ExprMir::TupleMir(x) => {
                let binding = self;
                x.elements.iter().fold(binding, |acc, x| {
                    acc.get_expr_binding_group(x)
                })
            }
            ExprMir::LambdaMir(x) => {
                self.get_func_binding_group(&x.func_name)
            }
            ExprMir::NamePropertyMir(x) => {
                self.get_expr_binding_group(&x.expr)
            }
        }
    }
}

#[test]
fn binding_group_test() {
    use combine::stream::state::SourcePosition;
    fn create_func_mir(name: &str) -> FuncMir {
        FuncMir {
            name: name.to_string(),
            body: ExprMir::NumMir(NumMir::new("5".to_string(), SourcePosition::new())),
            params_len: 0,
            pos: SourcePosition::new(),
        }
    }
    fn create_nest_func_mir(name: &str, f_name: &str) -> FuncMir {
        FuncMir {
            name: name.to_string(),
            body: ExprMir::GlobalVariableMir(GlobalVariableMir::new(f_name.to_string(), SourcePosition::new())),
            params_len: 0,
            pos: SourcePosition::new(),
        }
    }

    let a = create_nest_func_mir("a", "b");
    let b = create_nest_func_mir("b", "c");
    let c = create_nest_func_mir("c", "a");
    let mut h = HashMap::new();
    h.insert("a".to_string(), &a);
    h.insert("b".to_string(), &b);
    h.insert("c".to_string(), &b);
    let mut bindings = Binding::create_binding_group(h);
    let mut iter = bindings.iter();
    assert_eq!(iter.next().unwrap(), &"c".to_string());
    assert_eq!(iter.next().unwrap(), &"b".to_string());
    assert_eq!(iter.next().unwrap(), &"a".to_string());
}