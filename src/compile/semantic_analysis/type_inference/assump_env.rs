use compile::types::types::Scheme;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct AssumpEnv {
    env: Vec<HashMap<String, Scheme>>,
    nest: usize,
}

//型環境
impl AssumpEnv {
    pub fn new() -> AssumpEnv {
        AssumpEnv {
            env: vec![HashMap::new()],
            nest: 0,
        }
    }

    //環境をネスト
    pub fn in_nest(&mut self) {
        self.nest += 1;
        self.env.push(HashMap::new());
    }

    //環境のネストを抜ける
    pub fn out_nest(&mut self) {
        self.nest -= 1;
        self.env.pop();
    }

    pub fn global_get(&self, symbol: &String) -> Option<&Scheme> {
        self.env[0].get(symbol)
    }

    pub fn get(&self, symbol: &String) -> Option<&Scheme> {
        self.env[self.nest].get(symbol)
    }

    pub fn global_set(&mut self, symbol: String, q: Scheme) {
        println!("global_set {} {:?}", symbol, q);
        self.env[0].insert(symbol, q);
    }

    pub fn set(&mut self, symbol: String, q: Scheme) {
        self.env[self.nest].insert(symbol, q);
    }
}