use super::ast;

//優先順位ごとのinfixのまとまり
#[derive(Debug, Clone)]
pub struct Infixes {
    pub priority: i8,
    pub list: Vec<ast::InfixAST>,
}

impl Infixes {
    pub fn new(priority: i8) -> Infixes {
        Infixes {
            priority: priority,
            list: vec![],
        }
    }
}

use std::cmp::Ordering;

impl Ord for Infixes {
    fn cmp(&self, other: &Infixes) -> Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PartialOrd for Infixes {
    fn partial_cmp(&self, other: &Infixes) -> Option<Ordering> {
        Some(self.priority.cmp(&other.priority))
    }
}

impl PartialEq for Infixes {
    fn eq(&self, other: &Infixes) -> bool {
        self.priority == other.priority
    }
}

impl Eq for Infixes {}
