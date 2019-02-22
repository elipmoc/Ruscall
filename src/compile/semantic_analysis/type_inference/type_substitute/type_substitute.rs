use std::collections::HashMap;
use crate::compile::types::*;

//型代入環境
#[derive(Debug, PartialEq, Clone)]
pub struct TypeSubstitute {
    pub ty_sub: HashMap<TypeId, Type>,
}

impl TypeSubstitute {
    pub fn new() -> Self {
        TypeSubstitute { ty_sub: HashMap::new() }
    }
}