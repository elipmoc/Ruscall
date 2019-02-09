use std::collections::HashMap;
use crate::compile::types::*;
use super::super::type_env::TypeEnv;

//型代入環境
#[derive(Debug, PartialEq, Clone)]
pub struct TypeSubstitute {
    pub ty_sub: HashMap<TypeId, Type>,
    pub ty_env: TypeEnv,
}

impl TypeSubstitute {
    pub fn new() -> Self {
        TypeSubstitute { ty_sub: HashMap::new(), ty_env: TypeEnv::new() }
    }
}