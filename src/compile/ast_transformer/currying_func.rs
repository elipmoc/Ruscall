use crate::compile::ast::*;

//ASTを組み替えてカリー化を行う

impl ProgramAST {
    pub fn currying(mut self) -> ProgramAST {
        self.stmt_list =
            self.stmt_list
                .into_iter()
                .map(StmtAST::currying).collect();
        self
    }
}

impl StmtAST {
    fn currying(self) -> StmtAST {
        match self {
            StmtAST::DefFuncAST(x) => StmtAST::DefFuncAST(x.currying()),
            _ => self
        }
    }
}

impl DefFuncAST {
    fn currying(mut self) -> DefFuncAST {
        if self.params.len() == 0 {
            self.params.push(VariableAST { pos: self.pos, id: "_".to_string() });
        }
        let first_param = self.params[0].clone();
        self.body = self.body.currying(self.params.into_iter().skip(1), vec![first_param.clone()]);
        self.params = vec![first_param];
        self
    }
}

impl ExprAST {
    fn currying<I: Iterator<Item=VariableAST>>(self, mut iter: I, env: Vec<VariableAST>, ) -> ExprAST {
        match iter.next() {
            Some(v) => {
                let mut next_env = env.clone();
                next_env.push(v.clone());
                ExprAST::LambdaAST(Box::new(LambdaAST {
                    pos: self.get_pos(),
                    body: self.currying(iter, next_env),
                    env,
                    params: vec![v],
                }))
            }
            _ => self
        }
    }
}
