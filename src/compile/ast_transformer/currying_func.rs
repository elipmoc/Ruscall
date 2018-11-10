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
        self.body = currying(self.params.into_iter().skip(1), vec![first_param.clone()], self.body);
        self.params = vec![first_param];
        self
    }
}

fn currying<I: Iterator<Item=VariableAST>>(mut iter: I, env: Vec<VariableAST>, body: ExprAST) -> ExprAST {
    match iter.next() {
        Some(v) => {
            let mut next_env = env.clone();
            next_env.push(v.clone());
            ExprAST::LambdaAST(Box::new(LambdaAST {
                pos: body.get_pos(),
                body: currying(iter, next_env, body),
                env,
                params: vec![v],
            }))
        }
        _ => body
    }
}
