mod resolve_op;
mod currying_func;

use super::ast::ProgramAST;
use super::error::Error;

impl ProgramAST {
    pub fn ast_transformer(self) -> Result<ProgramAST, Error> {
        Ok(self.resolve_op()?.currying())
    }
}