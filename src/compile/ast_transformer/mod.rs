mod resolve_op;
mod currying_func;
mod ast_to_hir;

use super::ir::ast::ProgramAST;
use super::ir::hir::ProgramHir;
use super::error::Error;

impl ProgramAST {
    pub fn ast_transformer(self) -> Result<ProgramHir, Error> {
        Ok(self.to_hir()?.resolve_op()?.currying())
    }
}