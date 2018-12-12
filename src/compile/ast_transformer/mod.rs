mod resolve_op;
mod currying_func;
mod ast_to_hir;
mod resolve_named_params_constructor_call;
mod create_constructor;

use super::ir::ast::ProgramAST;
use super::ir::hir::ProgramHir;
use super::error::Error;

impl ProgramAST {
    pub fn ast_transformer(self) -> Result<ProgramHir, Error> {
        Ok(self.to_hir()?.resolve_op()?.resolve_named_params_constructor_call()?.create_constructor().currying())
    }
}