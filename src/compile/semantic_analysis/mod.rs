mod resolve_op;
mod ast_to_ir;
pub mod ir_tree;

pub use super::ast::ProgramAST;
pub use self::ir_tree as ir;

use super::error::Error;
use self::resolve_op::{resolve_op};

pub fn analysis(ast: ProgramAST) -> Result<ir::ProgramIr,Error> {
    resolve_op(ast)?.to_ir()
}