pub mod ast_to_ir;
pub mod ir_tree;
mod resolve_op;
pub mod type_check;

pub use self::ir_tree as ir;
pub use super::ast::ProgramAST;

use self::resolve_op::resolve_op;
use super::error::Error;

pub fn analysis(ast: ProgramAST) -> Result<(ir::ProgramIr), Error> {
    Result::Ok(resolve_op(ast)?.to_ir().ty_check()?)
}
