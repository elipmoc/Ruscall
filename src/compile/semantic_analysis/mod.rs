mod resolve_op;
pub mod ast_to_ir;
pub mod ir_tree;
pub mod global_variable_table;

pub use super::ast::ProgramAST;
pub use self::ir_tree as ir;

use self::global_variable_table::ConfirmGlobalVariableTable;
use super::error::Error;
use self::resolve_op::{resolve_op};

pub fn analysis(ast: ProgramAST) -> Result<(ir::ProgramIr),Error> {
    resolve_op(ast)?.to_ir()
}