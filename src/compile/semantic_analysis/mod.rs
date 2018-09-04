mod resolve_op;
mod ast_to_ir;
pub mod ir_tree;

pub use super::ast::ProgramAST;

use self::resolve_op::{resolve_op, ResolveOpResult};

pub fn analysis(ast: ProgramAST) -> ResolveOpResult {
    resolve_op(ast)
}