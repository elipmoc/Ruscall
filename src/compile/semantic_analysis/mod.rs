pub use super::ast::ProgramAST;

pub mod resolve_op;

use self::resolve_op::{resolve_op, ResolveOpResult};

pub fn analysis(ast: ProgramAST) -> ResolveOpResult {
    resolve_op(ast)
}