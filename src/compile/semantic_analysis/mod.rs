pub mod ast_to_ir;
pub mod ir_tree;
mod resolve_op;
pub mod type_get;
pub mod type_env;

pub use self::ir_tree as ir;
pub use super::ast::ProgramAST;

use self::resolve_op::resolve_op;
use super::error::Error;
use self::type_env::TypeResolved;

pub fn analysis(ast: ProgramAST) -> Result<(ir::ProgramIr, TypeResolved), Error> {
    let ir = resolve_op(ast)?.to_ir();
    let (ty_env, ty_subst) = ir.ty_get()?;
    let ty_resolved = TypeResolved::new(ty_env, ty_subst);
    Result::Ok((ir, ty_resolved))
}
