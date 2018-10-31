pub mod ast_to_ir;
pub mod ir;
mod resolve_op;
pub mod type_get;
pub mod type_env;

pub use super::ast::ProgramAST;

use self::resolve_op::resolve_op;
use super::error::Error;
use self::type_env::TypeInfo;

pub fn analysis(ast: ProgramAST) -> Result<(ir::ProgramIr, TypeInfo), Error> {
    let (ir, ty_info) = resolve_op(ast)?.to_ir();
    let ty_info = ir.ty_get(ty_info)?;
    let ty_resolved = ty_info.get_type_resolved();
    println!("{:?}", ty_resolved);
    Result::Ok((ir, ty_info))
}
