pub mod ast_to_ir;
pub mod ir;
mod resolve_op;
pub mod type_get;
pub mod type_env;
pub mod variable_table;

use super::ast::ProgramAST;

use self::resolve_op::resolve_op;
use super::error::Error;
use self::type_env::TypeInfo;

pub fn analysis(ast: ProgramAST) -> Result<ir::ProgramIr, Error> {
    let ir = resolve_op(ast)?.to_ir()?;
    let ir = ir.ty_get()?;
    let ty_resolved = ir.ty_info.get_type_resolved();
    println!("{:?}", ty_resolved);
    Result::Ok(ir)
}
