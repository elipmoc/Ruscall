pub mod hir_to_mir;
pub mod type_get;
pub mod variable_table;
pub mod type_variable_table;
pub mod type_inference;

pub use self::type_inference::type_env;
use super::ir::mir;
use super::ir::ast;
use super::error::Error;

pub fn analysis(ast: ast::ProgramAST) -> Result<mir::ProgramMir, Error> {
    let ir = ast.ast_transformer()?.to_mir()?;
    let ir = ir.ty_get()?;
    let ty_resolved = ir.ty_info.get_type_resolved();
    println!("{:?}", ty_resolved);
    Result::Ok(ir)
}
