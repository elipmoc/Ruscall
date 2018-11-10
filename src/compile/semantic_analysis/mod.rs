pub mod ast_to_ir;
pub mod ir;
pub mod type_env;
pub mod type_get;
pub mod variable_table;
pub mod type_variable_table;

use super::ast::ProgramAST;

use super::error::Error;

pub fn analysis(ast: ProgramAST) -> Result<ir::ProgramIr, Error> {
    let ir = ast.ast_transformer()?.to_ir()?;
    let ir = ir.ty_get()?;
    let ty_resolved = ir.ty_info.get_type_resolved();
    println!("{:?}", ty_resolved);
    Result::Ok(ir)
}
