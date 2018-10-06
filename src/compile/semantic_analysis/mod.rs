mod resolve_op;
pub mod ast_to_ir;
pub mod ir_tree;
pub mod type_check;
pub mod buildin_func;

pub use super::ast::ProgramAST;
pub use self::ir_tree as ir;

use super::error::Error;
use self::resolve_op::resolve_op;
use self::buildin_func::get_buildin_func_list;

pub fn analysis(ast: ProgramAST) -> Result<(ir::ProgramIr), Error> {
    Result::Ok(
        resolve_op(ast)?
            .to_ir(get_buildin_func_list())?
            .ty_check()?
    )
}