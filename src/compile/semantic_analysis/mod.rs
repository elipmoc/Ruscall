pub mod hir_to_mir;
pub mod type_get;
pub mod variable_table;
pub mod type_variable_table;
pub mod type_inference;
pub mod binding_group;

pub use self::type_inference::type_env;
use super::ir::mir;
use super::ir::ast;
use super::error::Error;
use self::binding_group::Binding;

pub fn analysis(ast: ast::ProgramAST) -> Result<mir::ProgramMir, Error> {
    let ir = ast.ast_transformer()?.to_mir()?;
    println!("\nType Inference \n");
    let ir = ir.ty_get()?;
    let ty_resolved = ir.ty_info.get_type_resolved();
    println!("\nGlobal Item Type List \n");
    println!("{:?}", ty_resolved);
    println!("\nBinding Group List \n");
    println!("{:?}", Binding::create_binding_group(ir.func_list.iter().map(|x| (x.name.clone(), x)).collect()));
    Result::Ok(ir)
}
