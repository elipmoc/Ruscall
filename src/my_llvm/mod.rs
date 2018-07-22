pub mod codegen;
pub mod execution_engine;
pub mod function;
pub mod generic_value;
mod helper;
pub mod module;
pub mod pass;
pub mod target;
pub mod types;
pub mod value;

pub mod easy {
    pub use my_llvm::codegen::*;
    pub use my_llvm::execution_engine::*;
    pub use my_llvm::function::*;
    pub use my_llvm::generic_value::*;
    pub use my_llvm::module::*;
    pub use my_llvm::pass::*;
    pub use my_llvm::target::*;
    pub use my_llvm::types::*;
    pub use my_llvm::value::*;
}
