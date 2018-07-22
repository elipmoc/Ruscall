extern crate llvm_sys as llvm;
use self::llvm::prelude::LLVMTypeRef;
use super::helper::*;
pub struct Function {
    pub llvm_function: *mut llvm::LLVMValue,
}

impl Function {
    pub fn new(f_name: &str, module: &super::module::Module, f_type: LLVMTypeRef) -> Function {
        unsafe {
            Function {
                llvm_function: llvm::core::LLVMAddFunction(
                    module.llvm_module,
                    string_cast(f_name).as_ptr(),
                    f_type,
                ),
            }
        }
    }
    pub fn append_basic_block(&self, name: &str) -> *mut llvm::LLVMBasicBlock {
        unsafe { llvm::core::LLVMAppendBasicBlock(self.llvm_function, string_cast(name).as_ptr()) }
    }
}
