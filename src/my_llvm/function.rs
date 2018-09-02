extern crate llvm_sys as llvm;

use self::llvm::prelude::LLVMTypeRef;
use self::llvm::prelude::LLVMValueRef;
use super::helper::*;

extern crate libc;

use std::mem;

pub struct Function {
    pub llvm_function: LLVMValueRef,
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
    pub fn get_params(&self, count: usize) -> Vec<LLVMValueRef> {
        unsafe {
            let params: *mut LLVMValueRef = libc::malloc(mem::size_of::<LLVMValueRef>()*count) as *mut LLVMValueRef;
            llvm::core::LLVMGetParams(self.llvm_function, params);
            Vec::from_raw_parts(params, count, count)
        }
    }
}
