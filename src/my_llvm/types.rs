extern crate llvm_sys as llvm;
use std::os::raw::c_char;

pub type TargetDataRef = *mut llvm::target::LLVMOpaqueTargetData;
pub type TargetTriple = *mut c_char;
use self::llvm::prelude::LLVMTypeRef;

pub fn int32_type() -> LLVMTypeRef {
    unsafe { llvm::core::LLVMInt32Type() }
}

pub fn void_type() -> LLVMTypeRef {
    unsafe { llvm::core::LLVMVoidType() }
}

pub fn function_type(ret_type: LLVMTypeRef, mut param_types: Vec<LLVMTypeRef>) -> LLVMTypeRef {
    unsafe {
        let function_type = llvm::core::LLVMFunctionType(
            ret_type,
            param_types.as_mut_ptr(),
            param_types.len() as u32,
            0,
        );
        function_type
    }
}
