extern crate llvm_sys as llvm;
use self::llvm::prelude::LLVMValueRef;
use super::helper::*;

pub fn set_value_name(val: LLVMValueRef, name: &str) {
    unsafe { llvm::core::LLVMSetValueName(val, string_cast(name).as_ptr()) }
}
