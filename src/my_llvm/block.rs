extern crate llvm_sys as llvm;

use self::llvm::prelude::*;
use super::helper::string_cast;

pub fn insert_basic_block(insert_before_block: LLVMBasicBlockRef, name: &str) -> LLVMBasicBlockRef {
    unsafe {
        llvm::core::LLVMInsertBasicBlock(insert_before_block, string_cast(name).as_ptr())
    }
}
