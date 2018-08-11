extern crate llvm_sys as llvm;
use self::llvm::prelude::*;
use super::helper::*;

pub struct CodeGenerator {
    builder: LLVMBuilderRef,
    context: LLVMContextRef,
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        unsafe {
            CodeGenerator {
                builder: llvm::core::LLVMCreateBuilder(),
                context: llvm::core::LLVMContextCreate(),
            }
        }
    }

    pub fn position_builder_at_end(&self, block: LLVMBasicBlockRef) {
        unsafe {
            llvm::core::LLVMPositionBuilderAtEnd(self.builder, block);
        }
    }

    pub fn build_alloca(&self, llvm_type: LLVMTypeRef, name: &str) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildAlloca(self.builder, llvm_type, string_cast(name).as_ptr()) }
    }

    pub fn build_load(&self, source: LLVMValueRef, dest_name: &str) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildLoad(self.builder, source, string_cast(dest_name).as_ptr()) }
    }

    pub fn build_store(&self, source: LLVMValueRef, dest: LLVMValueRef) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildStore(self.builder, source, dest) }
    }

    pub fn build_ret(&self, val: LLVMValueRef) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildRet(self.builder, val) }
    }

    pub fn build_add(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildAdd(self.builder, lhs, rhs, string_cast(name).as_ptr()) }
    }
    pub fn build_sub(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildSub(self.builder, lhs, rhs, string_cast(name).as_ptr()) }
    }
    pub fn build_mul(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildMul(self.builder, lhs, rhs, string_cast(name).as_ptr()) }
    }
    pub fn build_fdiv(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildFDiv(self.builder, lhs, rhs, string_cast(name).as_ptr()) }
    }

    pub fn dispose(&self) {
        unsafe {
            llvm::core::LLVMDisposeBuilder(self.builder);
            llvm::core::LLVMContextDispose(self.context);
        }
    }

    pub fn build_call(
        &self,
        func: LLVMValueRef,
        mut args: Vec<LLVMValueRef>,
        name: &str,
    ) -> LLVMValueRef {
        unsafe {
            llvm::core::LLVMBuildCall(
                self.builder,
                func,
                args.as_mut_ptr(),
                args.len() as u32,
                string_cast(name).as_ptr(),
            )
        }
    }
}
