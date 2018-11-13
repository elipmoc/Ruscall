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

    pub fn build_struct_gep(&self, pointer: LLVMValueRef, idx: u32, name: &str) -> LLVMValueRef {
        unsafe {
            llvm::core::LLVMBuildStructGEP(
                self.builder,
                pointer,
                idx,
                string_cast(name).as_ptr(),
            )
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
    pub fn build_br(&self, dest: LLVMBasicBlockRef) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildBr(self.builder, dest) }
    }
    pub fn build_cond_br(&self, cond: LLVMValueRef, t: LLVMBasicBlockRef, e: LLVMBasicBlockRef) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildCondBr(self.builder, cond, t, e) }
    }
    pub fn build_phi(&self, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
        unsafe { llvm::core::LLVMBuildPhi(self.builder, ty, string_cast(name).as_ptr()) }
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
    pub fn build_ieq(&self, lhs: LLVMValueRef, rhs: LLVMValueRef, name: &str) -> LLVMValueRef {
        unsafe {
            llvm::core::LLVMBuildICmp(
                self.builder,
                llvm::LLVMIntPredicate::LLVMIntEQ,
                lhs, rhs,
                string_cast(name).as_ptr())
        }
    }
    //浮動小数点数から符号付整数へのキャスト
    pub fn build_fp_to_si(
        &self,
        val: LLVMValueRef,
        dest_ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        unsafe {
            llvm::core::LLVMBuildFPToSI(self.builder, val, dest_ty, string_cast(name).as_ptr())
        }
    }
    //符号付整数から浮動小数点数へのキャスト
    pub fn build_si_to_fp(
        &self,
        val: LLVMValueRef,
        dest_ty: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        unsafe {
            llvm::core::LLVMBuildSIToFP(self.builder, val, dest_ty, string_cast(name).as_ptr())
        }
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
    pub fn get_insert_block(&self) -> LLVMBasicBlockRef {
        unsafe {
            llvm::core::LLVMGetInsertBlock(self.builder)
        }
    }
}
