use self::llvm::core::*;
use self::llvm::prelude::*;
use self::llvm::transforms::scalar::LLVMAddPromoteMemoryToRegisterPass;
extern crate llvm_sys as llvm;

pub struct FunctionPassManager {
    llvm_pass_namager: LLVMPassManagerRef,
}

impl FunctionPassManager {
    pub fn new(module: &super::module::Module) -> FunctionPassManager {
        unsafe {
            let pass_ref = LLVMCreateFunctionPassManagerForModule(module.llvm_module);
            FunctionPassManager {
                llvm_pass_namager: pass_ref,
            }
        }
    }
    pub fn dispose(self) {
        unsafe {
            LLVMDisposePassManager(self.llvm_pass_namager);
        }
    }

    pub fn init(&self) -> bool {
        unsafe {
            let llvm_bool = LLVMInitializeFunctionPassManager(self.llvm_pass_namager);
            if llvm_bool == 0 {
                true
            } else {
                false
            }
        }
    }

    pub fn run(&self, value: LLVMValueRef) -> bool {
        unsafe {
            let llvm_bool = LLVMRunFunctionPassManager(self.llvm_pass_namager, value);
            if llvm_bool == 0 {
                true
            } else {
                false
            }
        }
    }
    pub fn add_promote_memory_to_register_pass(&self) {
        unsafe { LLVMAddPromoteMemoryToRegisterPass(self.llvm_pass_namager) }
    }
}
