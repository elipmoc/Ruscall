extern crate llvm_sys as llvm;

use self::llvm::analysis::*;
use self::llvm::core::*;
use self::llvm::prelude::*;
use super::helper::*;
use super::types::{TargetDataRef, TargetTriple};
use super::function::Function;
use std::os::raw::c_char;

#[derive(Clone)]
pub struct Module {
    pub llvm_module: LLVMModuleRef,
}

impl Module {
    pub fn new(m_name: &str) -> Module {
        unsafe {
            let module = LLVMModuleCreateWithName(string_cast(m_name).as_ptr());
            Module {
                llvm_module: module,
            }
        }
    }

    pub fn verify_module(&self) -> Option<String> {
        let mut error: *mut c_char = 0 as *mut c_char;
        let ok = unsafe {
            LLVMVerifyModule(
                self.llvm_module,
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
                &mut error,
            )
        };
        if ok == 0 {
            Option::None
        } else {
            Option::Some(ram_to_string(error))
        }
    }

    pub fn get_named_function(&self, f_name: &str) -> Function {
        let llvm_func = unsafe { LLVMGetNamedFunction(self.llvm_module, string_cast(f_name).as_ptr()) };
        Function { llvm_function: llvm_func }
    }

    pub fn dump_module(&self) {
        unsafe { LLVMDumpModule(self.llvm_module) }
    }

    pub fn dispose_module(self) {
        unsafe {
            LLVMDisposeModule(self.llvm_module);
        }
    }
    pub fn set_data_layout(&self, data_layout: TargetDataRef) {
        unsafe { LLVMSetDataLayout(self.llvm_module, data_layout as *const c_char) }
    }

    pub fn set_target_triple(&self, target_triple: TargetTriple) {
        unsafe { LLVMSetTarget(self.llvm_module, target_triple) }
    }

    pub fn get_memory_buffer_ref(&self) -> LLVMMemoryBufferRef {
        unsafe { llvm::bit_writer::LLVMWriteBitcodeToMemoryBuffer(self.llvm_module) }
    }

    pub fn write_bitcode_to_file(&self, path: &str) -> i32 {
        unsafe {
            llvm::bit_writer::LLVMWriteBitcodeToFile(self.llvm_module, string_cast(path).as_ptr())
        }
    }
}
