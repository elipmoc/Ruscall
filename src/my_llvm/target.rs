use std::os::raw::c_char;
extern crate llvm_sys as llvm;
use self::llvm::target_machine::*;
pub use self::llvm::target_machine::{
    LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMRelocMode,
};
use super::helper::*;

use super::types::{TargetDataRef, TargetTriple};

pub fn init_llvm_native_target() {
    unsafe {
        if llvm::target::LLVM_InitializeNativeTarget() != 0 {
            panic!("Could not initialise target");
        }
        if llvm::target::LLVM_InitializeNativeAsmPrinter() != 0 {
            panic!("Could not initialise ASM Printer");
        }
        if llvm::target::LLVM_InitializeNativeAsmParser() != 0 {
            panic!("Could not initialise ASM Parser");
        }
    }
}

pub fn init_llvm_all_target() {
    unsafe {
        llvm::target::LLVM_InitializeAllTargetInfos();
        llvm::target::LLVM_InitializeAllTargets();
        llvm::target::LLVM_InitializeAllTargetMCs();
        llvm::target::LLVM_InitializeAllAsmParsers();
        llvm::target::LLVM_InitializeAllAsmPrinters();
    }
}

pub struct TargetMachine {
    llvm_target_machine: LLVMTargetMachineRef,
    pub target_triple: TargetTriple,
}

impl TargetMachine {
    pub fn create(
        cpu: &str,
        features: &str,
        level: LLVMCodeGenOptLevel,
        reloc: LLVMRelocMode,
        code_model: LLVMCodeModel,
    ) -> Result<TargetMachine, String> {
        unsafe {
            let target_triple = LLVMGetDefaultTargetTriple();
            let target = try!{get_target_from_triple(target_triple)};
            let llvm_target_machine = LLVMCreateTargetMachine(
                target,
                target_triple,
                string_cast(cpu).as_ptr(),
                string_cast(features).as_ptr(),
                level,
                reloc,
                code_model,
            );

            Result::Ok(TargetMachine {
                llvm_target_machine: llvm_target_machine,
                target_triple: target_triple,
            })
        }
    }

    pub fn create_data_layout(&self) -> TargetDataRef {
        unsafe { LLVMCreateTargetDataLayout(self.llvm_target_machine) }
    }

    pub fn dispose(self) {
        unsafe { LLVMDisposeTargetMachine(self.llvm_target_machine) }
    }

    //moduleを形式を指定してファイルの保存
    pub fn emit_to_file(
        &self,
        module: &super::module::Module,
        file_name: &str,
        file_type: LLVMCodeGenFileType,
    ) -> Option<String> {
        let mut error: *mut c_char = 0 as *mut c_char;
        unsafe {
            let ok = LLVMTargetMachineEmitToFile(
                self.llvm_target_machine,
                module.llvm_module,
                string_cast(file_name).into_raw(),
                file_type,
                &mut error,
            );
            if ok == 0 {
                Option::None
            } else {
                Option::Some(ram_to_string(error))
            }
        }
    }
}

fn get_target_from_triple(triple: *const c_char) -> Result<LLVMTargetRef, String> {
    let mut target: LLVMTargetRef = 0 as LLVMTargetRef;
    let mut error: *mut c_char = 0 as *mut c_char;
    unsafe {
        let ok = LLVMGetTargetFromTriple(triple, &mut target, &mut error);
        if ok == 0 {
            Result::Ok(target)
        } else {
            Result::Err(ram_to_string(error))
        }
    }
}
