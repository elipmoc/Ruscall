extern crate llvm_sys as llvm;
use self::llvm::execution_engine as llex;
use self::llvm::prelude::*;
use super::helper::*;
use std::os::raw::c_char;

pub fn linkin_interpreter() {
    unsafe {
        llex::LLVMLinkInInterpreter();
    }
}

pub struct ExecutionEngine {
    engine: llex::LLVMExecutionEngineRef,
}

impl ExecutionEngine {
    pub fn new() -> ExecutionEngine {
        let engine = 0 as llex::LLVMExecutionEngineRef;
        ExecutionEngine { engine: engine }
    }

    pub fn create_interpreter_for_module(
        &mut self,
        module: &super::module::Module,
    ) -> Option<String> {
        let mut error = 0 as *mut c_char;
        let ok = unsafe {
            llex::LLVMCreateInterpreterForModule(&mut self.engine, module.llvm_module, &mut error)
        };
        if ok == 0 {
            Option::None
        } else {
            Option::Some(ram_to_string(error))
        }
    }

    pub fn run_function(
        &self,
        function: LLVMValueRef,
        mut params: Vec<llex::LLVMGenericValueRef>,
    ) -> llex::LLVMGenericValueRef {
        unsafe {
            llex::LLVMRunFunction(
                self.engine,
                function,
                params.len() as u32,
                params.as_mut_ptr(),
            )
        }
    }
}
