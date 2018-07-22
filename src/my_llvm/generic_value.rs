extern crate llvm_sys as llvm;

pub fn generic_value_to_int(
    g_value: llvm::execution_engine::LLVMGenericValueRef,
    is_signed: bool,
) -> u64 {
    unsafe { llvm::execution_engine::LLVMGenericValueToInt(g_value, if is_signed { 1 } else { 0 }) }
}
