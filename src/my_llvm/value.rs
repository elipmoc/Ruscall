extern crate llvm_sys as llvm;

pub fn const_int(
    llvm_type: *mut llvm::LLVMType,
    val: u64,
    sign_flag: bool,
) -> *mut llvm::LLVMValue {
    unsafe { llvm::core::LLVMConstInt(llvm_type, val, if sign_flag { 1 } else { 0 }) }
}

pub fn const_struct(
    llvm_type: *mut llvm::LLVMType,
    mut vals: Vec<LLVMValueRef>, ) -> *mut llvm::LLVMValue
{
    unsafe {
        llvm::core::LLVMConstNamedStruct(
            llvm_type,
            vals.as_mut_ptr(),
            vals.len() as u32,
        )
    }
}

use self::llvm::prelude::LLVMValueRef;
pub use self::llvm::LLVMLinkage;

pub fn set_linkage(global_value: LLVMValueRef, linkage: LLVMLinkage) {
    unsafe { llvm::core::LLVMSetLinkage(global_value, linkage) }
}

pub fn type_of(val: LLVMValueRef) -> *mut llvm::LLVMType {
    unsafe {
        llvm::core::LLVMTypeOf(val)
    }
}

pub fn dump_val(val:LLVMValueRef){
    unsafe {
        llvm::core::LLVMDumpValue(val);
    }
}

pub fn dump_ty(ty:*mut llvm::LLVMType){
    unsafe {
        llvm::core::LLVMDumpType(ty)
    }
}
