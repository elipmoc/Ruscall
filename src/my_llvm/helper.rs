use std::ffi::CString;

pub fn string_cast(s: &str) -> CString {
    CString::new(s).unwrap()
}

pub fn ram_to_string(raw: *mut i8) -> String {
    unsafe { CString::from_raw(raw).into_string().unwrap() }
}
