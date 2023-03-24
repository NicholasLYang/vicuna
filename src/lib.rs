pub use crate::compiler::compile;

mod ast;
mod compiler;
mod js_backend;
mod type_checker;

#[no_mangle]
extern "C" fn allocate_buffer(size: usize) -> *const u8 {
    let buffer = vec![0_u8; size];
    let ptr = buffer.as_ptr();
    std::mem::forget(buffer);

    ptr
}

#[no_mangle]
extern "C" fn deallocate_buffer(ptr: *mut u8, size: usize) {
    let buffer = unsafe {
        Vec::from_raw_parts(ptr, size, size)
    };
    drop(buffer);
}

#[no_mangle]
extern "C" fn compile_code(ptr: *mut u8, length: usize) -> *const u8 {
    let code = unsafe {
        let bytes: Vec<u8> = Vec::from_raw_parts(ptr, length, length);
        String::from_utf8(bytes).unwrap()
    };

    let compiler_output = compile(&code).unwrap_or_else(|err| err.to_string());
    let mut bytes = compiler_output.into_bytes();
    let mut output = bytes.len().to_le_bytes().to_vec();
    output.append(&mut bytes);
    let ptr = output.as_ptr();
    std::mem::forget(output);

    ptr
}
