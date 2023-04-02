use vicuna_compiler::compile;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile_code(code: &str) -> String {
    match compile(code) {
        Ok(compiler_output) => serde_json::to_string(&compiler_output)
            .unwrap_or_else(|_| "Failed to serialize output".to_string()),
        Err(err) => err.to_string(),
    }
}
