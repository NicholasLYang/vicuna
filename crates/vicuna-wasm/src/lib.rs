use vicuna_compiler::{compile, CompilerOutput};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct WasmOutput {
    js: Option<String>,
    cst: Option<String>,
    ast: Option<String>,
    errors: Vec<String>,
}

#[wasm_bindgen]
impl WasmOutput {
    #[wasm_bindgen(getter)]
    pub fn js(&self) -> Option<String> {
        self.js.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn cst(&self) -> Option<String> {
        self.cst.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn ast(&self) -> Option<String> {
        self.ast.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn errors(&self) -> String {
        self.errors.join("\n")
    }
}

#[wasm_bindgen(start)]
fn init() {
    console_error_panic_hook::set_once();
    tracing_wasm::set_as_global_default();
}

#[wasm_bindgen]
pub fn run_compiler(source: &str) -> WasmOutput {
    match compile(source) {
        Ok(CompilerOutput {
            js,
            type_errors,
            ast,
        }) => {
            let errors = type_errors.into_iter().map(|e| e.to_string()).collect();
            WasmOutput {
                js: Some(js),
                cst: None,
                ast: Some(format!("{:#?}", ast)),
                errors,
            }
        }
        Err(err) => WasmOutput {
            js: None,
            cst: None,
            ast: None,
            errors: vec![err.to_string()],
        },
    }
}
