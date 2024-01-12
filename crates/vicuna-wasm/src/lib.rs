use miette::Report;
use std::collections::HashMap;
use std::path::PathBuf;
use vicuna_compiler::{compile, parse, CompilerOutput};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct WasmOutput {
    js: Option<HashMap<PathBuf, String>>,
    ast: Option<HashMap<PathBuf, String>>,
    errors: Vec<String>,
}

#[wasm_bindgen]
impl WasmOutput {
    #[wasm_bindgen(getter)]
    pub fn js(&self) -> Option<HashMap<PathBuf, String>> {
        self.js.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn ast(&self) -> Option<HashMap<PathBuf, String>> {
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
    let mut errors = vec![];
    let (program, parse_errors) = parse(source);

    for error in parse_errors {
        errors.push(error.to_string());
    }

    let Some(program) = program else {
        return WasmOutput {
            js: None,
            ast: None,
            errors,
        };
    };

    match compile(source) {
        Ok(CompilerOutput {
            js,
            ast,
            diagnostics,
        }) => {
            let errors = errors
                .into_iter()
                .chain(diagnostics.into_iter().map(|d| d.to_string()))
                .collect();

            WasmOutput {
                js: js,
                ast: ast.map(|ast| format!("{:#?}", ast)),
                errors,
            }
        }
        Err(err) => WasmOutput {
            js: None,
            ast: Some(format!("{:#?}", program)),
            errors: vec![err.to_string()],
        },
    }
}
