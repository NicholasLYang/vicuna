use serde::{Deserialize, Serialize};
use serde_wasm_bindgen::Error;
use std::collections::HashMap;
use std::path::PathBuf;
use vicuna_compiler::{compile, parse, CompilerOutput, Program};
use wasm_bindgen::prelude::*;

#[derive(Debug, Serialize, Deserialize)]
pub struct WasmOutput {
    js: Option<HashMap<PathBuf, String>>,
    ast: Option<HashMap<PathBuf, Program>>,
    errors: Vec<String>,
}

#[wasm_bindgen(start)]
fn init() {
    console_error_panic_hook::set_once();
    tracing_wasm::set_as_global_default();
}

#[wasm_bindgen]
pub fn run_compiler(source: &str) -> Result<JsValue, Error> {
    let mut errors = vec![];
    let (program, parse_errors) = parse(source);

    for error in parse_errors {
        errors.push(error.to_string());
    }

    let Some(program) = program else {
        return serde_wasm_bindgen::to_value(&WasmOutput {
            js: None,
            ast: None,
            errors,
        });
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
                js: js.map(|s| serde_wasm_bindgen::to_value(&s).unwrap()),
                ast: ast.map(|ast| serde_wasm_bindgen::to_value(&ast).unwrap()),
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
