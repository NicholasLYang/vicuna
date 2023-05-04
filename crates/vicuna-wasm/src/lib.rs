use miette::Report;
use vicuna_compiler::{compile, parse, CompilerOutput, Errors};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct WasmOutput {
    js: Option<String>,
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
            errors:
                Errors {
                    parse_errors,
                    type_errors,
                },
            ast,
        }) => {
            let errors = parse_errors
                .into_iter()
                .map(|e| e.to_string())
                .chain(
                    type_errors
                        .into_iter()
                        .map(|e| format!("{:?}", Report::new(e))),
                )
                .collect();

            WasmOutput {
                js: Some(js),
                ast: Some(format!("{:#?}", ast)),
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
