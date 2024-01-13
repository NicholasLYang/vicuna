use std::path::Path;
use vicuna_compiler::{compile_code, parse, CompilerOutput};
use wasm_bindgen::prelude::*;

#[derive(Debug)]
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

    match compile_code(source) {
        Ok(CompilerOutput {
            js,
            ast,
            diagnostics,
        }) => {
            let errors = errors
                .into_iter()
                .chain(diagnostics.into_iter().map(|d| d.to_string()))
                .collect();

            let js = js.map(|mut js| js.remove(Path::new("main.vc")).unwrap());
            let ast =
                ast.map(|mut ast| format!("{:#?}", ast.remove(Path::new("main.vc")).unwrap()));

            WasmOutput { js, ast, errors }
        }
        Err(err) => WasmOutput {
            js: None,
            ast: Some(format!("{:#?}", program)),
            errors: vec![err.to_string()],
        },
    }
}
