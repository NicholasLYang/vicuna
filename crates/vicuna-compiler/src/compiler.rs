use crate::ast::{build_ast, Program};
use crate::js_backend::JsBackend;
use crate::parser::parse;
use crate::type_checker::{TypeChecker, TypeError};
use anyhow::Result;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct CompilerOutput {
    pub js: String,
    pub ast: Program,
    pub type_errors: Vec<TypeError>,
}

pub fn compile(source: &str) -> Result<CompilerOutput> {
    let cst = parse(source)?;
    let program = build_ast(source, cst)?;

    let type_checker = TypeChecker::new();
    let type_errors = type_checker.check(&program);

    let mut output = Vec::new();
    let mut js_backend = JsBackend::new(&mut output);
    js_backend.emit_program(&program)?;
    let js = String::from_utf8(output)?;
    Ok(CompilerOutput {
        js,
        type_errors,
        ast: program,
    })
}
