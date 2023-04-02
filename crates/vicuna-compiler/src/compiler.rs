use crate::ast::parse;
use crate::js_backend::JsBackend;
use crate::type_checker::{TypeChecker, TypeError};
use anyhow::Result;
use serde::Serialize;
use tracing::debug;
use tracing_subscriber;

#[derive(Debug, Clone, Serialize)]
pub struct CompilerOutput {
    pub js: String,
    pub type_errors: Vec<TypeError>,
}

pub fn compile(source: &str) -> Result<CompilerOutput> {
    tracing_subscriber::fmt::init();

    let program = parse(source)?;
    debug!("PROGRAM: {:#?}", program);

    let type_checker = TypeChecker::new();
    let type_errors = type_checker.check(&program);

    let mut output = Vec::new();
    let mut js_backend = JsBackend::new(&mut output);
    js_backend.emit_program(&program)?;
    let js = String::from_utf8(output)?;
    Ok(CompilerOutput { js, type_errors })
}
