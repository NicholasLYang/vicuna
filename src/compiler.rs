use crate::js_backend::JsBackend;
use anyhow::{anyhow, Result};
use serde::Serialize;
use tree_sitter_c2rust::{Language, Parser};
use crate::ast::ASTBuilder;
use crate::type_checker::{TypeChecker, TypeError};

extern "C" {
    fn tree_sitter_vicuna() -> Language;
}

#[derive(Debug, Clone, Serialize)]
pub struct CompilerOutput {
    js: String,
    type_errors: Vec<TypeError>
}

pub fn compile(source: &str) -> Result<CompilerOutput> {
    let mut parser = Parser::new();
    parser
        .set_language(unsafe { tree_sitter_vicuna() })
        .unwrap();

    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| anyhow!("Unable to parse code"))?;
    let mut ast_builder = ASTBuilder::new(&source, &tree)?;
    let program = ast_builder.build_ast()?;

    let mut type_checker = TypeChecker::new();
    let type_errors = type_checker.check(&program);

    let mut output = Vec::new();
    let mut js_backend = JsBackend::new(&mut output);
    js_backend.emit_program(&program)?;
    let js = String::from_utf8(output)?;
    Ok(CompilerOutput {
        js,
        type_errors,
    })
}
