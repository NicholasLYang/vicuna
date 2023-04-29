use crate::ast::Program;
use crate::js_backend::JsBackend;
use crate::parser::parse;
use crate::type_checker::{TypeChecker, TypeError};
use anyhow::Result;
use chumsky::error::Simple;
use tracing::debug;

#[derive(Debug, Clone)]
pub struct CompilerOutput {
    pub js: String,
    pub ast: Program,
    pub errors: Errors,
}

#[derive(Debug, Clone)]
pub struct CheckOutput {
    pub ast: Program,
    pub errors: Errors,
}

#[derive(Debug, Clone)]
pub struct Errors {
    pub parse_errors: Vec<Simple<char>>,
    pub type_errors: Vec<TypeError>,
}

pub fn check(source: &str) -> Result<CheckOutput> {
    let (program, parse_errors) = parse(source);

    let program = program.ok_or_else(|| anyhow::anyhow!("Fatal parse error"))?;
    debug!("AST: {:#?}", program);

    let type_checker = TypeChecker::new();
    let type_errors = type_checker.check(&program);

    Ok(CheckOutput {
        ast: program,
        errors: Errors {
            parse_errors,
            type_errors,
        },
    })
}

pub fn compile(source: &str) -> Result<CompilerOutput> {
    let (program, parse_errors) = parse(source);

    let program = program.ok_or_else(|| anyhow::anyhow!("Fatal parse error"))?;
    debug!("AST: {:#?}", program);

    let type_checker = TypeChecker::new();
    let type_errors = type_checker.check(&program);

    let mut output = Vec::new();
    let mut js_backend = JsBackend::new(&mut output);
    js_backend.emit_program(&program)?;
    let js = String::from_utf8(output)?;
    Ok(CompilerOutput {
        js,
        ast: program,
        errors: Errors {
            parse_errors,
            type_errors,
        },
    })
}
