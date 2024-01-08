use crate::ast::Program;
use crate::js_backend::JsBackend;
use crate::parser::{parse, ParseError};
use crate::type_checker::{TypeChecker, TypeError};
use anyhow::Result;
use tracing::debug;

#[derive(Debug, Clone)]
pub struct CompilerOutput {
    pub js: Option<String>,
    pub ast: Option<Program>,
    pub errors: Errors,
}

#[derive(Debug, Clone)]
pub struct Errors {
    pub parse_errors: Vec<ParseError>,
    pub type_errors: Vec<TypeError>,
}

pub fn check(source: &str) -> Errors {
    let (program, parse_errors) = parse(source);

    let Some(program) = program else {
        return Errors {
            parse_errors,
            type_errors: Vec::new(),
        };
    };
    debug!("AST: {:?}", program);

    let type_checker = TypeChecker::new();
    let type_errors = type_checker.check(&program);

    Errors {
        parse_errors,
        type_errors,
    }
}

pub fn compile(source: &str) -> Result<CompilerOutput> {
    let (program, parse_errors) = parse(source);

    let Some(program) = program else {
        return Ok(CompilerOutput {
            js: None,
            ast: None,
            errors: Errors {
                parse_errors,
                type_errors: Vec::new(),
            },
        });
    };
    debug!("AST: {:#?}", program);

    let type_checker = TypeChecker::new();
    let type_errors = type_checker.check(&program);

    let mut output = Vec::new();
    let mut js_backend = JsBackend::new(&mut output);
    js_backend.emit_program(&program)?;
    let js = String::from_utf8(output)?;
    Ok(CompilerOutput {
        js: Some(js),
        ast: Some(program),
        errors: Errors {
            parse_errors,
            type_errors,
        },
    })
}
