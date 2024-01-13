use crate::ast::Program;
use crate::diagnostics::Diagnostic;
use crate::js_backend::JsBackend;
use crate::parse;
use crate::resolver::ResolverBuilder;
use crate::type_checker::TypeChecker;
use anyhow::Result;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct CompilerOutput {
    /// The JavaScript output for each file
    pub js: Option<HashMap<PathBuf, String>>,
    /// The AST for each file
    pub ast: Option<HashMap<PathBuf, Program>>,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn check(source_path: &Path) -> Vec<Diagnostic> {
    let resolver_builder = ResolverBuilder::new(source_path.to_path_buf());
    let (mut resolver, mut diagnostics) = resolver_builder.build();

    let files = match resolver.traverse() {
        Ok(files) => files,
        Err(diagnostic) => {
            diagnostics.push(Diagnostic::Resolver(diagnostic));

            return diagnostics;
        }
    };

    for file in files {
        let Some(ast) = resolver.remove_ast(file) else {
            continue;
        };
        let type_checker = TypeChecker::new();
        let type_errors = type_checker.check(&ast, file);
        diagnostics.extend(type_errors.into_iter().map(Diagnostic::Type));
    }

    diagnostics
}
/// Compiles a snippet of code. Mostly used for playground
pub fn compile_code(code: &str) -> Result<CompilerOutput> {
    let (program, parse_errors) = parse(code);
    let diagnostics = parse_errors
        .into_iter()
        .map(Diagnostic::Parse)
        .collect::<Vec<_>>();

    let program = match program {
        Some(program) => program,
        None => {
            return Ok(CompilerOutput {
                js: None,
                ast: None,
                diagnostics,
            });
        }
    };

    let mut js = HashMap::new();
    let mut asts = HashMap::new();
    let mut output = Vec::new();
    let mut js_backend = JsBackend::new(&mut output);
    js_backend.emit_program(&program)?;
    js.insert(PathBuf::from("main.vc"), String::from_utf8(output)?);
    asts.insert(PathBuf::from("main.vc"), program);

    Ok(CompilerOutput {
        js: Some(js),
        ast: Some(asts),
        diagnostics,
    })
}

pub fn compile(source_path: &Path) -> Result<CompilerOutput> {
    let resolver_builder = ResolverBuilder::new(source_path.to_path_buf());
    let (mut resolver, mut diagnostics) = resolver_builder.build();

    let files = match resolver.traverse() {
        Ok(files) => files,
        Err(diagnostic) => {
            diagnostics.push(Diagnostic::Resolver(diagnostic));

            return Ok(CompilerOutput {
                js: None,
                ast: None,
                diagnostics,
            });
        }
    };

    let mut js = HashMap::new();
    let mut asts = HashMap::new();
    for file in files {
        let Some(ast) = resolver.remove_ast(file) else {
            continue;
        };
        let type_checker = TypeChecker::new();
        let type_errors = type_checker.check(&ast);
        diagnostics.extend(type_errors.into_iter().map(Diagnostic::Type));
        let mut output = Vec::new();
        let mut js_backend = JsBackend::new(&mut output);
        js_backend.emit_program(&ast)?;
        let Some(path) = resolver.get_path(file) else {
            continue;
        };
        js.insert(path.clone(), String::from_utf8(output)?);
        asts.insert(path.clone(), ast);
    }

    Ok(CompilerOutput {
        js: Some(js),
        ast: Some(asts),
        diagnostics,
    })
}
