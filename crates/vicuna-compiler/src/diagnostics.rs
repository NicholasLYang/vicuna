//! Module for handling diagnostics. Diagnostics are different than errors
//! in that they're not necessarily fatal. The compiler is expected to
//! accumulate diagnostics and then print them out at the end of compilation.
//! Errors should be quite rare in the compiler, because they imply a total
//! stoppage of compilation.
use crate::parser::ParseDiagnostic;
use crate::resolver::ResolverDiagnostic;
use thiserror::Error;

#[derive(Debug, Error, miette::Diagnostic)]
pub enum Diagnostic {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Parse(ParseDiagnostic),
    #[error(transparent)]
    #[diagnostic(transparent)]
    Resolver(ResolverDiagnostic),
}
