mod ast;
mod compiler;
mod diagnostics;
mod js_backend;
mod parser;
mod resolver;
mod symbol_table;
mod type_checker;

pub use compiler::{check, compile, CompilerOutput};
pub use diagnostics::Diagnostic;
pub use parser::parse;
