mod ast;
mod compiler;
mod js_backend;
mod parser;
mod symbol_table;
mod type_checker;

pub use compiler::{check, compile, CompilerOutput, Errors};
pub use parser::parse;
