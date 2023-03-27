pub use crate::compiler::compile;

mod ast;
mod compiler;
mod js_backend;
mod type_checker;
mod wasm;
