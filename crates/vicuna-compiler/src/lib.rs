mod ast;
mod compiler;
mod js_backend;
mod type_checker;
mod wasm;

pub use compiler::compile;
