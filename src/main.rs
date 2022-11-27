use crate::ast::{BinaryOp, Expr, ExpressionBlock, Stmt, Value};
use crate::js_backend::JsBackend;
use anyhow::Result;

mod ast;
mod compiler;
mod js_backend;
mod server;

fn main() -> Result<()> {
    server::run_server()
}
