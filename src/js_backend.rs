use crate::ast::UnaryOp;
use crate::{BinaryOp, Expr, Stmt, Value};
use anyhow::Result;
use std::io::Write;

pub struct JsBackend<T: Write> {
    output: T,
}

impl<T: Write> JsBackend<T> {
    pub fn new(output: T) -> JsBackend<T> {
        Self { output }
    }

    pub fn emit_program(&mut self, program: &[Stmt]) -> Result<()> {
        for stmt in program {
            self.emit_stmt(stmt)?;
        }

        Ok(())
    }

    fn emit_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Let(name, rhs) => {
                write!(self.output, "let {} = ", name)?;
                self.emit_expr(rhs)?;
                self.output.write_all(b";\n")?;
            }
            Stmt::LetIf {
                name,
                condition,
                then_block,
                else_block,
            } => {
                writeln!(self.output, "let {};", name)?;
                write!(self.output, "if (")?;
                self.emit_expr(condition)?;
                writeln!(self.output, ") {{")?;
                for stmt in &then_block.stmts {
                    self.emit_stmt(stmt)?;
                }
                if let Some(end_expr) = &then_block.end_expr {
                    write!(self.output, "{} = ", name)?;
                    self.emit_expr(end_expr)?;
                    writeln!(self.output, ";")?;
                }
                writeln!(self.output, "}} else {{")?;
                for stmt in &else_block.stmts {
                    self.emit_stmt(&stmt)?;
                }
                if let Some(end_expr) = &else_block.end_expr {
                    write!(self.output, "{} = ", name)?;
                    self.emit_expr(&end_expr)?;
                    writeln!(self.output, ";")?;
                }
                writeln!(self.output, "}}")?;
            }
            Stmt::Expr(expr) => {
                self.emit_expr(expr)?;
                self.output.write_all(b";\n")?;
            }
        }
        Ok(())
    }

    fn emit_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Value(value) => {
                self.emit_value(value)?;
            }
            Expr::Variable(name) => {
                // TODO: Figure out better way of mapping special names
                if name == "print" {
                    self.output.write_all(b"console.log")?;
                } else {
                    self.output.write_all(name.as_bytes())?;
                }
            }
            Expr::Call { callee, calls } => {
                self.emit_expr(callee)?;
                for call in calls {
                    self.output.write_all(b"(")?;
                    let arity = call.len();
                    for (i, arg) in call.iter().enumerate() {
                        self.emit_expr(arg)?;
                        if i < arity - 1 {
                            self.output.write_all(b", ")?;
                        }
                    }
                    self.output.write_all(b")")?;
                }
            }
            Expr::Binary(op, lhs, rhs) => {
                let op_str = match op {
                    BinaryOp::Add => b"+",
                    BinaryOp::Subtract => b"-",
                    BinaryOp::Divide => b"/",
                    BinaryOp::Multiply => b"*",
                };

                self.emit_expr(lhs)?;
                self.output.write_all(op_str)?;
                self.emit_expr(rhs)?;
            }
            Expr::Unary(op, rhs) => {
                let op_str = match op {
                    UnaryOp::Negate => b'-',
                    UnaryOp::Not => b'!',
                };
                self.output.write_all(&[op_str])?;
                self.emit_expr(rhs)?;
            }
        }

        Ok(())
    }

    fn emit_value(&mut self, value: &Value) -> Result<()> {
        match value {
            Value::I32(i) => {
                write!(self.output, "({}|0)", i)?;
            }
            Value::F32(f) => {
                write!(self.output, "{}", f)?;
            }
            Value::Bool(bool) => {
                write!(self.output, "{}", if *bool { "true" } else { "false" })?;
            }
            Value::String(s) => {
                write!(self.output, "\"{}\"", s)?;
            }
        }

        Ok(())
    }
}
