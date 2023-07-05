use crate::ast::{
    BinaryOp, Expr, ExprBlock, ExprFields, Function, ImportType, MatchBindings, PostFix, Program,
    Span, Stmt, TypeDeclaration, TypeFields, UnaryOp, Value,
};
use anyhow::Result;
use std::io::Write;
use std::mem;

// We need to keep track of whether we're emitting an expression block
// as the last expression of a function, in which case we want to add
// a return statement to the last expression, or whether we're emitting
// it as a variable binding, in which case we want to assign the result
// to the variable.
enum ExprBlockState {
    Return,
    Binding(String),
}

pub struct JsBackend<T: Write> {
    output: T,
    expression_block_state: Option<ExprBlockState>,
}

impl<T: Write> JsBackend<T> {
    pub fn new(output: T) -> JsBackend<T> {
        Self {
            output,
            expression_block_state: None,
        }
    }

    // Emits the standard preamble for Vicuna
    pub fn emit_preamble(&mut self) -> Result<()> {
        Ok(self.output.write_all(b"let __match__;\n")?)
    }

    pub fn emit_program(&mut self, program: &Program) -> Result<()> {
        self.emit_preamble()?;
        for stmt in &program.statements {
            self.emit_stmt(stmt)?;
        }

        Ok(())
    }

    fn emit_stmt(&mut self, stmt: &Span<Stmt>) -> Result<()> {
        match &stmt.0 {
            Stmt::Let(name, if_expr @ Span(Expr::If { .. } | Expr::Match { .. }, _)) => {
                write!(self.output, "let {};\n", name.0)?;
                let old_expression_block_state = mem::replace(
                    &mut self.expression_block_state,
                    Some(ExprBlockState::Binding(name.0.clone())),
                );

                self.emit_expr(if_expr)?;
                self.expression_block_state = old_expression_block_state;
            }
            Stmt::Let(name, rhs) => {
                write!(self.output, "let {} = ", name.0)?;
                self.emit_expr(rhs)?;
                self.output.write_all(b";\n")?;
            }
            Stmt::Expr(expr) => {
                self.emit_expr(expr)?;
                self.output.write_all(b";\n")?;
            }
            Stmt::Function(Function {
                name, params, body, ..
            }) => {
                write!(self.output, "function {}(", name.0)?;
                for (idx, (arg, _)) in params.iter().enumerate() {
                    self.output.write_all(arg.0.as_bytes())?;
                    if idx != params.len() - 1 {
                        self.output.write_all(b", ")?;
                    }
                }
                writeln!(self.output, ") {{")?;
                for stmt in &body.0.stmts {
                    self.emit_stmt(stmt)?;
                }

                let old_expression_block_state = mem::replace(
                    &mut self.expression_block_state,
                    Some(ExprBlockState::Return),
                );

                match body.0.end_expr.as_deref() {
                    Some(block_expr @ Span(Expr::If { .. } | Expr::Match { .. }, _)) => {
                        self.emit_expr(block_expr)?;
                    }
                    Some(end_expr) => {
                        self.output.write_all(b"return ")?;
                        self.emit_expr(end_expr)?;
                        self.output.write_all(b";\n")?;
                    }
                    _ => {}
                }

                self.expression_block_state = old_expression_block_state;

                writeln!(self.output, "}}")?;
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                write!(self.output, "if (")?;
                self.emit_expr(condition)?;
                writeln!(self.output, ") {{")?;
                for stmt in then_block {
                    self.emit_stmt(stmt)?;
                }

                writeln!(self.output, "}} else {{")?;
                for stmt in else_block {
                    self.emit_stmt(stmt)?;
                }
                writeln!(self.output, "}}")?;
            }
            Stmt::Return(expr) => {
                self.output.write_all(b"return ")?;
                if let Some(expr) = expr {
                    self.emit_expr(expr)?;
                }
                self.output.write_all(b";\n")?;
            }
            Stmt::Import {
                ty: Span(ImportType::External, _),
                default_import,
                named_imports,
                path,
            } => {
                self.output.write_all(b"import ")?;
                if let Some(default_import) = default_import {
                    self.output.write_all(default_import.0.as_bytes())?;
                }
                if !named_imports.is_empty() {
                    if default_import.is_some() {
                        self.output.write_all(b", ")?;
                    }

                    self.output.write_all(b"{ ")?;
                    for (idx, name) in named_imports.iter().enumerate() {
                        self.output.write_all(name.0.as_bytes())?;
                        if idx != named_imports.len() - 1 {
                            self.output.write_all(b", ")?;
                        }
                    }
                    self.output.write_all(b" }")?;
                }

                self.output.write_all(b" from \"")?;
                self.output.write_all(path.0.as_bytes())?;
                self.output.write_all(b"\";\n")?;
            }
            Stmt::Type(TypeDeclaration::Enum { variants, .. }) => {
                for (variant_name, variant_fields) in variants {
                    if let TypeFields::Tuple(fields) = variant_fields {
                        self.output.write_all(b"function ")?;
                        self.output.write_all(variant_name.0.as_bytes())?;
                        self.output.write_all(b"(")?;
                        for (idx, _) in fields.iter().enumerate() {
                            self.output.write_all(b"a")?;
                            self.output.write_all(idx.to_string().as_bytes())?;
                            if idx != fields.len() - 1 {
                                self.output.write_all(b", ")?;
                            }
                        }
                        self.output.write_all(b") {\n")?;
                        self.output.write_all(b"const out = [];\n")?;
                        for idx in 0..(fields.len()) {
                            self.output.write_all(b"out[")?;
                            self.output.write_all(idx.to_string().as_bytes())?;
                            self.output.write_all(b"] = ")?;
                            self.output.write_all(b"a")?;
                            self.output.write_all(idx.to_string().as_bytes())?;
                            self.output.write_all(b";\n")?;
                        }
                        self.output.write_all(b"out.type = \"")?;
                        self.output.write_all(variant_name.0.as_bytes())?;
                        self.output.write_all(b"\";\n")?;

                        self.output.write_all(b"return out;\n")?;
                        self.output.write_all(b"}\n")?;
                    }
                }
            }
            // TODO: Add TypeScript type generation
            Stmt::Type(_) => {}
            Stmt::Use { .. } => {}
            Stmt::Import { .. } => todo!("internal imports not implemented yet"),
        }
        Ok(())
    }

    fn emit_expr(&mut self, expr: &Span<Expr>) -> Result<()> {
        match &expr.0 {
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
            Expr::PostFix(callee, Span(PostFix::Args(args), _)) => {
                self.emit_expr(callee)?;
                self.output.write_all(b"(")?;
                let arity = args.len();
                for (i, arg) in args.iter().enumerate() {
                    self.emit_expr(arg)?;
                    if i < arity - 1 {
                        self.output.write_all(b", ")?;
                    }
                }
                self.output.write_all(b")")?;
            }
            Expr::PostFix(callee, Span(PostFix::Index(index), _)) => {
                self.emit_expr(callee)?;
                self.output.write_all(b"[")?;
                self.emit_expr(index)?;
                self.output.write_all(b"]")?;
            }
            Expr::PostFix(callee, Span(PostFix::Field(name), _)) => {
                self.emit_expr(callee)?;
                self.output.write_all(b".")?;
                self.output.write_all(name.0.as_bytes())?;
            }
            Expr::Binary(op, lhs, rhs) => {
                let op_str: &[u8] = match op.0 {
                    BinaryOp::Add => b"+",
                    BinaryOp::Subtract => b"-",
                    BinaryOp::Divide => b"/",
                    BinaryOp::Multiply => b"*",
                    BinaryOp::GreaterThan => b">",
                    BinaryOp::GreaterThanOrEqual => b">=",
                    BinaryOp::LessThan => b"<",
                    BinaryOp::LessThanOrEqual => b"<=",
                    BinaryOp::Equal => b"===",
                    BinaryOp::NotEqual => b"!==",
                };

                self.emit_expr(lhs)?;
                self.output.write_all(op_str)?;
                self.emit_expr(rhs)?;
            }
            Expr::Unary(op, rhs) => {
                let op_str = match op.0 {
                    UnaryOp::Negate => b'-',
                    UnaryOp::Not => b'!',
                };
                self.output.write_all(&[op_str])?;
                self.emit_expr(rhs)?;
            }
            Expr::Struct(name, fields) => {
                self.output.write_all(b"{")?;

                self.output.write_all(b" \"__type__\": \"")?;
                self.output.write_all(name.0.as_bytes())?;
                self.output.write_all(b"\", ")?;
                self.emit_expr_fields(fields)?;
                self.output.write_all(b"}")?;
            }
            Expr::Enum {
                variant_name,
                fields,
                ..
            } => {
                self.output.write_all(b"{")?;
                self.output.write_all(b" \"__type__\": \"")?;
                self.output.write_all(variant_name.0.as_bytes())?;
                self.output.write_all(b"\", ")?;
                self.emit_expr_fields(fields)?;
                self.output.write_all(b"}")?;
            }
            // We assume that the if expression is well placed, and therefore not
            // in the middle of an expression itself
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.output.write_all(b"if (")?;
                self.emit_expr(condition)?;
                self.output.write_all(b") {")?;
                self.emit_expression_block(then_block)?;
                self.output.write_all(b"} else\n")?;
                self.emit_expression_block(else_block)?;
                self.output.write_all(b"}")?;
            }
            Expr::Match { expr, cases } => {
                self.output.write_all(b"__match__ = ")?;
                self.emit_expr(expr)?;
                self.output.write_all(b";\n")?;

                self.output.write_all(b"switch (__match__.__type__) {\n")?;
                for (case, body) in cases {
                    self.output.write_all(b"case \"")?;
                    self.output.write_all(case.0.variant_name.0.as_bytes())?;
                    self.output.write_all(b"\": {\n")?;

                    if let Some(bindings) = &case.0.fields {
                        self.emit_match_bindings(bindings)?;
                    }

                    self.emit_expression_block(body)?;
                    self.output.write_all(b"break;\n")?;
                    self.output.write_all(b"}\n")?;
                }
                self.output.write_all(b"}")?;
            }
        }

        Ok(())
    }

    fn emit_expr_fields(&mut self, fields: &ExprFields) -> Result<()> {
        match fields {
            ExprFields::Tuple(fields) => {
                for (i, field) in fields.into_iter().enumerate() {
                    self.output.write_all(format!("{}: ", i).as_bytes())?;
                    self.emit_expr(field)?;
                    if i < fields.len() - 1 {
                        self.output.write_all(b", ")?;
                    }
                }
            }
            ExprFields::Named(fields) => {
                for (i, (name, field)) in fields.into_iter().enumerate() {
                    self.output.write_all(name.0.as_bytes())?;
                    self.output.write_all(b": ")?;
                    self.emit_expr(field)?;
                    if i < fields.len() - 1 {
                        self.output.write_all(b", ")?;
                    }
                }
            }
            ExprFields::Empty => {}
        }

        Ok(())
    }

    fn emit_match_bindings(&mut self, bindings: &MatchBindings) -> Result<()> {
        match bindings {
            MatchBindings::Tuple(fields) => {
                for (idx, field) in fields.into_iter().enumerate() {
                    self.output.write_all(b"const ")?;
                    self.output.write_all(field.0.as_bytes())?;
                    self.output.write_all(b" = __match__[")?;
                    self.output.write_all(idx.to_string().as_bytes())?;
                    self.output.write_all(b"];\n")?;
                }
            }
            MatchBindings::Named(fields) => {
                for (field, rename) in fields {
                    let name = rename.as_ref().unwrap_or(&field);

                    self.output.write_all(b"const ")?;
                    self.output.write_all(name.0.as_bytes())?;
                    self.output.write_all(b" = __match__.")?;
                    self.output.write_all(field.0.as_bytes())?;
                    self.output.write_all(b";\n")?;
                }
            }
        }

        Ok(())
    }

    fn emit_expression_block(&mut self, block: &Span<ExprBlock>) -> Result<()> {
        for stmt in &block.0.stmts {
            self.emit_stmt(stmt)?;
        }
        match block.0.end_expr.as_deref() {
            // If the end expression is an if expression, we don't bind or return,
            // and let the if expression determine that itself.
            Some(if_expr @ Span(Expr::If { .. }, _)) => {
                self.emit_expr(&if_expr)?;
            }
            Some(end_expr) => {
                let expr_block_state = self
                    .expression_block_state
                    .as_ref()
                    .expect("Expression block state should be set");
                match expr_block_state {
                    ExprBlockState::Return => {
                        self.output.write_all(b"return ")?;
                        self.emit_expr(end_expr)?;
                        self.output.write_all(b";\n")?;
                    }
                    ExprBlockState::Binding(var) => {
                        self.output.write_all(var.as_bytes())?;
                        self.output.write_all(b" = ")?;
                        self.emit_expr(end_expr)?;
                    }
                }
            }
            _ => {}
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
