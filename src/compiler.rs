use crate::ast::{BinaryOp, Expr, ExpressionBlock, Stmt, Value};
use crate::js_backend::JsBackend;
use anyhow::{anyhow, Result};
use tree_sitter_c2rust::{Language, Parser, Tree, TreeCursor};

extern "C" {
    fn tree_sitter_vicuna() -> Language;
}

// pub fn get_cst(source: &str) -> Option<Tree> {
//     let mut parser = Parser::new();
//     parser
//         .set_language(unsafe { tree_sitter_vicuna() })
//         .unwrap();
//
//     parser.parse(&source, None)
// }

pub fn compile(source: &str) -> Result<String> {
    let mut parser = Parser::new();
    parser
        .set_language(unsafe { tree_sitter_vicuna() })
        .unwrap();
    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| anyhow!("Unable to parse code"))?;
    let mut ast_builder = ASTBuilder::new(&source, &tree)?;
    let program = ast_builder.build_ast()?;
    let mut output = Vec::new();
    let mut js_backend = JsBackend::new(&mut output);
    js_backend.emit_program(&program)?;
    Ok(String::from_utf8(output)?)
}

/// Builds AST from tree-sitter CST
struct ASTBuilder<'a> {
    source: &'a [u8],
    cursor: TreeCursor<'a>,
}

impl<'a> ASTBuilder<'a> {
    fn new(source: &'a str, tree: &'a Tree) -> Result<ASTBuilder<'a>> {
        Ok(Self {
            source: source.as_bytes(),
            cursor: tree.walk(),
        })
    }

    // If cursor does not point to expected kind, error
    fn expect_kind(&mut self, expected_kind: &str) -> Result<()> {
        let kind = self.cursor.node().kind();
        if kind == expected_kind {
            Ok(())
        } else {
            Err(anyhow!(
                "Expected `{}`, received `{}`: {}",
                expected_kind,
                kind,
                self.cursor.node().utf8_text(self.source)?
            ))
        }
    }

    // If cursor does point to expected parent kind, move cursor to its children
    // If no children, we error. If we do not point to expected kind, we error.
    fn expect_and_consume_parent(&mut self, expected_parent_kind: &str) -> Result<()> {
        self.expect_kind(expected_parent_kind)?;
        let has_children = self.cursor.goto_first_child();

        if has_children {
            Ok(())
        } else {
            Err(anyhow!(
                "Expected `{}` to have child nodes",
                expected_parent_kind
            ))
        }
    }

    // If cursor does point to expected sibling kind, move cursor to its next sibling
    // If no more siblings, we error. If we do not point to expected kind, we error.
    fn expect_and_consume_sibling(&mut self, expected_sibling_kind: &str) -> Result<()> {
        self.expect_kind(expected_sibling_kind)?;
        let has_sibling = self.cursor.goto_next_sibling();

        if has_sibling {
            Ok(())
        } else {
            Err(anyhow!(
                "Expected `{}` to have sibling node",
                expected_sibling_kind
            ))
        }
    }

    fn build_ast(&mut self) -> Result<Vec<Stmt>> {
        let mut has_next_stmt = self.cursor.goto_first_child();
        let mut stmts = Vec::new();
        while has_next_stmt {
            stmts.push(self.build_stmt()?);
            // Skip ";"
            self.cursor.goto_next_sibling();
            // Skip "\n"
            self.cursor.goto_next_sibling();
            has_next_stmt = self.cursor.goto_next_sibling();
        }

        Ok(stmts)
    }

    fn build_stmt(&mut self) -> Result<Stmt> {
        self.expect_and_consume_parent("statement")?;
        let node = self.cursor.node();
        let result = match node.kind() {
            "let_declaration" => {
                self.cursor.goto_first_child();
                // First child is "let"
                self.expect_and_consume_sibling("let")?;
                // Next is variable name
                let var_name = self.cursor.node().utf8_text(self.source)?;
                // Next is "="
                self.cursor.goto_next_sibling();
                // Next is expr
                self.cursor.goto_next_sibling();
                let rhs = self.build_expr()?;

                Ok(Stmt::Let(var_name.to_string(), rhs))
            }
            "let_if_declaration" => {
                self.cursor.goto_first_child();
                self.expect_and_consume_sibling("let")?;
                let name = self.cursor.node().utf8_text(self.source)?;
                self.cursor.goto_next_sibling();
                self.expect_and_consume_sibling("=")?;
                self.expect_and_consume_sibling("if")?;
                let condition = self.build_expr()?;
                self.cursor.goto_next_sibling();
                let then_block = self.build_expression_block()?;
                self.cursor.goto_next_sibling();
                self.expect_and_consume_sibling("else")?;
                let else_block = self.build_expression_block()?;

                Ok(Stmt::LetIf {
                    name: name.to_string(),
                    condition,
                    then_block,
                    else_block,
                })
            }
            "expression_statement" => {
                self.cursor.goto_first_child();
                let expr = self.build_expr()?;
                self.cursor.goto_parent();
                Ok(Stmt::Expr(expr))
            }
            unknown_kind => Err(anyhow!(
                "Kind `{}` is not yet handled in statement",
                unknown_kind
            )),
        };

        self.cursor.goto_parent();
        result
    }

    fn build_expression_block(&mut self) -> Result<ExpressionBlock> {
        self.expect_and_consume_parent("expression_block")?;
        self.expect_and_consume_sibling("{")?;
        let mut stmts = Vec::new();
        let block = loop {
            match self.cursor.node().kind() {
                "statement" => {
                    stmts.push(self.build_stmt()?);
                    self.expect_and_consume_sibling(";")?;
                }
                "expression" => {
                    let end_expr = self.build_expr()?;
                    break ExpressionBlock {
                        stmts,
                        end_expr: Some(end_expr),
                    };
                }
                "}" => {
                    break ExpressionBlock {
                        stmts,
                        end_expr: None,
                    }
                }
                unknown_kind => {
                    return Err(anyhow!(
                        "Kind `{}` is not yet handled in expression block",
                        unknown_kind
                    ))
                }
            }
        };

        self.cursor.goto_parent();
        Ok(block)
    }

    fn build_expr(&mut self) -> Result<Expr> {
        self.expect_and_consume_parent("expression")?;
        let node = self.cursor.node();
        let result = match node.kind() {
            "primary_expression" => {
                self.cursor.goto_first_child();
                let node = self.cursor.node();
                let result = match node.kind() {
                    "value" => Ok(Expr::Value(self.build_value()?)),
                    "identifier" => Ok(Expr::Variable(node.utf8_text(self.source)?.to_string())),
                    unknown_kind => Err(anyhow!(
                        "Kind `{}` is not handled yet in primary_expression.",
                        unknown_kind
                    )),
                };

                self.cursor.goto_parent();
                result
            }
            "binary_expression" => {
                self.cursor.goto_first_child();
                let lhs = self.build_expr()?;

                if !self.cursor.goto_next_sibling() {
                    return Err(anyhow!("Missing operator and rhs in binary expression"));
                }

                let op_node = self.cursor.node();
                let op = match op_node.utf8_text(self.source)? {
                    "+" => BinaryOp::Add,
                    "*" => BinaryOp::Multiply,
                    "/" => BinaryOp::Divide,
                    "-" => BinaryOp::Subtract,
                    op => return Err(anyhow!("Unknown operator `{}`", op)),
                };

                if !self.cursor.goto_next_sibling() {
                    return Err(anyhow!("Missing operator and rhs in binary expression"));
                }

                let rhs = self.build_expr()?;

                self.cursor.goto_parent();
                Ok(Expr::Binary(op, Box::new(lhs), Box::new(rhs)))
            }
            "call_expression" => {
                let mut callee = None;
                let mut calls = Vec::new();
                let mut has_next_child = self.cursor.goto_first_child();
                while has_next_child {
                    match self.cursor.field_name() {
                        Some("callee") => {
                            callee = Some(self.build_expr()?);
                        }
                        Some("call") => {
                            calls.push(self.build_call_arguments()?);
                        }
                        Some(unknown_field) => {
                            return Err(anyhow!(
                                "Unhandled field `{}` in call_expression",
                                unknown_field
                            ))
                        }
                        None => {}
                    }
                    has_next_child = self.cursor.goto_next_sibling();
                }
                self.cursor.goto_parent();

                let callee = callee.ok_or_else(|| anyhow!("Function call must have callee"))?;
                if calls.is_empty() {
                    Ok(callee)
                } else {
                    Ok(Expr::Call {
                        callee: Box::new(callee),
                        calls,
                    })
                }
            }
            unknown_kind => Err(anyhow!(
                "Kind `{}` is not handled yet in expression: {}",
                unknown_kind,
                node.utf8_text(self.source)?
            )),
        };

        self.cursor.goto_parent();
        result
    }

    fn build_call_arguments(&mut self) -> Result<Vec<Expr>> {
        let mut arguments = Vec::new();
        let mut has_next_child = self.cursor.goto_first_child();
        while has_next_child {
            if let Some("argument") = self.cursor.field_name() {
                arguments.push(self.build_expr()?);
            }
            has_next_child = self.cursor.goto_next_sibling();
        }

        self.cursor.goto_parent();
        Ok(arguments)
    }

    fn build_value(&mut self) -> Result<Value> {
        self.cursor.goto_first_child();
        let node = self.cursor.node();
        let node_text = node.utf8_text(self.source)?;
        self.cursor.goto_parent();
        match node.kind() {
            "integer" => {
                let i: i32 = node_text.parse()?;
                Ok(Value::I32(i))
            }
            "float" => {
                let f: f32 = node_text.parse()?;
                Ok(Value::F32(f))
            }
            "string" => {
                let s = node_text.strip_prefix('"').unwrap_or(node_text);
                let s = s.strip_suffix('"').unwrap_or(s);

                Ok(Value::String(s.to_string()))
            }
            "boolean" => {
                let b: bool = node_text.parse()?;

                Ok(Value::Bool(b))
            }
            // TODO: In the future we should collect these in a Unknown type.
            unknown_kind => Err(anyhow!(
                "Kind `{}` is not handled yet in value.",
                unknown_kind
            )),
        }
    }
}
