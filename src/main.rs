use std::fs::read_to_string;
use tree_sitter::{Parser, Language, Tree, TreeCursor};
use anyhow::{anyhow, Result};
use crate::ast::{BinaryOp, Expr, Value};

mod ast;

extern "C" { fn tree_sitter_vicuna() -> Language; }

fn main() -> Result<()> {
    let source = read_to_string("tree-sitter-vicuna/example-file")?;
    let mut parser = Parser::new();
    parser.set_language(unsafe { tree_sitter_vicuna() }).unwrap();
    let tree = parser.parse(&source, None).ok_or_else(|| anyhow!("Unable to parse code"))?;

    let mut ast_builder = ASTBuilder::new(&source, &tree)?;
    let tree = ast_builder.build_ast()?;
    println!("{:?}", tree);
    Ok(())
}

/// Builds AST from tree-sitter CST
struct ASTBuilder<'a> {
    source: &'a [u8],
    cursor: TreeCursor<'a>
}

impl<'a> ASTBuilder<'a>{
    fn new(source: &'a str, tree: &'a Tree) -> Result<ASTBuilder<'a>> {
        Ok(Self {
            source: source.as_bytes(),
            cursor: tree.walk()
        })
    }

    fn build_ast(&mut self) -> Result<Vec<Expr>> {
        let mut has_next_expr = self.cursor.goto_first_child();
        let mut expressions = Vec::new();
        while has_next_expr {
            expressions.push(self.build_expr()?);
            // Skip ";"
            self.cursor.goto_next_sibling();
            // Skip "\n"
            self.cursor.goto_next_sibling();
            has_next_expr = self.cursor.goto_next_sibling();
        }

        Ok(expressions)
    }

    fn build_expr(&mut self) -> Result<Expr> {
        let node = self.cursor.node();
        match node.kind() {
            "primary_expression" => {
                self.cursor.goto_first_child();
                let node = self.cursor.node();
                let result = match node.kind() {
                    "value" => {
                        Ok(Expr::Value(self.build_value()?))
                    },
                    "variable" => {
                        Ok(Expr::Variable(node.utf8_text(self.source)?.to_string()))
                    }
                    unknown_kind => Err(anyhow!("Kind `{}` is not handled yet in primary_expression.", unknown_kind))
                };

                self.cursor.goto_parent();
                result
            },
            "binary_expression" => {
                self.cursor.goto_first_child();
                let lhs = self.build_expr()?;

                if !self.cursor.goto_next_sibling() {
                    return Err(anyhow!("Missing operator and rhs in binary expression"))
                }

                let op_node = self.cursor.node();
                let op = match op_node.utf8_text(self.source)? {
                    "+" => BinaryOp::Add,
                    "*" => BinaryOp::Multiply,
                    "/" => BinaryOp::Divide,
                    "-" => BinaryOp::Subtract,
                    op => return Err(anyhow!("Unknown operator `{}`", op))
                };

                if !self.cursor.goto_next_sibling() {
                    return Err(anyhow!("Missing operator and rhs in binary expression"))
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
                            return Err(anyhow!("Unhandled field `{}` in call_expression", unknown_field))
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
                        calls
                    })
                }
            }
            unknown_kind => Err(anyhow!("Kind `{}` is not handled yet in expression.", unknown_kind))
        }
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
                let i: i64 = node_text.parse()?;
                Ok(Value::I64(i))
            }
            "float" => {
                let f: f64 = node_text.parse()?;
                Ok(Value::F64(f))
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
            unknown_kind => Err(anyhow!("Kind `{}` is not handled yet in value.", unknown_kind))
        }
    }
}
