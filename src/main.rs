use tree_sitter::{Parser, Language, Tree, TreeCursor};
use anyhow::{anyhow, Result};
use crate::ast::{Expr, Value};

mod ast;

extern "C" { fn tree_sitter_vicuna() -> Language; }

fn main() -> Result<()> {
    let source = "10";
    let tree = parse_into_cst(source)?.unwrap();
    let expr = parse_source_file(source.as_bytes(), &mut tree.walk())?;
    println!("{:?}", expr);
    Ok(())
}

fn parse_into_cst(source: &str) -> Result<Option<Tree>> {
    let mut parser = Parser::new();
    parser.set_language(unsafe { tree_sitter_vicuna() }).unwrap();
    Ok(parser.parse(source, None))
}

fn parse_source_file(source: &[u8], cursor: &mut TreeCursor) -> Result<Vec<Expr>> {
    let mut has_next_expr = cursor.goto_first_child();
    let mut expressions = Vec::new();
    while has_next_expr {
        expressions.push(parse_cst_into_expr(source, cursor)?);
        has_next_expr = cursor.goto_next_sibling();
    }

    Ok(expressions)
}

fn parse_cst_into_expr(source: &[u8], cursor: &mut TreeCursor) -> Result<Expr> {
    let node = cursor.node();
    match node.kind() {
        "primary_expression" => {
            cursor.goto_first_child();
            let node = cursor.node();
            let result = match node.kind() {
                "value" => {
                    Ok(Expr::Value(parse_cst_into_value(source, cursor)?))
                },
                "variable" => {
                    Ok(Expr::Variable(node.utf8_text(source)?.to_string()))
                }
                unknown_kind => Err(anyhow!("Kind `{}` is not handled yet in primary_expression.", unknown_kind))
            };

            cursor.goto_parent();
            result
        },
        "call_expression" => {
            let mut callee = None;
            let mut calls = Vec::new();
            let mut has_next_child = cursor.goto_first_child();
            while has_next_child {
                match cursor.field_name() {
                    Some("callee") => {
                        callee = Some(parse_cst_into_expr(source, cursor)?);
                    }
                    Some("arguments") => {
                        calls.push(parse_call_arguments(source, cursor)?)
                    }
                    Some(unknown_field) => {
                        return Err(anyhow!("Unhandled field `{}` in call_expression", unknown_field))
                    }
                    None => {}
                }
                has_next_child = cursor.goto_next_sibling();
            }
            cursor.goto_parent();

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

fn parse_call_arguments(source: &[u8], cursor: &mut TreeCursor) -> Result<Vec<Expr>> {
    Ok(Vec::new())
}

fn parse_cst_into_value(source: &[u8], cursor: &mut TreeCursor) -> Result<Value> {
    cursor.goto_first_child();
    let node = cursor.node();
    let node_text = node.utf8_text(source)?;
    cursor.goto_parent();
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
