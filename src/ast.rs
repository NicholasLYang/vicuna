use anyhow::{anyhow, Result};
use std::collections::HashMap;
use tree_sitter_c2rust::{Language, Parser, Tree, TreeCursor};

extern "C" {
    fn tree_sitter_vicuna() -> Language;
}

pub struct Program {
    pub type_declarations: Vec<TypeDeclaration>,
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDeclaration {
    Struct(String, HashMap<String, TypeSig>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, TypeSig)>,
    pub return_type: Option<TypeSig>,
    pub body: ExpressionBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let(String, Expr),
    Function(Function),
    /// Let assigned to an if expression.
    /// ```
    ///  let a = if foo {
    ///    1
    ///  } else {
    ///    2
    ///  }
    /// ```
    LetIf {
        name: String,
        condition: Expr,
        then_block: ExpressionBlock,
        else_block: ExpressionBlock,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionBlock {
    pub stmts: Vec<Stmt>,
    pub end_expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Value(Value),
    Variable(String),
    // Right now we only handle function calls but eventually we will also handle indexing (foo[1])
    // and field access (bar.bat). In which case we need to rename this and make the `Vec<Expr>`
    // into a proper enum with variants for Call, Index, and Field
    Call {
        callee: Box<Expr>,
        calls: Vec<Vec<Expr>>,
    },
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeSig {
    I32,
    F32,
    String,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I32(i32),
    F32(f32),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Divide,
    Multiply,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
}

/// Builds AST from tree-sitter CST
pub struct ASTBuilder<'a> {
    source: &'a [u8],
    cursor: TreeCursor<'a>,
}

impl<'a> ASTBuilder<'a> {
    pub fn new(source: &'a str, tree: &'a Tree) -> Result<ASTBuilder<'a>> {
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

    // If cursor does point to expected kind, move cursor to its next sibling
    // If no more siblings, we error. If we do not point to expected kind, we error.
    fn expect_and_consume_kind(&mut self, expected_kind: &str) -> Result<()> {
        self.expect_kind(expected_kind)?;
        let has_sibling = self.cursor.goto_next_sibling();

        if has_sibling {
            Ok(())
        } else {
            Err(anyhow!("Expected `{}` to have sibling", expected_kind))
        }
    }

    // If next sibling exists, move cursor to it. If no more siblings, we error.
    fn expect_and_consume_sibling(&mut self) -> Result<()> {
        let has_sibling = self.cursor.goto_next_sibling();

        if has_sibling {
            Ok(())
        } else {
            Err(anyhow!(
                "Expected `{}` to have sibling node, instead reached end of input",
                self.cursor.node().kind()
            ))
        }
    }

    pub fn build_ast(&mut self) -> Result<Program> {
        let mut has_next_stmt = self.cursor.goto_first_child();
        let mut type_declarations = Vec::new();
        let mut statements = Vec::new();
        while has_next_stmt {
            match self.cursor.node().kind() {
                "statement" => {
                    statements.push(self.build_stmt()?);
                }
                "type_declaration" => {
                    type_declarations.push(self.build_type_declaration()?);
                }
                kind => {
                    return Err(anyhow!(
                        "Expected `statement` or `type_declaration`, received `{}`: {}",
                        kind,
                        self.cursor.node().utf8_text(self.source)?
                    ))
                }
            }
            has_next_stmt = self.cursor.goto_next_sibling();
        }

        Ok(Program {
            type_declarations,
            statements,
        })
    }

    fn build_name_type_pair(&mut self) -> Result<(String, TypeSig)> {
        let param_name = self.cursor.node().utf8_text(self.source)?.to_string();
        self.expect_and_consume_kind(":")?;
        let param_type = match self.cursor.node().utf8_text(self.source)? {
            "i32" => TypeSig::I32,
            "f32" => TypeSig::F32,
            "bool" => TypeSig::Bool,
            "string" => TypeSig::String,
            sig => return Err(anyhow!("Unknown type signature `{}`", sig)),
        };

        Ok((param_name, param_type))
    }

    fn build_parameter_list(&mut self) -> Result<Vec<(String, TypeSig)>> {
        self.expect_and_consume_parent("parameter_list")?;
        self.expect_and_consume_kind("(")?;
        let mut params = Vec::new();
        let mut has_next_param = self.cursor.goto_next_sibling();
        while has_next_param {
            let (param_name, param_type) = self.build_name_type_pair()?;
            params.push((param_name, param_type));

            self.cursor.goto_next_sibling();
            match self.cursor.node().kind() {
                ")" => break,
                "," => {
                    self.cursor.goto_next_sibling();
                }
                _ => return Err(anyhow!("Expected `,` or `)`")),
            }

            has_next_param = self.cursor.goto_next_sibling();
        }

        self.cursor.goto_parent();

        Ok(params)
    }

    fn build_type_declaration(&mut self) -> Result<TypeDeclaration> {
        self.expect_and_consume_parent("type_declaration")?;
        self.expect_and_consume_kind("struct")?;
        let type_name = self.cursor.node().utf8_text(self.source)?.to_string();
        self.expect_and_consume_kind("{")?;
        let mut fields = HashMap::new();
        let mut has_next_param = self.cursor.goto_next_sibling();
        while has_next_param {
            let (param_name, param_type) = self.build_name_type_pair()?;
            fields.insert(param_name, param_type);

            self.cursor.goto_next_sibling();
            match self.cursor.node().kind() {
                "}" => break,
                "," => {
                    self.cursor.goto_next_sibling();
                }
                _ => return Err(anyhow!("Expected `,` or `}}`")),
            }

            has_next_param = self.cursor.goto_next_sibling();
        }

        Ok(TypeDeclaration::Struct(type_name, fields))
    }

    fn build_stmt(&mut self) -> Result<Stmt> {
        self.expect_and_consume_parent("statement")?;
        let node = self.cursor.node();
        let result = match node.kind() {
            "let_declaration" => {
                self.cursor.goto_first_child();
                // First child is "let"
                self.expect_and_consume_kind("let")?;
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
                self.expect_and_consume_kind("let")?;
                let name = self.cursor.node().utf8_text(self.source)?;
                self.cursor.goto_next_sibling();
                self.expect_and_consume_kind("=")?;
                self.expect_and_consume_kind("if")?;
                let condition = self.build_expr()?;
                self.cursor.goto_next_sibling();
                let then_block = self.build_expression_block()?;
                self.cursor.goto_next_sibling();
                self.expect_and_consume_kind("else")?;
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
            "function" => {
                self.cursor.goto_first_child();
                self.expect_and_consume_kind("fn")?;
                let name = self.cursor.node().utf8_text(self.source)?;
                self.cursor.goto_next_sibling();

                let params = self.build_parameter_list()?;
                self.cursor.goto_next_sibling();

                let body = self.build_expression_block()?;
                self.cursor.goto_parent();

                Ok(Stmt::Function(Function {
                    name: name.to_string(),
                    params,
                    return_type: None,
                    body,
                }))
            }
            unknown_kind => Err(anyhow!(
                "Kind `{}` is not yet handled in statement",
                unknown_kind
            )),
        };

        self.cursor.goto_parent();
        self.cursor.goto_parent();
        result
    }

    fn build_expression_block(&mut self) -> Result<ExpressionBlock> {
        self.expect_and_consume_parent("expression_block")?;
        self.expect_and_consume_kind("{")?;
        let mut stmts = Vec::new();
        let block = loop {
            match self.cursor.node().kind() {
                "statement" => {
                    stmts.push(self.build_stmt()?);
                    self.expect_and_consume_sibling()?;
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
            "unary_expression" => {
                if !self.cursor.goto_first_child() {
                    return Err(anyhow!("Missing operator in unary expression"));
                }
                let op_node = self.cursor.node();
                let op = match op_node.utf8_text(self.source)? {
                    "-" => UnaryOp::Negate,
                    "!" => UnaryOp::Not,
                    op => return Err(anyhow!("Unknown operator `{}`", op)),
                };

                if !self.cursor.goto_next_sibling() {
                    return Err(anyhow!("Missing rhs in binary expression"));
                }

                let rhs = self.build_expr()?;

                self.cursor.goto_parent();
                Ok(Expr::Unary(op, Box::new(rhs)))
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

pub fn parse(source: &str) -> Result<Program> {
    let mut parser = Parser::new();
    parser.set_language(unsafe { tree_sitter_vicuna() })?;

    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| anyhow!("Unable to parse code"))?;
    let mut ast_builder = ASTBuilder::new(&source, &tree)?;

    ast_builder.build_ast()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_function() -> Result<()> {
        let source = r#"
        fn main() {
            let a = 1;
            let b = 2;
        }
        ""#;
        let ast = parse(source)?;
        assert_eq!(ast.statements.len(), 1);
        assert_eq!(
            ast.statements[0],
            Stmt::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: None,
                body: ExpressionBlock {
                    stmts: vec![
                        Stmt::Let("a".to_string(), Expr::Value(Value::I32(1))),
                        Stmt::Let("b".to_string(), Expr::Value(Value::I32(2)))
                    ],
                    end_expr: None,
                },
            })
        );

        let source = r#"fn main() {}""#;
        let ast = parse(source)?;
        assert_eq!(ast.statements.len(), 1);
        assert_eq!(
            ast.statements[0],
            Stmt::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: None,
                body: ExpressionBlock {
                    stmts: vec![],
                    end_expr: None,
                },
            })
        );

        let source = r#"fn main() { 10 }""#;
        let ast = parse(source)?;
        assert_eq!(ast.statements.len(), 1);
        assert_eq!(
            ast.statements[0],
            Stmt::Function(Function {
                name: "main".to_string(),
                params: vec![],
                return_type: None,
                body: ExpressionBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Value(Value::I32(10))),
                },
            })
        );

        Ok(())
    }
}
