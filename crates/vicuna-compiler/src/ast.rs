use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Default)]
pub struct Program {
    pub type_declarations: Vec<TypeDeclaration>,
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeDeclaration {
    Struct {
        name: String,
        fields: HashMap<String, TypeSig>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, TypeSig)>,
    pub return_type: Option<TypeSig>,
    pub body: ExprBlock,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Stmt {
    Let(String, Expr),
    Function(Function),
    // Let assigned to an if expression.
    // ```
    //  let a = if foo {
    //    1
    //  } else {
    //    2
    //  }
    // ```
    LetIf {
        name: String,
        condition: Expr,
        then_block: ExprBlock,
        else_block: ExprBlock,
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ExprBlock {
    pub stmts: Vec<Stmt>,
    pub end_expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expr {
    Value(Value),
    Variable(String),
    // Combination of call, i.e. `foo(10)`, field access, i.e. `foo.bar`,
    // and index access, i.e. `foo[10]`.
    // This could probably be optimized by storing `PostFix` as a Vec
    PostFix(Box<Expr>, PostFix),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum PostFix {
    Field(String),
    Index(Box<Expr>),
    Args(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeSig {
    I32,
    F32,
    String,
    Bool,
    Named(String),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Value {
    I32(i32),
    F32(f32),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum BinaryOp {
    Add,
    Subtract,
    Divide,
    Multiply,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum UnaryOp {
    Negate,
    Not,
}
