use serde::Serialize;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::Range;

#[derive(Debug, Clone, Serialize, Default)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub struct Span<T: Debug + Clone + PartialEq + Serialize + Hash>(pub T, pub Range<usize>);

impl<T: Debug + Clone + PartialEq + Serialize + Hash> Eq for Span<T> {}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub enum TypeDeclaration {
    Struct {
        name: Span<String>,
        fields: Vec<(String, Span<TypeSig>)>,
    },
    Enum {
        name: String,
        variants: Vec<(String, Vec<(String, Span<TypeSig>)>)>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub struct Function {
    pub name: Span<String>,
    pub params: Vec<(Span<String>, Span<TypeSig>)>,
    pub return_type: Option<Span<TypeSig>>,
    pub body: Span<ExprBlock>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub enum Stmt {
    Let(Span<String>, Span<Expr>),
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
        name: Span<String>,
        condition: Span<Expr>,
        then_block: Span<ExprBlock>,
        else_block: Span<ExprBlock>,
    },
    Expr(Span<Expr>),
    If {
        condition: Span<Expr>,
        then_block: Vec<Span<Stmt>>,
        else_block: Vec<Span<Stmt>>,
    },
    Return(Option<Span<Expr>>),
    Import {
        ty: Span<ImportType>,
        default_import: Option<Span<String>>,
        named_imports: Vec<Span<String>>,
        path: Span<String>,
    },
    Type(TypeDeclaration),
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub enum ImportType {
    // An import from another Vicuna module
    Internal,
    // An import from a JavaScript module
    External,
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub struct ExprBlock {
    pub stmts: Vec<Span<Stmt>>,
    pub end_expr: Option<Span<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub enum Expr {
    Value(Value),
    Variable(String),
    // Combination of call, i.e. `foo(10)`, field access, i.e. `foo.bar`,
    // and index access, i.e. `foo[10]`.
    // This could probably be optimized by storing `PostFix` as a Vec
    PostFix(Box<Span<Expr>>, Span<PostFix>),
    Binary(Span<BinaryOp>, Box<Span<Expr>>, Box<Span<Expr>>),
    Unary(Span<UnaryOp>, Box<Span<Expr>>),
    Struct(Span<String>, Vec<(Span<String>, Span<Expr>)>),
    Enum {
        enum_name: Span<String>,
        variant_name: Span<String>,
        fields: Vec<(Span<String>, Span<Expr>)>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub enum PostFix {
    Field(Span<String>),
    Index(Box<Span<Expr>>),
    Args(Vec<Span<Expr>>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub enum TypeSig {
    I32,
    F32,
    String,
    Bool,
    Named(String),
}

impl Eq for TypeSig {}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub enum Value {
    I32(i32),
    F32(f32),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Hash)]
pub enum BinaryOp {
    Add,
    Subtract,
    Divide,
    Multiply,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize, Hash)]
pub enum UnaryOp {
    Negate,
    Not,
}
