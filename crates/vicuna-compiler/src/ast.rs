use serde::Serialize;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Range;

#[derive(Debug, Clone, Serialize, Default)]
pub struct Program {
    pub statements: Vec<Span<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Span<T: Debug + Clone + PartialEq + Serialize>(pub T, pub Range<usize>);

impl<T: Debug + Clone + PartialEq + Serialize + Hash> Eq for Span<T> {}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeDeclaration {
    Struct {
        name: Span<String>,
        fields: Vec<(Span<String>, Span<TypeSig>)>,
    },
    Enum {
        name: Span<String>,
        variants: Vec<(Span<String>, Vec<(Span<String>, Span<TypeSig>)>)>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Function {
    pub name: Span<String>,
    pub params: Vec<(Span<String>, Span<TypeSig>)>,
    pub return_type: Option<Span<TypeSig>>,
    pub body: Span<ExprBlock>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Stmt {
    Let(Span<String>, Span<Expr>),
    Function(Function),
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

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ImportType {
    // An import from another Vicuna module
    Internal,
    // An import from a JavaScript module
    External,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ExprBlock {
    pub stmts: Vec<Span<Stmt>>,
    pub end_expr: Option<Box<Span<Expr>>>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
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
    // If expressions are not available in all places, because they
    // don't transpile cleanly to JavaScript. Instead, they're only
    // available when binding to a variable, i.e. let a = if foo { 1 } else { 2 }
    // and at the end of a function, i.e.
    // ```
    // fn foo() -> i32 {
    //   if bar {
    //     1
    //   } else {
    //     2
    //   }
    // }
    // ```
    If {
        condition: Box<Span<Expr>>,
        then_block: Span<ExprBlock>,
        else_block: Span<ExprBlock>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum PostFix {
    Field(Span<String>),
    Index(Box<Span<Expr>>),
    Args(Vec<Span<Expr>>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeSig {
    I32,
    F32,
    String,
    Bool,
    Named(String),
}

impl Eq for TypeSig {}

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
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum UnaryOp {
    Negate,
    Not,
}
