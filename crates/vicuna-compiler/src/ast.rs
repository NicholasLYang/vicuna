use serde::ser::{SerializeMap, SerializeSeq};
use serde::Serialize;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Range;

#[derive(Debug, Clone, Serialize, Default)]
pub struct Program {
    pub statements: Vec<Span<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Span<T: Debug + Clone + PartialEq>(pub T, pub Range<usize>);

impl<T: Debug + Clone + PartialEq + Serialize + Hash> Eq for Span<T> {}

pub type TypeParams = Span<Vec<Span<String>>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Fields<T: Debug + Clone + PartialEq> {
    Named(Vec<(Span<String>, Span<T>)>),
    Tuple(Vec<Span<T>>),
    Empty,
}

impl<T: Serialize + Debug + Clone + PartialEq> Serialize for Fields<T> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Fields::Named(fields) => {
                let mut map = serializer.serialize_map(Some(fields.len()))?;
                for (name, value) in fields {
                    map.serialize_entry(name, value)?;
                }
                map.end()
            }
            Fields::Tuple(fields) => {
                let mut seq = serializer.serialize_seq(Some(fields.len()))?;
                for value in fields {
                    seq.serialize_element(value)?;
                }
                seq.end()
            }
            Fields::Empty => serializer.serialize_none(),
        }
    }
}

impl<T: Debug + Clone + PartialEq> Fields<T> {
    pub fn map<U: Debug + Clone + PartialEq>(
        &self,
        mut f: impl FnMut(&Span<T>) -> Span<U>,
    ) -> Fields<U> {
        match self {
            Fields::Named(fields) => Fields::Named(
                fields
                    .iter()
                    .map(|(name, value)| (name.clone(), f(value)))
                    .collect(),
            ),
            Fields::Tuple(fields) => Fields::Tuple(fields.iter().map(f).collect()),
            Fields::Empty => Fields::Empty,
        }
    }
}

pub type TypeFields = Fields<TypeSig>;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeDeclaration {
    Struct {
        name: Span<String>,
        type_parameters: Option<TypeParams>,
        fields: TypeFields,
    },
    Enum {
        name: Span<String>,
        type_parameters: Option<TypeParams>,
        variants: Vec<(Span<String>, TypeFields)>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Function {
    pub name: Span<String>,
    pub type_parameters: Option<TypeParams>,
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
    For {
        iterator_variable: Span<String>,
        iterator: Span<Expr>,
        body: Vec<Span<Stmt>>,
    },
    Return(Option<Span<Expr>>),
    Import {
        ty: Span<ImportType>,
        default_import: Option<Span<String>>,
        named_imports: Vec<Span<String>>,
        path: Span<String>,
    },
    Type(TypeDeclaration),
    // The equivalent of OCaml's open, basically put a variant in scope. I'll probably end
    // up consolidating this with `import` at some point.
    Use {
        module: Span<String>,
        name: Span<String>,
    },
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

pub type ExprFields = Fields<Expr>;

impl ExprFields {
    pub fn len(&self) -> usize {
        match self {
            ExprFields::Tuple(fields) => fields.len(),
            ExprFields::Named(fields) => fields.len(),
            ExprFields::Empty => 0,
        }
    }
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
    Struct(Span<String>, ExprFields),
    Enum {
        enum_name: Span<String>,
        variant_name: Span<String>,
        fields: ExprFields,
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
    Match {
        expr: Box<Span<Expr>>,
        cases: Vec<(Span<MatchCase>, Span<ExprBlock>)>,
    },
    Array(Vec<Span<Expr>>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum MatchCase {
    Enum {
        enum_name: Span<String>,
        variant_name: Span<String>,
        fields: Option<MatchBindings>,
    },
    String(String),
    Char(char),
    Variable(Span<String>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum MatchBindings {
    // Tuple(field1, field2, field3) => { ... }
    Tuple(Vec<Span<String>>),
    // Struct { field1, field2, field3: rename } => { ... }
    Named(Vec<(Span<String>, Option<Span<String>>)>),
}

impl MatchBindings {
    pub fn len(&self) -> usize {
        match self {
            MatchBindings::Tuple(fields) => fields.len(),
            MatchBindings::Named(fields) => fields.len(),
        }
    }
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
    Array(Box<Span<TypeSig>>),
    Named(Span<String>, Vec<Span<TypeSig>>),
}

impl Eq for TypeSig {}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Value {
    I32(i32),
    F32(f32),
    Bool(bool),
    String(String),
    Char(char),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum BinaryOp {
    Assign,
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
