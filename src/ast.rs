#[derive(Debug)]
pub enum Expr {
    Value(Value),
    Variable(String),
    // Right now we only handle function calls but eventually we will also handle indexing (foo[1])
    // and field access (bar.bat). In which case we need to rename this and make the `Vec<Expr>`
    // into a proper enum with variants for Call, Index, and Field
    Call {
        callee: Box<Expr>,
        calls: Vec<Vec<Expr>>
    },
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    If { condition: Box<Expr>, then_block: Box<Expr>, else_block: Box<Expr> },
    Function { arguments: Vec<(String, TypeSig)>, body: Vec<Expr> }
}

#[derive(Debug)]
pub enum TypeSig {
    I64,
    F64,
    String,
    Bool
}

#[derive(Debug)]
pub enum Value {
    I64(i64),
    F64(f64),
    Bool(bool),
    String(String)
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Subtract,
    Divide,
    Multiply
}

#[derive(Debug)]
enum UnaryOp {
    Negate,
    Not
}
