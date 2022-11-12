#[derive(Debug)]
pub enum Stmt {
    Let(String, Expr),
    Expr(Expr)
}

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
    Unary(UnaryOp, Box<Expr>)
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
    I32(i32),
    F32(f32),
    Bool(bool),
    String(String)
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Divide,
    Multiply
}

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    Not
}
