pub enum Expr {
    Value(Value),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    If { condition: Box<Expr>, then_block: Box<Expr>, else_block: Box<Expr> },
    Function { arguments: Vec<(String, TypeSig)>, body: Vec<Expr> }
}

pub enum TypeSig {
    I64,
    F64,
    String,
    Bool
}

pub enum Value {
    I32(i32),
    F32(f32),
    String(String)
}

enum BinaryOp {
    Add,
    Subtract,
    Divide,
    Multiply
}

enum UnaryOp {
    Negate,
    Not
}
