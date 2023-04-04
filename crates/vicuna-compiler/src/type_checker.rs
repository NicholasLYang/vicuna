//! Type checker module. Currently extremely extremely simple.
//!
//! The type checker is designed to be executed in parallel to the compilation
//! process, similar to TypeScript (any code that parses should compile, but may not be semantically valid)
//!
//!
//! The current checker does not do
//!  - Pattern matching
//!  - Generics
//!  - Type unification
//!  - Borrow checking
//!
use crate::ast::{
    BinaryOp, Expr, Function, Program, Stmt, TypeDeclaration, TypeSig, UnaryOp, Value,
};
use serde::Serialize;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

// TODO: Intern strings
type Name = String;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Type {
    I32,
    F32,
    Bool,
    Void,
    String,
    Function {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
    Named(Name),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::String => write!(f, "string"),
            Type::Function {
                param_types,
                return_type,
            } => {
                write!(f, "(")?;
                for (i, param_type) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param_type)?;
                }
                write!(f, ") -> {}", return_type)
            }
            Type::Named(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug)]
struct SymbolTable {
    scopes: Vec<HashMap<Name, Type>>,
    current_scope: usize,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            current_scope: 0,
        }
    }

    fn enter_scope(&mut self) {
        self.current_scope += 1;
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.current_scope -= 1;
        self.scopes.pop();
    }

    fn insert(&mut self, name: Name, ty: Type) {
        self.scopes[self.current_scope].insert(name, ty);
    }

    fn lookup(&self, name: &Name) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }

        None
    }
}

pub struct TypeChecker {
    symbol_table: SymbolTable,
    named_types: HashMap<String, HashMap<Name, Type>>,
    pub(crate) errors: Vec<TypeError>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeError {
    TypeMismatch(Type, Type),
    UndefinedVariable(Name),
    ArityMismatch(usize, usize),
    NotCallable(Type),
}

// TODO: Make this some fancy error display
impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::TypeMismatch(expected, actual) => {
                write!(f, "Expected type {}, got {}", expected, actual)
            }
            TypeError::UndefinedVariable(name) => write!(f, "Undefined variable {}", name),
            TypeError::ArityMismatch(expected, actual) => {
                write!(f, "Expected {} arguments, got {}", expected, actual)
            }
            TypeError::NotCallable(ty) => write!(f, "Type {} is not callable", ty),
        }
    }
}

impl From<Option<&TypeSig>> for Type {
    fn from(value: Option<&TypeSig>) -> Self {
        match value {
            Some(sig) => sig.into(),
            None => Type::Void,
        }
    }
}

impl From<&TypeSig> for Type {
    fn from(value: &TypeSig) -> Self {
        match value {
            TypeSig::I32 => Type::I32,
            TypeSig::F32 => Type::F32,
            TypeSig::Bool => Type::Bool,
            TypeSig::String => Type::String,
            TypeSig::Named(name) => Type::Named(name.clone()),
        }
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            named_types: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn check(mut self, program: &Program) -> Vec<TypeError> {
        self.add_type_declarations(&program.type_declarations);
        self.check_block(&program.statements);

        self.errors
    }

    fn add_type_declarations(&mut self, type_declarations: &[TypeDeclaration]) {
        for decl in type_declarations {
            #[allow(irrefutable_let_patterns)]
            if let TypeDeclaration::Struct { name, fields } = decl {
                self.named_types.insert(
                    name.clone(),
                    fields
                        .iter()
                        .map(|(name, ty)| (name.clone(), ty.into()))
                        .collect(),
                );
            }
        }
    }

    fn check_block(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            if let Stmt::Function(Function {
                name,
                return_type: return_type,
                params,
                ..
            }) = stmt
            {
                let ty = Type::Function {
                    param_types: params.iter().map(|(_, ty)| ty.into()).collect(),
                    return_type: Box::new(return_type.as_ref().into()),
                };
                self.symbol_table.insert(name.clone(), ty);
            }
        }
        for stmt in stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Option<()> {
        match stmt {
            Stmt::Let(name, rhs) => {
                let ty = self.check_expr(rhs)?;
                self.symbol_table.insert(name.clone(), ty);
            }
            Stmt::LetIf {
                name,
                condition,
                then_block,
                else_block,
            } => {
                let condition_ty = self.check_expr(condition)?;
                if condition_ty != Type::Bool {
                    self.errors
                        .push(TypeError::TypeMismatch(Type::Bool, condition_ty));
                }

                self.symbol_table.enter_scope();

                self.check_block(&then_block.stmts);

                let then_ty = if let Some(end_expr) = &then_block.end_expr {
                    self.check_expr(end_expr)
                } else {
                    Some(Type::Void)
                };

                self.symbol_table.exit_scope();
                self.symbol_table.enter_scope();
                self.check_block(&else_block.stmts);

                let else_ty = if let Some(end_expr) = &else_block.end_expr {
                    self.check_expr(&end_expr)
                } else {
                    Some(Type::Void)
                };
                self.symbol_table.exit_scope();

                // We only call try here because we want to collect all the type errors in both branches
                let then_ty = then_ty?;
                let else_ty = else_ty?;
                if then_ty != else_ty {
                    self.errors
                        .push(TypeError::TypeMismatch(then_ty.clone(), else_ty.clone()));
                }

                self.symbol_table.insert(name.clone(), then_ty);
            }
            Stmt::Expr(expr) => {
                self.check_expr(expr)?;
            }
            Stmt::Function(Function {
                name: _,
                params,
                body,
                return_type,
            }) => {
                self.symbol_table.enter_scope();

                for (name, ty) in params {
                    self.symbol_table.insert(name.clone(), ty.into());
                }

                self.check_block(&body.stmts);

                let end_expr_ty = if let Some(end_expr) = &body.end_expr {
                    self.check_expr(end_expr)
                } else {
                    Some(Type::Void)
                };
                self.symbol_table.exit_scope();

                let end_expr_ty = end_expr_ty?;
                let return_ty = return_type.as_ref().into();
                if end_expr_ty != return_ty {
                    self.errors
                        .push(TypeError::TypeMismatch(end_expr_ty, return_ty));
                }
            }
        }

        Some(())
    }

    fn check_expr(&mut self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Binary(op, lhs, rhs) => {
                let lhs_ty = self.check_expr(lhs)?;
                let rhs_ty = self.check_expr(rhs)?;
                match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if lhs_ty == Type::I32 && rhs_ty == Type::I32 {
                            Some(Type::I32)
                        } else if lhs_ty == Type::F32 && rhs_ty == Type::F32 {
                            Some(Type::F32)
                        } else {
                            self.errors
                                .push(TypeError::TypeMismatch(lhs_ty.clone(), rhs_ty.clone()));
                            None
                        }
                    }
                }
            }
            Expr::Unary(op, rhs) => {
                let rhs_ty = self.check_expr(rhs)?;
                match op {
                    UnaryOp::Not => {
                        if rhs_ty != Type::Bool {
                            self.errors
                                .push(TypeError::TypeMismatch(Type::Bool, rhs_ty));
                        }

                        Some(Type::Bool)
                    }
                    UnaryOp::Negate => {
                        if rhs_ty != Type::I32 {
                            self.errors.push(TypeError::TypeMismatch(Type::I32, rhs_ty));
                        }

                        Some(Type::I32)
                    }
                }
            }
            Expr::Value(value) => match value {
                Value::I32(_) => Some(Type::I32),
                Value::Bool(_) => Some(Type::Bool),
                Value::String(_) => Some(Type::String),
                Value::F32(_) => Some(Type::F32),
            },
            Expr::Variable(name) => {
                if let Some(ty) = self.symbol_table.lookup(name) {
                    Some(ty.clone())
                } else {
                    self.errors.push(TypeError::UndefinedVariable(name.clone()));
                    None
                }
            }
            Expr::Call { callee, args } => {
                println!("callee: {:?}", callee);
                println!("symbol table: {:?}", self.symbol_table);
                let callee_ty = self.check_expr(callee)?;
                if let Type::Function {
                    param_types,
                    return_type,
                } = callee_ty
                {
                    if args.len() != param_types.len() {
                        self.errors
                            .push(TypeError::ArityMismatch(param_types.len(), args.len()));
                    }

                    for (arg, param_type) in args.iter().zip(param_types) {
                        let arg_ty = self.check_expr(arg)?;
                        if arg_ty != param_type {
                            self.errors
                                .push(TypeError::TypeMismatch(param_type.clone(), arg_ty.clone()));
                        }
                    }

                    Some(*return_type)
                } else {
                    self.errors.push(TypeError::NotCallable(callee_ty));
                    None
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ExprBlock;

    #[test]
    fn test_check_expr() {
        let mut checker = TypeChecker::new();
        let expr = Expr::Binary(
            BinaryOp::Add,
            Box::new(Expr::Value(Value::I32(1))),
            Box::new(Expr::Value(Value::I32(2))),
        );

        assert_eq!(checker.check_expr(&expr), Some(Type::I32));

        let expr = Expr::Binary(
            BinaryOp::Add,
            Box::new(Expr::Value(Value::I32(1))),
            Box::new(Expr::Value(Value::Bool(true))),
        );

        assert_eq!(checker.check_expr(&expr), None);
        assert_eq!(
            checker.errors,
            vec![TypeError::TypeMismatch(Type::I32, Type::Bool)]
        );
    }

    #[test]
    fn test_variable_scopes() {
        let checker = TypeChecker::new();
        let program = Program {
            type_declarations: vec![],
            statements: vec![
                Stmt::Let("x".into(), Expr::Value(Value::I32(1))),
                Stmt::Let("y".into(), Expr::Value(Value::I32(2))),
                Stmt::LetIf {
                    name: "z".into(),
                    condition: Expr::Value(Value::Bool(true)),
                    then_block: ExprBlock {
                        stmts: vec![],
                        end_expr: Some(Expr::Variable("x".into())),
                    },
                    else_block: ExprBlock {
                        stmts: vec![],
                        end_expr: Some(Expr::Variable("y".into())),
                    },
                },
                Stmt::Expr(Expr::Binary(
                    BinaryOp::Add,
                    Box::new(Expr::Variable("z".into())),
                    Box::new(Expr::Variable("y".into())),
                )),
            ],
        };

        assert_eq!(checker.check(&program), vec![]);

        let checker = TypeChecker::new();
        let program = Program {
            type_declarations: vec![],
            statements: vec![
                Stmt::Let("x".into(), Expr::Value(Value::I32(1))),
                Stmt::Let("y".into(), Expr::Value(Value::I32(2))),
                Stmt::LetIf {
                    name: "z".into(),
                    condition: Expr::Value(Value::Bool(true)),
                    then_block: ExprBlock {
                        stmts: vec![],
                        end_expr: Some(Expr::Variable("a".into())),
                    },
                    else_block: ExprBlock {
                        stmts: vec![],
                        end_expr: Some(Expr::Variable("b".into())),
                    },
                },
                Stmt::Expr(Expr::Binary(
                    BinaryOp::Add,
                    Box::new(Expr::Variable("c".into())),
                    Box::new(Expr::Variable("y".into())),
                )),
            ],
        };

        assert_eq!(
            checker.check(&program),
            vec![
                TypeError::UndefinedVariable("a".into()),
                TypeError::UndefinedVariable("b".into()),
                TypeError::UndefinedVariable("c".into())
            ]
        );
    }

    #[test]
    fn test_function_hoisting() {
        let checker = TypeChecker::new();
        let program = Program {
            type_declarations: vec![],
            statements: vec![
                Stmt::Function(Function {
                    name: "f".into(),
                    params: vec![],
                    return_type: Some(TypeSig::I32),
                    body: ExprBlock {
                        stmts: vec![],
                        end_expr: Some(Expr::Value(Value::I32(1))),
                    },
                }),
                Stmt::Let(
                    "x".into(),
                    Expr::Call {
                        callee: Box::new(Expr::Variable("f".into())),
                        args: vec![],
                    },
                ),
            ],
        };

        assert_eq!(checker.check(&program), vec![]);

        let checker = TypeChecker::new();
        let program = Program {
            type_declarations: vec![],
            statements: vec![Stmt::Let(
                "x".into(),
                Expr::Call {
                    callee: Box::new(Expr::Variable("f".into())),
                    args: vec![],
                },
            )],
        };

        assert_eq!(
            checker.check(&program),
            vec![TypeError::UndefinedVariable("f".into())]
        );
    }
}
