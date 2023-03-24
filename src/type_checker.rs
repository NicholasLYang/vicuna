//! Type checker module. Currently extremely extremely simple.
//!
//! The type checker is designed to be executed in parallel to the compilation
//! process, similar to TypeScript (any code that parses should compile, but may not be semantically valid)
//!
//! The current checker does not do
//!  - Pattern matching
//!  - Generics
//!  - Type unification
//!  - Borrow checking
//!
use std::collections::HashMap;
use serde::Serialize;
use crate::ast::{BinaryOp, Expr, Stmt, UnaryOp, Value};

// TODO: Intern strings
type Name = String;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Type {
    I32,
    F32,
    Bool,
    Void,
    String
}

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
    pub(crate) errors: Vec<TypeError>
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeError {
    TypeMismatch(Type, Type),
    UndefinedVariable(Name),
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    pub fn check(mut self, program: &[Stmt]) -> Vec<TypeError> {
        for stmt in program {
            self.check_stmt(stmt);
        }

        self.errors
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
                    self.errors.push(TypeError::TypeMismatch(Type::Bool, condition_ty));
                }

                self.symbol_table.enter_scope();

                for stmt in &then_block.stmts {
                    self.check_stmt(stmt)?;
                }

                let then_ty = if let Some(end_expr) = &then_block.end_expr {
                    self.check_expr(end_expr)
                } else {
                    Some(Type::Void)
                };

                self.symbol_table.exit_scope();
                self.symbol_table.enter_scope();
                for stmt in &else_block.stmts {
                    self.check_stmt(&stmt)?;
                }

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
                    self.errors.push(TypeError::TypeMismatch(then_ty.clone(), else_ty.clone()));
                }

                self.symbol_table.insert(name.clone(), then_ty);
            }
            Stmt::Expr(expr) => {
                self.check_expr(expr)?;
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
                            self.errors.push(TypeError::TypeMismatch(lhs_ty.clone(), rhs_ty.clone()));
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
                            self.errors.push(TypeError::TypeMismatch(Type::Bool, rhs_ty));
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
            Expr::Value(value) => {
                match value {
                    Value::I32(_) => Some(Type::I32),
                    Value::Bool(_) => Some(Type::Bool),
                    Value::String(_) => Some(Type::String),
                    Value::F32(_) => Some(Type::F32)
                }
            }
            Expr::Variable(name) => {
                if let Some(ty) = self.symbol_table.lookup(name) {
                    Some(ty.clone())
                } else {
                    self.errors.push(TypeError::UndefinedVariable(name.clone()));
                    None
                }
            }
            Expr::Call { .. } => todo!()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::ExpressionBlock;
    use super::*;

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
        assert_eq!(checker.errors, vec![TypeError::TypeMismatch(Type::I32, Type::Bool)]);
    }

    #[test]
    fn test_check() {
        let mut checker = TypeChecker::new();
        let program = vec![
            Stmt::Let("x".into(), Expr::Value(Value::I32(1))),
            Stmt::Let("y".into(), Expr::Value(Value::I32(2))),
            Stmt::LetIf {
                name: "z".into(),
                condition: Expr::Value(Value::Bool(true)),
                then_block: ExpressionBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Variable("x".into())),
                },
                else_block: ExpressionBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Variable("y".into())),
                },
            },
            Stmt::Expr(Expr::Binary(
                BinaryOp::Add,
                Box::new(Expr::Variable("z".into())),
                Box::new(Expr::Variable("y".into())),
            )),
        ];

        assert_eq!(checker.check(&program), vec![]);

        let mut checker = TypeChecker::new();
        let program = vec![
            Stmt::Let("x".into(), Expr::Value(Value::I32(1))),
            Stmt::Let("y".into(), Expr::Value(Value::I32(2))),
            Stmt::LetIf {
                name: "z".into(),
                condition: Expr::Value(Value::Bool(true)),
                then_block: ExpressionBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Variable("a".into())),
                },
                else_block: ExpressionBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Variable("b".into())),
                },
            },
            Stmt::Expr(Expr::Binary(
                BinaryOp::Add,
                Box::new(Expr::Variable("c".into())),
                Box::new(Expr::Variable("y".into())),
            )),
        ];

        assert_eq!(checker.check(&program), vec![TypeError::UndefinedVariable("a".into()), TypeError::UndefinedVariable("b".into()), TypeError::UndefinedVariable("c".into())]);
    }
}
