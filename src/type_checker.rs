use std::collections::HashMap;
use crate::ast::{BinaryOp, Expr, Stmt, UnaryOp, Value};

// TODO: Intern strings
type Name = String;

#[derive(Debug, Clone, PartialEq)]
enum Type {
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

struct TypeChecker {
    symbol_table: SymbolTable
}

#[derive(Debug, Clone, PartialEq)]
enum TypeError {
    TypeMismatch(Type, Type),
    UndefinedVariable(Name),
}

impl TypeChecker {
    fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new()
        }
    }

    fn check(&mut self, program: &[Stmt]) -> Vec<TypeError> {
        let mut errors = Vec::new();
        for stmt in program {
            if let Err(err) = self.check_stmt(stmt) {
                errors.push(err)
            }
        }

        errors
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), TypeError> {
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
                    return Err(TypeError::TypeMismatch(Type::Bool, condition_ty));
                }

                self.symbol_table.enter_scope();

                for stmt in &then_block.stmts {
                    self.check_stmt(stmt)?;
                }

                let then_ty = if let Some(end_expr) = &then_block.end_expr {
                    self.check_expr(end_expr)?
                } else {
                    Type::Void
                };

                self.symbol_table.exit_scope();
                self.symbol_table.enter_scope();
                for stmt in &else_block.stmts {
                    self.check_stmt(&stmt)?;
                }

                let else_ty = if let Some(end_expr) = &else_block.end_expr {
                    self.check_expr(&end_expr)?
                } else {
                    Type::Void
                };

                if then_ty != else_ty {
                    return Err(TypeError::TypeMismatch(then_ty, else_ty));
                }

                self.symbol_table.exit_scope();
            }
            Stmt::Expr(expr) => {
                self.check_expr(expr)?;
            }
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<Type, TypeError> {
        match expr {
            Expr::Binary(op, lhs, rhs) => {
                let lhs_ty = self.check_expr(lhs)?;
                let rhs_ty = self.check_expr(rhs)?;
                match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if lhs_ty == Type::I32 && rhs_ty == Type::I32 {
                            Ok(Type::I32)
                        } else {
                            Err(TypeError::TypeMismatch(lhs_ty, rhs_ty))
                        }
                    }
                }
            }
            Expr::Unary(op, rhs) => {
                let rhs_ty = self.check_expr(rhs)?;
                match op {
                    UnaryOp::Not => {
                        if rhs_ty == Type::Bool {
                            Ok(Type::Bool)
                        } else {
                            Err(TypeError::TypeMismatch(Type::Bool, rhs_ty))
                        }
                    }
                    UnaryOp::Negate => {
                        if rhs_ty == Type::I32 {
                            Ok(Type::I32)
                        } else {
                            Err(TypeError::TypeMismatch(Type::I32, rhs_ty))
                        }
                    }
                }
            }
            Expr::Value(value) => {
                match value {
                    Value::I32(_) => Ok(Type::I32),
                    Value::Bool(_) => Ok(Type::Bool),
                    Value::String(_) => Ok(Type::String),
                    Value::F32(_) => Ok(Type::F32)
                }
            }
            Expr::Variable(name) => {
                if let Some(ty) = self.symbol_table.lookup(name) {
                    Ok(ty.clone())
                } else {
                    Err(TypeError::UndefinedVariable(name.clone()))
                }
            }
            Expr::Call { .. } => todo!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_check_expr() {
        let mut checker = TypeChecker::new();
        let expr = Expr::Binary(
            BinaryOp::Add,
            Box::new(Expr::Value(Value::I32(1))),
            Box::new(Expr::Value(Value::I32(2))),
        );

        assert_eq!(checker.check_expr(&expr), Ok(Type::I32));

        let expr = Expr::Binary(
            BinaryOp::Add,
            Box::new(Expr::Value(Value::I32(1))),
            Box::new(Expr::Value(Value::Bool(true))),
        );

        assert_eq!(checker.check_expr(&expr), Err(TypeError::TypeMismatch(Type::I32, Type::Bool)));
    }
}
