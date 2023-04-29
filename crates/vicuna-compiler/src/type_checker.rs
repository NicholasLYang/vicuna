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
    BinaryOp, Expr, Function, ImportType, PostFix, Program, Span, Stmt, TypeDeclaration, TypeSig,
    UnaryOp, Value,
};
use miette::Diagnostic;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem;
use thiserror::Error;

// TODO: Intern strings
type Name = String;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Type {
    I32,
    F32,
    Bool,
    Void,
    String,
    // A type for JS values
    Js,
    Array(Box<Type>),
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
            Type::Js => write!(f, "<js value>"),
            Type::Array(ty) => write!(f, "{}[]", ty),
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
struct SymbolTable<T> {
    scopes: Vec<HashMap<Name, T>>,
    current_scope: usize,
}

impl<T> SymbolTable<T> {
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

    fn insert(&mut self, name: Name, value: T) {
        self.scopes[self.current_scope].insert(name, value);
    }

    fn lookup(&self, name: &Name) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }

        None
    }
}

pub struct TypeChecker {
    symbol_table: SymbolTable<Type>,
    defined_structs: SymbolTable<HashMap<Name, Type>>,
    defined_enums: SymbolTable<HashMap<Name, HashMap<Name, Type>>>,
    return_type: Option<Type>,
    pub(crate) errors: Vec<TypeError>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Diagnostic, Error)]
pub enum TypeError {
    #[error("Type mismatch: expected {0}, got {1}")]
    #[diagnostic(code(type_error::type_mismatch))]
    TypeMismatch(Type, Type),
    #[error("Got different struct fields than defined: expected {0:?}, got {1:?}")]
    FieldsMismatch(HashMap<Name, Type>, HashMap<Name, Option<Type>>),
    #[error("Undefined variable {0}")]
    UndefinedVariable(Name),
    #[error("Undefined struct {0}")]
    UndefinedStruct(Name),
    #[error("Expected {0} args but received {1}")]
    ArityMismatch(usize, usize),
    #[error("Type {0} is not callable")]
    NotCallable(Type),
    #[error("Type {0} is not a struct")]
    NotStruct(Type),
    #[error("Type {0} is not an array")]
    NotArray(Type),
    #[error("Cannot return outside a function")]
    ReturnOutsideFunction,
    #[error("Struct {struct_name} does not have field {field_name}")]
    UndefinedField { struct_name: Name, field_name: Name },
    #[error("Enum {enum_name} does not have variant {variant_name}")]
    UndefinedVariant { enum_name: Name, variant_name: Name },
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
            defined_structs: SymbolTable::new(),
            defined_enums: SymbolTable::new(),
            return_type: None,
            errors: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.symbol_table.enter_scope();
        self.defined_enums.enter_scope();
        self.defined_structs.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.symbol_table.exit_scope();
        self.defined_enums.exit_scope();
        self.defined_structs.exit_scope();
    }

    pub fn check(mut self, program: &Program) -> Vec<TypeError> {
        self.check_block(&program.statements);

        self.errors
    }

    fn add_type_declaration(&mut self, type_declaration: &TypeDeclaration) {
        match type_declaration {
            TypeDeclaration::Struct { name, fields } => {
                self.defined_structs.insert(
                    name.clone(),
                    fields
                        .iter()
                        .map(|(name, ty)| (name.clone(), ty.into()))
                        .collect(),
                );
            }
            TypeDeclaration::Enum { name, variants } => {
                self.defined_enums.insert(
                    name.clone(),
                    variants
                        .iter()
                        .map(|(name, fields)| {
                            (
                                name.clone(),
                                fields
                                    .iter()
                                    .map(|(name, ty)| (name.clone(), ty.into()))
                                    .collect(),
                            )
                        })
                        .collect(),
                );
            }
        }
    }

    fn check_block(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            if let Stmt::Function(Function {
                name,
                return_type,
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

                self.enter_scope();

                self.check_block(&then_block.stmts);

                let then_ty = if let Some(end_expr) = &then_block.end_expr {
                    self.check_expr(end_expr)
                } else {
                    Some(Type::Void)
                };

                self.exit_scope();
                self.enter_scope();
                self.check_block(&else_block.stmts);

                let else_ty = if let Some(end_expr) = &else_block.end_expr {
                    self.check_expr(end_expr)
                } else {
                    Some(Type::Void)
                };
                self.symbol_table.exit_scope();

                // We only call try here because we want to collect all the type errors in both branches
                let then_ty = then_ty?;
                let else_ty = else_ty?;
                if then_ty != else_ty {
                    self.errors
                        .push(TypeError::TypeMismatch(then_ty.clone(), else_ty));
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
                self.enter_scope();

                for (name, ty) in params {
                    self.symbol_table.insert(name.clone(), ty.into());
                }

                let return_type: Type = return_type.as_ref().into();

                let old_return_type = mem::replace(&mut self.return_type, Some(return_type));

                self.check_block(&body.stmts);

                let end_expr_ty = if let Some(end_expr) = &body.end_expr {
                    self.check_expr(end_expr)
                } else {
                    Some(Type::Void)
                };
                self.symbol_table.exit_scope();
                let return_type = mem::replace(&mut self.return_type, old_return_type)
                    .expect("return type should be set");

                let end_expr_ty = end_expr_ty?;

                if end_expr_ty != return_type {
                    self.errors
                        .push(TypeError::TypeMismatch(end_expr_ty, return_type));
                }
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition_ty = self.check_expr(condition)?;
                if condition_ty != Type::Bool {
                    self.errors
                        .push(TypeError::TypeMismatch(Type::Bool, condition_ty));
                }

                self.enter_scope();
                self.check_block(then_block);
                self.exit_scope();

                self.enter_scope();
                self.check_block(else_block);
                self.exit_scope();
            }
            Stmt::Return(expr) => {
                let ty = if let Some(expr) = expr {
                    self.check_expr(expr)?
                } else {
                    Type::Void
                };

                if let Some(return_type) = &self.return_type {
                    if ty != *return_type {
                        self.errors
                            .push(TypeError::TypeMismatch(ty, return_type.clone()));
                    }
                } else {
                    self.errors.push(TypeError::ReturnOutsideFunction);
                }
            }
            Stmt::Import {
                ty: ImportType::External,
                default_import,
                named_imports,
                path: _,
            } => {
                if let Some(default_import) = default_import {
                    self.symbol_table.insert(default_import.clone(), Type::Js);
                }

                for name in named_imports {
                    self.symbol_table.insert(name.clone(), Type::Js);
                }
            }
            Stmt::Type(decl) => {
                self.add_type_declaration(decl);
            }
            Stmt::Import { .. } => todo!("internal imports not implemented yet"),
        }

        Some(())
    }

    fn check_expr(&mut self, expr: &Span<Expr>) -> Option<Type> {
        match &expr.0 {
            Expr::Binary(op, lhs, rhs) => {
                let lhs_ty = self.check_expr(lhs)?;
                let rhs_ty = self.check_expr(rhs)?;
                match op.0 {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if lhs_ty == Type::I32 && rhs_ty == Type::I32 {
                            Some(Type::I32)
                        } else if lhs_ty == Type::F32 && rhs_ty == Type::F32 {
                            Some(Type::F32)
                        } else {
                            self.errors.push(TypeError::TypeMismatch(lhs_ty, rhs_ty));
                            None
                        }
                    }
                }
            }
            Expr::Unary(op, rhs) => {
                let rhs_ty = self.check_expr(rhs)?;
                match op.0 {
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
            Expr::PostFix(callee, PostFix::Args(args)) => {
                if let Expr::Variable(name) = callee.as_ref() {
                    if name == "print" {
                        return Some(Type::Void);
                    }
                }

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
            Expr::PostFix(callee, PostFix::Field(field)) => {
                let callee_ty = self.check_expr(callee)?;
                if let Type::Named(struct_name) = callee_ty {
                    let fields = self.defined_structs.lookup(&struct_name)?;
                    if let Some(ty) = fields.get(field) {
                        Some(ty.clone())
                    } else {
                        self.errors.push(TypeError::UndefinedField {
                            struct_name: struct_name.clone(),
                            field_name: field.clone(),
                        });

                        None
                    }
                } else {
                    self.errors.push(TypeError::NotStruct(callee_ty));
                    None
                }
            }
            Expr::PostFix(callee, PostFix::Index(index)) => {
                let callee_ty = self.check_expr(callee)?;
                let index_ty = self.check_expr(index)?;
                if index_ty != Type::I32 {
                    self.errors
                        .push(TypeError::TypeMismatch(Type::I32, index_ty));
                }

                if let Type::Array(ty) = callee_ty {
                    Some(*ty)
                } else {
                    self.errors.push(TypeError::NotArray(callee_ty));
                    None
                }
            }
            Expr::Struct(name, literal_fields) => {
                let Some(struct_type_fields) = self.defined_structs.lookup(name).cloned() else {
                    self.errors.push(TypeError::UndefinedStruct(name.clone()));
                    return None;
                };

                self.compare_fields(literal_fields, struct_type_fields)?;

                Some(Type::Named(name.clone()))
            }
            Expr::Enum {
                enum_name,
                variant_name,
                fields,
            } => {
                let Some(variant_fields) = self.defined_enums.lookup(enum_name).and_then(|enum_variants| enum_variants.get(variant_name)).cloned() else {
                    self.errors.push(TypeError::UndefinedVariant {
                        enum_name: enum_name.0.clone(),
                        variant_name: variant_name.0.clone(),
                    });
                    return None;
                };

                self.compare_fields(fields, variant_fields)?;

                Some(Type::Named(variant_name.0.clone()))
            }
        }
    }

    fn compare_fields(
        &mut self,
        literal_fields: &HashMap<String, Span<Expr>>,
        mut struct_type_fields: HashMap<String, Type>,
    ) -> Option<()> {
        if literal_fields.len() != struct_type_fields.len() {
            let struct_type_fields = struct_type_fields.clone();
            let literal_field_types = literal_fields
                .iter()
                .map(|(field_name, field_value)| {
                    let field_ty = self.check_expr(field_value);
                    (field_name.clone(), field_ty)
                })
                .collect::<HashMap<_, _>>();

            self.errors.push(TypeError::FieldsMismatch(
                struct_type_fields,
                literal_field_types,
            ));
            return None;
        }

        for (field_name, expr) in literal_fields.iter() {
            let expected_ty = if let Some(expected_ty) = struct_type_fields.remove(field_name) {
                expected_ty
            } else {
                self.errors.push(TypeError::UndefinedField {
                    struct_name: field_name.clone(),
                    field_name: field_name.clone(),
                });
                return None;
            };

            let expr_ty = self.check_expr(expr)?;

            if expr_ty != expected_ty {
                self.errors
                    .push(TypeError::TypeMismatch(expr_ty.clone(), expected_ty));
            }
        }

        Some(())
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
                    Expr::PostFix(Box::new(Expr::Variable("f".into())), PostFix::Args(vec![])),
                ),
            ],
        };

        assert_eq!(checker.check(&program), vec![]);

        let checker = TypeChecker::new();
        let program = Program {
            type_declarations: vec![],
            statements: vec![Stmt::Let(
                "x".into(),
                Expr::PostFix(Box::new(Expr::Variable("f".into())), PostFix::Args(vec![])),
            )],
        };

        assert_eq!(
            checker.check(&program),
            vec![TypeError::UndefinedVariable("f".into())]
        );
    }
}
