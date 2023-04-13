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
    BinaryOp, Expr, Function, PostFix, Program, Stmt, TypeDeclaration, TypeSig, UnaryOp, Value,
};
use serde::Serialize;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem;

// TODO: Intern strings
type Name = String;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Type {
    I32,
    F32,
    Bool,
    Void,
    String,
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
    return_type: Option<Type>,
    pub(crate) errors: Vec<TypeError>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum TypeError {
    TypeMismatch(Type, Type),
    FieldsMismatch(HashMap<Name, Type>, HashMap<Name, Option<Type>>),
    UndefinedVariable(Name),
    UndefinedStruct(Name),
    ArityMismatch(usize, usize),
    NotCallable(Type),
    NotStruct(Type),
    NotArray(Type),
    ReturnOutsideFunction,
    UndefinedField { struct_name: Name, field_name: Name },
}

// TODO: Make this some fancy error display
impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::TypeMismatch(expected, actual) => {
                write!(f, "Expected type {}, got {}", expected, actual)
            }
            TypeError::FieldsMismatch(expected, actual) => {
                write!(f, "Expected fields {:?}, got {:?}", expected, actual)
            }
            TypeError::UndefinedVariable(name) => write!(f, "Undefined variable {}", name),
            TypeError::UndefinedStruct(name) => write!(f, "Undefined struct {}", name),
            TypeError::ArityMismatch(expected, actual) => {
                write!(f, "Expected {} arguments, got {}", expected, actual)
            }
            TypeError::NotCallable(ty) => write!(f, "Type {} is not callable", ty),
            TypeError::NotStruct(ty) => write!(f, "Type {} is not a struct", ty),
            TypeError::NotArray(ty) => write!(f, "Type {} is not an array", ty),
            TypeError::ReturnOutsideFunction => write!(f, "Return statement outside function"),
            TypeError::UndefinedField {
                struct_name,
                field_name,
            } => write!(
                f,
                "Struct {} does not have field {}",
                struct_name, field_name
            ),
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
            return_type: None,
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
                self.symbol_table.enter_scope();

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

                self.symbol_table.enter_scope();
                self.check_block(then_block);
                self.symbol_table.exit_scope();

                self.symbol_table.enter_scope();
                self.check_block(else_block);
                self.symbol_table.exit_scope();
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
                            self.errors.push(TypeError::TypeMismatch(lhs_ty, rhs_ty));
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
                    let fields = self.named_types.get(&struct_name)?;
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
                let Some(mut struct_type_fields) = self.named_types.get(name).cloned() else {
                    self.errors.push(TypeError::UndefinedStruct(name.clone()));
                    return None;
                };

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
                    let expected_ty =
                        if let Some(expected_ty) = struct_type_fields.remove(field_name) {
                            expected_ty
                        } else {
                            self.errors.push(TypeError::UndefinedField {
                                struct_name: name.clone(),
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

                Some(Type::Named(name.clone()))
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
