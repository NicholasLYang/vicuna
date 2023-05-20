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
    BinaryOp, Expr, ExprBlock, Function, ImportType, MatchBindings, PostFix, Program, Span, Stmt,
    TypeDeclaration, TypeSig, UnaryOp, Value,
};
use miette::Diagnostic;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::mem;
use std::ops::Range;
use thiserror::Error;
use tracing::debug;

// TODO: Intern strings
type Name = String;

#[derive(Clone, PartialEq, Serialize, Hash)]
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

#[derive(Clone, PartialEq, Serialize, Hash)]
pub enum InferredType {
    Known(Type),
    Unknown,
}

impl Debug for InferredType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InferredType::Known(ty) => write!(f, "{}", ty),
            InferredType::Unknown => write!(f, "<unknown type>"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Debug for Type {
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

    fn lookup_mut(&mut self, name: &Name) -> Option<&mut T> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(ty) = scope.get_mut(name) {
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
    #[error("Type mismatch: expected {expected_ty}, got {received_ty}")]
    #[diagnostic(code(type_error::type_mismatch))]
    TypeMismatch {
        expected_ty: Type,
        received_ty: Type,
        #[label]
        span: Range<usize>,
    },
    #[error("Got different struct fields than defined: expected {expected_fields:?}, got {received_fields:?}")]
    #[diagnostic(code(type_error::struct_fields_mismatch))]
    FieldsMismatch {
        expected_fields: HashMap<Name, Type>,
        received_fields: HashMap<Name, InferredType>,
        #[label]
        span: Range<usize>,
    },
    #[error("Missing field {field_name} in struct {struct_name}")]
    #[diagnostic(code(type_error::missing_field))]
    MissingField {
        field_name: Name,
        struct_name: Name,
        #[label]
        span: Range<usize>,
    },
    #[error("Undefined variable {0}")]
    UndefinedVariable(Name, #[label] Range<usize>),
    #[error("Undefined type {0}")]
    UndefinedType(Name, #[label] Range<usize>),
    #[error("Expected {0} args but received {1}")]
    ArityMismatch(usize, usize, #[label] Range<usize>),
    #[error("Type {0} is not callable")]
    NotCallable(Type, #[label] Range<usize>),
    #[error("Type {0} is not a struct")]
    NotStruct(Type, #[label] Range<usize>),
    #[error("Type {0} is not an array")]
    NotArray(Type, #[label] Range<usize>),
    #[error("Type {0} is not an enum")]
    NotEnum(String, #[label] Range<usize>),
    #[error("Cannot return outside a function")]
    ReturnOutsideFunction(#[label] Range<usize>),
    #[error("Struct {struct_name} does not have field {field_name}")]
    UndefinedField {
        struct_name: Name,
        field_name: Name,
        #[label]
        span: Range<usize>,
    },
    #[error("Enum {enum_name} does not have variant {variant_name}")]
    UndefinedVariant {
        enum_name: Name,
        variant_name: Name,
        #[label]
        span: Range<usize>,
    },
}

impl From<Option<&TypeSig>> for Type {
    fn from(value: Option<&TypeSig>) -> Self {
        match value {
            Some(sig) => sig.into(),
            None => Type::Void,
        }
    }
}

impl From<&Option<Span<TypeSig>>> for Type {
    fn from(value: &Option<Span<TypeSig>>) -> Self {
        match value {
            Some(sig) => sig.into(),
            None => Type::Void,
        }
    }
}

impl From<&Span<TypeSig>> for Type {
    fn from(span: &Span<TypeSig>) -> Self {
        let ty: Type = (&span.0).into();

        ty
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
                let fields: HashMap<_, _> = fields
                    .iter()
                    .map(|(name, ty)| (name.0.clone(), ty.into()))
                    .collect();

                self.defined_structs.insert(name.0.clone(), fields);
            }
            TypeDeclaration::Enum { name, variants } => {
                self.defined_enums.insert(
                    name.0.clone(),
                    variants
                        .iter()
                        .map(|(name, fields)| {
                            (
                                name.0.clone(),
                                fields
                                    .iter()
                                    .map(|(name, ty)| (name.0.clone(), ty.into()))
                                    .collect(),
                            )
                        })
                        .collect(),
                );
            }
        }
    }

    fn check_block(&mut self, stmts: &[Span<Stmt>]) {
        for stmt in stmts {
            if let Stmt::Function(Function {
                name,
                return_type,
                params,
                ..
            }) = &stmt.0
            {
                let ty = Type::Function {
                    param_types: params.iter().map(|(_, ty)| ty.into()).collect(),
                    return_type: Box::new(return_type.into()),
                };
                self.symbol_table.insert(name.0.clone(), ty);
            }
        }
        for stmt in stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Span<Stmt>) -> Option<()> {
        match &stmt.0 {
            Stmt::Let(name, rhs) => {
                let ty = self.check_expr(rhs)?;
                self.symbol_table.insert(name.0.clone(), ty);
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
                    self.symbol_table.insert(name.0.clone(), ty.into());
                }

                let return_type: Type = return_type.as_ref().map(|span| &span.0).into();

                let old_return_type = mem::replace(&mut self.return_type, Some(return_type));

                self.check_block(&body.0.stmts);

                let return_type = mem::replace(&mut self.return_type, old_return_type)
                    .expect("return type should be set");

                if let Some(end_expr) = &body.0.end_expr {
                    let end_expr_ty = self.check_expr(end_expr)?;

                    if end_expr_ty != return_type {
                        self.errors.push(TypeError::TypeMismatch {
                            expected_ty: return_type,
                            received_ty: end_expr_ty,
                            span: body.0.end_expr.as_ref().unwrap().1.clone(),
                        });
                    }
                };

                self.symbol_table.exit_scope();
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition_ty = self.check_expr(condition)?;
                if condition_ty != Type::Bool {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: Type::Bool,
                        received_ty: condition_ty,
                        span: condition.1.clone(),
                    });
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
                        self.errors.push(TypeError::TypeMismatch {
                            expected_ty: return_type.clone(),
                            received_ty: ty,
                            span: stmt.1.clone(),
                        });
                    }
                } else {
                    self.errors
                        .push(TypeError::ReturnOutsideFunction(stmt.1.clone()));
                }
            }
            Stmt::Import {
                ty: Span(ImportType::External, _),
                default_import,
                named_imports,
                path: _,
            } => {
                if let Some(default_import) = default_import {
                    self.symbol_table.insert(default_import.0.clone(), Type::Js);
                }

                for name in named_imports {
                    self.symbol_table.insert(name.0.clone(), Type::Js);
                }
            }
            Stmt::Type(decl) => {
                self.add_type_declaration(decl);
            }
            Stmt::Use { module, name } => {
                let Some(variants) = self.defined_enums.lookup(&module.0) else {
                    self.errors.push(TypeError::NotEnum(module.0.clone(), stmt.1.clone()));
                    return None;
                };

                if &name.0 == "*" {
                    for (variant_name, variant_fields) in variants {
                        self.defined_structs
                            .insert(variant_name.clone(), variant_fields.clone());
                    }
                } else {
                    let Some(variant) = variants.get(&name.0) else {
                        self.errors.push(TypeError::UndefinedVariant {
                            enum_name: module.0.clone(),
                            variant_name: name.0.clone(),
                            span: stmt.1.clone(),
                        });
                        return None;
                    };

                    self.defined_structs.insert(name.0.clone(), variant.clone());
                }
            }
            Stmt::Import { .. } => todo!("internal imports not implemented yet"),
        }

        Some(())
    }

    fn check_expr(&mut self, expr: &Span<Expr>) -> Option<Type> {
        debug!("checking expr: {:?}", expr.0);
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
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: lhs_ty,
                                received_ty: rhs_ty,
                                span: rhs.1.clone(),
                            });
                            None
                        }
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        if lhs_ty != rhs_ty {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: lhs_ty,
                                received_ty: rhs_ty,
                                span: rhs.1.clone(),
                            });
                        }

                        Some(Type::Bool)
                    }
                    BinaryOp::GreaterThan
                    | BinaryOp::GreaterThanOrEqual
                    | BinaryOp::LessThan
                    | BinaryOp::LessThanOrEqual => {
                        let is_number = (lhs_ty == Type::I32 && rhs_ty == Type::I32)
                            || (lhs_ty == Type::F32 && rhs_ty == Type::F32);
                        if !is_number {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: lhs_ty,
                                received_ty: rhs_ty,
                                span: rhs.1.clone(),
                            });
                        }

                        Some(Type::Bool)
                    }
                }
            }
            Expr::Unary(op, rhs) => {
                let rhs_ty = self.check_expr(rhs)?;
                match op.0 {
                    UnaryOp::Not => {
                        if rhs_ty != Type::Bool {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: Type::Bool,
                                received_ty: rhs_ty,
                                span: expr.1.clone(),
                            });
                        }

                        Some(Type::Bool)
                    }
                    UnaryOp::Negate => {
                        if rhs_ty != Type::I32 {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: Type::I32,
                                received_ty: rhs_ty,
                                span: expr.1.clone(),
                            });
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
                    self.errors
                        .push(TypeError::UndefinedVariable(name.clone(), expr.1.clone()));
                    None
                }
            }
            Expr::PostFix(callee, Span(PostFix::Args(args), args_span)) => {
                if let Expr::Variable(name) = &callee.0 {
                    if name == "print" {
                        for arg in args {
                            self.check_expr(arg)?;
                        }
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
                        self.errors.push(TypeError::ArityMismatch(
                            param_types.len(),
                            args.len(),
                            args_span.clone(),
                        ));
                    }

                    for (arg, param_type) in args.iter().zip(param_types) {
                        let arg_ty = self.check_expr(arg)?;
                        if arg_ty != param_type {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: param_type.clone(),
                                received_ty: arg_ty.clone(),
                                span: arg.1.clone(),
                            });
                        }
                    }

                    Some(*return_type)
                } else {
                    self.errors
                        .push(TypeError::NotCallable(callee_ty, callee.1.clone()));
                    None
                }
            }
            Expr::PostFix(callee, Span(PostFix::Field(field), field_span)) => {
                let callee_ty = self.check_expr(callee)?;
                debug!("callee type: {:?}", callee);
                if let Type::Named(struct_name) = callee_ty {
                    let fields = self.defined_structs.lookup(&struct_name)?;
                    if let Some(ty) = fields.get(&field.0) {
                        Some(ty.clone())
                    } else {
                        self.errors.push(TypeError::UndefinedField {
                            struct_name: struct_name.clone(),
                            field_name: field.0.clone(),
                            span: field_span.clone(),
                        });

                        None
                    }
                } else {
                    self.errors
                        .push(TypeError::NotStruct(callee_ty, callee.1.clone()));
                    None
                }
            }
            Expr::PostFix(callee, Span(PostFix::Index(index), _)) => {
                let callee_ty = self.check_expr(callee)?;
                let index_ty = self.check_expr(index)?;
                if index_ty != Type::I32 {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: Type::I32,
                        received_ty: index_ty,
                        span: index.1.clone(),
                    });
                }

                if let Type::Array(ty) = callee_ty {
                    Some(*ty)
                } else {
                    self.errors
                        .push(TypeError::NotArray(callee_ty, callee.1.clone()));
                    None
                }
            }
            Expr::Struct(name, literal_fields) => {
                let Some(struct_type_fields) = self.defined_structs.lookup(&name.0).cloned() else {
                    self.errors.push(TypeError::UndefinedType(name.0.clone(), expr.1.clone()));
                    return None;
                };

                self.compare_fields(&name.0, expr.1.clone(), literal_fields, &struct_type_fields)?;

                Some(Type::Named(name.0.clone()))
            }
            Expr::Enum {
                enum_name,
                variant_name,
                fields,
            } => {
                let enum_variants = self.defined_enums.lookup(&enum_name.0);

                let Some(variant_fields) = enum_variants.and_then(|enum_variants| enum_variants.get(&variant_name.0)).cloned() else {
                    self.errors.push(TypeError::UndefinedVariant {
                        enum_name: enum_name.0.clone(),
                        variant_name: variant_name.0.clone(),
                        span: variant_name.1.clone(),
                    });
                    return None;
                };

                self.compare_fields(&variant_name.0, expr.1.clone(), fields, &variant_fields)?;

                Some(Type::Named(enum_name.0.clone()))
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition_ty = self.check_expr(condition)?;
                if condition_ty != Type::Bool {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: Type::Bool,
                        received_ty: condition_ty,
                        span: condition.1.clone(),
                    });
                }

                let then_ty = self.check_expression_block(then_block)?;
                let else_ty = self.check_expression_block(else_block)?;

                if then_ty != else_ty {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: then_ty.clone(),
                        received_ty: else_ty.clone(),
                        span: expr.1.clone(),
                    });
                }

                Some(then_ty)
            }
            Expr::Match { expr, cases } => {
                let expr_ty = self.check_expr(expr)?;
                // TODO: Handle non-enum pattern matching
                let Type::Named(name) = expr_ty else {
                    self.errors.push(TypeError::NotEnum(expr_ty.to_string(), expr.1.clone()));
                    return None;
                };

                // Type of each case block must be the same
                let mut case_type = None;

                for (case, block) in cases {
                    let Some(enum_variants) = self.defined_enums.lookup_mut(&name) else {
                        self.errors.push(TypeError::UndefinedType(name, expr.1.clone()));
                        return None;
                    };

                    let Some(expected_fields) = enum_variants.remove(&case.0.variant_name.0) else {
                        self.errors.push(TypeError::UndefinedVariant {
                            enum_name: name.clone(),
                            variant_name: case.0.variant_name.0.clone(),
                            span: case.0.variant_name.1.clone(),
                        });
                        return None;
                    };

                    let Some(received_fields) = &case.0.fields else {
                        if !expected_fields.is_empty() {
                            self.errors.push(TypeError::FieldsMismatch {
                                expected_fields: expected_fields.clone(),
                                received_fields: HashMap::new(),
                                span: case.0.variant_name.1.clone(),
                            });
                        }
                        continue;
                    };

                    // TODO: Handle rest expressions
                    if expected_fields.len() != received_fields.len() {
                        let received_fields = match received_fields {
                            MatchBindings::Tuple(fields) => fields
                                .into_iter()
                                .map(|field| (field.0.clone(), InferredType::Unknown))
                                .collect(),
                            MatchBindings::Named(fields) => fields
                                .into_iter()
                                .map(|(name, _)| (name.0.clone(), InferredType::Unknown))
                                .collect(),
                        };

                        self.errors.push(TypeError::FieldsMismatch {
                            expected_fields: expected_fields.clone(),
                            received_fields,
                            span: case.0.variant_name.1.clone(),
                        });
                    }

                    self.symbol_table.enter_scope();

                    match received_fields {
                        MatchBindings::Tuple(fields) => {
                            for (idx, field) in fields.into_iter().enumerate() {
                                if let Some(expected_field_ty) =
                                    expected_fields.get(&idx.to_string())
                                {
                                    self.symbol_table
                                        .insert(field.0.clone(), expected_field_ty.clone());
                                } else {
                                    self.errors.push(TypeError::UndefinedField {
                                        struct_name: format!("{}::{}", name, case.0.variant_name.0),
                                        field_name: field.0.clone(),
                                        span: field.1.clone(),
                                    });
                                }
                            }
                        }
                        MatchBindings::Named(fields) => {
                            for (field, rename) in fields {
                                if let Some(expected_field_ty) = expected_fields.get(&field.0) {
                                    let name = rename.as_ref().unwrap_or(&field).0.clone();
                                    self.symbol_table.insert(name, expected_field_ty.clone());
                                } else {
                                    self.errors.push(TypeError::UndefinedField {
                                        struct_name: format!("{}::{}", name, case.0.variant_name.0),
                                        field_name: field.0.clone(),
                                        span: field.1.clone(),
                                    });
                                }
                            }
                        }
                    }

                    enum_variants.insert(case.0.variant_name.0.clone(), expected_fields);

                    let ty = self.check_expression_block(block)?;
                    if let Some(expected_ty) = &case_type {
                        if &ty != expected_ty {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: expected_ty.clone(),
                                received_ty: ty,
                                span: block.1.clone(),
                            });
                        }
                    } else {
                        case_type = Some(ty);
                    }

                    self.symbol_table.exit_scope();
                }

                case_type
            }
        }
    }

    fn check_expression_block(&mut self, block: &Span<ExprBlock>) -> Option<Type> {
        for stmt in &block.0.stmts {
            self.check_stmt(stmt)?;
        }
        if let Some(expr) = &block.0.end_expr {
            self.check_expr(expr)
        } else {
            Some(Type::Void)
        }
    }

    // Gets types from the fields of a struct literal. Used to produce an error
    fn get_field_types(
        &mut self,
        fields: &Vec<(Span<String>, Span<Expr>)>,
    ) -> HashMap<String, InferredType> {
        let mut field_types = HashMap::new();
        for (field_name, field_value) in fields {
            let field_ty = match self.check_expr(field_value) {
                Some(ty) => InferredType::Known(ty),
                None => InferredType::Unknown,
            };
            field_types.insert(field_name.0.clone(), field_ty);
        }

        field_types
    }

    fn compare_fields(
        &mut self,
        struct_name: &str,
        struct_span: Range<usize>,
        literal_fields: &Vec<(Span<String>, Span<Expr>)>,
        struct_type_fields: &HashMap<String, Type>,
    ) -> Option<()> {
        if literal_fields.len() != struct_type_fields.len() {
            let struct_type_fields = struct_type_fields.clone();
            let literal_field_types = self.get_field_types(literal_fields);

            self.errors.push(TypeError::FieldsMismatch {
                expected_fields: struct_type_fields,
                received_fields: literal_field_types,
                span: struct_span,
            });
            return None;
        }

        for (field_name, field_value) in literal_fields {
            let expr_ty = self.check_expr(field_value)?;
            if let Some(expected_ty) = struct_type_fields.get(&field_name.0) {
                if &expr_ty != expected_ty {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: expected_ty.clone(),
                        received_ty: expr_ty.clone(),
                        span: field_value.1.clone(),
                    });
                }
            } else {
                self.errors.push(TypeError::MissingField {
                    struct_name: struct_name.to_string(),
                    field_name: field_name.0.clone(),
                    span: field_name.1.clone(),
                });
            }
        }

        Some(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_program(code: &str) -> Program {
        let (program, _) = crate::parse(code);

        program.expect("Failed to parse program")
    }

    #[test]
    fn test_check_expr() {
        let mut checker = TypeChecker::new();
        let expr = Span(
            Expr::Binary(
                Span(BinaryOp::Add, 3..4),
                Box::new(Span(Expr::Value(Value::I32(1)), 0..1)),
                Box::new(Span(Expr::Value(Value::I32(2)), 6..7)),
            ),
            0..7,
        );

        assert_eq!(checker.check_expr(&expr), Some(Type::I32));
        insta::assert_yaml_snapshot!(checker.errors);

        let expr = Span(
            Expr::Binary(
                Span(BinaryOp::Add, 3..4),
                Box::new(Span(Expr::Value(Value::I32(1)), 0..1)),
                Box::new(Span(Expr::Value(Value::Bool(true)), 6..9)),
            ),
            0..9,
        );

        assert_eq!(checker.check_expr(&expr), None);
        insta::assert_yaml_snapshot!(checker.errors);
    }

    #[test]
    fn test_variable_scopes() {
        let checker = TypeChecker::new();
        let program = parse_program(
            "
        let x = 1;
        let y = 2;
        let z = if true { x } else { y }
        z + y;",
        );

        insta::assert_yaml_snapshot!(checker.check(&program));

        let checker = TypeChecker::new();
        let program = parse_program(
            "
        let x = 1;
        let y = 2;
        let z = if true { a } else { b }
        c + y;
        ",
        );

        insta::assert_yaml_snapshot!(checker.check(&program))
    }

    #[test]
    fn test_function_hoisting() {
        let checker = TypeChecker::new();
        let program = parse_program(
            "
        fn f() -> i32 { 1 }
        let x = f();
        ",
        );

        insta::assert_yaml_snapshot!(checker.check(&program));

        let checker = TypeChecker::new();
        let program = parse_program("let x = f();");

        insta::assert_yaml_snapshot!(checker.check(&program));
    }
}
