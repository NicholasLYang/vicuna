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
    BinaryOp, Expr, ExprBlock, ExprFields, Fields, Function, ImportType, MatchBindings, PostFix,
    Program, Span, Stmt, TypeDeclaration, TypeFields, TypeParams, TypeSig, UnaryOp, Value,
};
use miette::Diagnostic;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::mem;
use std::ops::Range;
use std::sync::Arc;
use thiserror::Error;
use tracing::debug;

// TODO: Intern strings
type Name = String;

#[derive(Clone, PartialEq, Serialize)]
pub enum Type {
    I32,
    F32,
    Bool,
    Void,
    String,
    /// Generic types
    Variable {
        name: Name,
        /// Index into type variables vector
        idx: usize,
    },
    /// A type for JS values
    Js,
    Array(Box<Type>),
    Function {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
    Struct {
        // Structs can be thought of as an instantiation of a schema,
        // where a schema is the fully generic struct. For instance:
        //
        // Given this type declaration:
        // ```
        // struct Box<T> {
        //  inner: T
        // }
        // ```
        //
        // Box<i32> is a struct, but Box<T> is a schema.
        schema: Arc<StructSchema>,
        type_arguments: Vec<Type>,
    },
    Enum {
        schema: Arc<EnumSchema>,
        type_arguments: Vec<Type>,
    },
}

#[derive(Clone, PartialEq, Serialize)]
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

impl Display for FieldsSchema {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Debug for FieldsSchema {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldsSchema::Named(fields) => {
                write!(f, "{{ ")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, ty)?;
                }
                write!(f, " }}")
            }
            FieldsSchema::Tuple(fields) => {
                write!(f, "(")?;
                for (i, ty) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            FieldsSchema::Empty => Ok(()),
        }
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
            Type::Variable(name, idx) => write!(f, "{}@{}", name, idx),
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
            Type::Struct {
                schema,
                type_arguments,
            } => {
                write!(f, "struct {}<", schema.name)?;
                for (i, type_argument) in type_arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", type_argument)?;
                }
                write!(f, ">")?;

                write!(f, " {{ ")?;
                write!(f, "{}", schema.fields)?;
                write!(f, " }}")
            }
            Type::Enum {
                schema,
                type_arguments,
            } => {
                write!(f, "enum {}<", schema.name)?;
                for (i, type_argument) in type_arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", type_argument)?;
                }
                write!(f, ">")?;

                write!(f, " {{ ")?;
                for (i, (variant_name, _variant)) in schema.variants.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", variant_name)?;
                }
                write!(f, " }}")
            }
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

    #[allow(dead_code)]
    fn lookup_mut(&mut self, name: &Name) -> Option<&mut T> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(ty) = scope.get_mut(name) {
                return Some(ty);
            }
        }

        None
    }
}

#[derive(Clone, PartialEq, Serialize)]
pub enum FieldsSchema {
    Tuple(Vec<Type>),
    Named(HashMap<Name, Type>),
    Empty,
}

impl FieldsSchema {
    fn is_empty(&self) -> bool {
        match self {
            FieldsSchema::Empty => true,
            FieldsSchema::Tuple(fields) => fields.is_empty(),
            FieldsSchema::Named(fields) => fields.is_empty(),
        }
    }

    fn len(&self) -> usize {
        match self {
            FieldsSchema::Empty => 0,
            FieldsSchema::Tuple(fields) => fields.len(),
            FieldsSchema::Named(fields) => fields.len(),
        }
    }
}

#[derive(Debug, PartialEq, Serialize)]
pub struct StructSchema {
    name: Name,
    fields: FieldsSchema,
    // Generic parameters used in struct
    type_parameters: Vec<Name>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct EnumSchema {
    name: Name,
    variants: HashMap<Name, Arc<StructSchema>>,
    type_parameters: Vec<Name>,
}

/// We have multiple symbols that need to be tracked and scoped accordingly.
#[derive(Debug, Clone)]
enum SymbolTableEntry {
    /// Normal variables like parameters or let bindings
    Variable { ty: Type },
    /// Generic type variables like struct Foo<T>
    TypeVariable {
        /// Index into the `type_variables` vector
        idx: usize,
    },
    /// Structs
    Struct { schema: Arc<StructSchema> },
    /// Enums
    Enum { schema: Arc<EnumSchema> },
}

pub struct TypeChecker {
    symbol_table: SymbolTable<SymbolTableEntry>,
    type_variables: Vec<Option<Type>>,
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
        expected_fields: FieldsSchema,
        received_fields: Fields<InferredType>,
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
    #[error("Type parameter {name} is not used anywhere")]
    UnusedTypeParameter {
        name: Name,
        #[label]
        span: Range<usize>,
    },
    #[error("expected {expected} generic arguments but {received} arguments were supplied")]
    TypeArityMismatch { expected: usize, received: usize },
    #[error("Trying to use a {pattern} pattern to match against {ty}")]
    WrongPattern {
        pattern: &'static str,
        ty: &'static str,
        #[label]
        span: Range<usize>,
    },
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            type_variables: Vec::new(),
            return_type: None,
            errors: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.symbol_table.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.symbol_table.exit_scope();
    }

    pub fn check(mut self, program: &Program) -> Vec<TypeError> {
        debug!("checking program");
        self.check_block(&program.statements);

        self.errors
    }

    fn add_type_parameters(&mut self, type_parameters: &Option<TypeParams>) {
        if let Some(params) = type_parameters {
            for param in &params.0 {
                self.type_variables.push(None);
                let idx = self.type_variables.len() - 1;
                self.symbol_table
                    .insert(param.0.clone(), SymbolTableEntry::TypeVariable { idx })
            }
        }
    }

    fn check_type_sig(&mut self, type_sig: &Span<TypeSig>) -> Option<Type> {
        match &type_sig.0 {
            TypeSig::I32 => Some(Type::I32),
            TypeSig::F32 => Some(Type::F32),
            TypeSig::Bool => Some(Type::Bool),
            TypeSig::String => Some(Type::String),
            TypeSig::Named(name, type_args) => match self.symbol_table.lookup(&name.0) {
                Some(SymbolTableEntry::Struct { schema })
                    if schema.type_parameters.len() == type_args.len() =>
                {
                    Some(Type::Struct {
                        schema: schema.clone(),
                        type_arguments: type_args
                            .iter()
                            .map(|type_sig| self.check_type_sig(type_sig))
                            .collect::<Option<Vec<_>>>()?,
                    })
                }
                Some(SymbolTableEntry::Enum { schema })
                    if schema.type_parameters.len() == type_args.len() =>
                {
                    Some(Type::Enum {
                        schema: schema.clone(),
                        type_arguments: type_args
                            .iter()
                            .map(|type_sig| self.check_type_sig(type_sig))
                            .collect::<Option<Vec<_>>>()?,
                    })
                }
                Some(SymbolTableEntry::Enum { schema }) => {
                    self.errors.push(TypeError::TypeArityMismatch {
                        expected: schema.type_parameters.len(),
                        received: type_args.len(),
                    });

                    None
                }
                Some(SymbolTableEntry::Struct { schema }) => {
                    self.errors.push(TypeError::TypeArityMismatch {
                        expected: schema.type_parameters.len(),
                        received: type_args.len(),
                    });

                    None
                }
                Some(SymbolTableEntry::TypeVariable { idx }) => Some(
                    self.type_variables[*idx]
                        .clone()
                        .unwrap_or_else(|| Type::Variable {
                            name: name.0.clone(),
                            idx,
                        }),
                ),
                _ => {
                    self.errors
                        .push(TypeError::UndefinedType(name.0.clone(), type_sig.1.clone()));

                    None
                }
            },
        }
    }

    fn unify(&mut self, t1: Type, t2: Type) -> bool {
        match (t1, t2) {
            (Type::Variable { .. }, Type::Variable { .. }) => todo!("too fancy, will handle later"),
            (
                Type::Variable {
                    name: t1_name,
                    idx: t1_idx,
                },
                t2,
            ) => {
                let var = self.type_variables.get_mut(t1_idx).unwrap();
            }
        }
    }

    fn add_type_fields(&mut self, fields: &TypeFields) -> Option<FieldsSchema> {
        match fields {
            TypeFields::Named(fields) => {
                let mut fields_with_types = HashMap::new();
                for (name, type_sig) in fields {
                    let ty = self.check_type_sig(type_sig)?;
                    fields_with_types.insert(name.0.clone(), ty);
                }

                Some(FieldsSchema::Named(fields_with_types))
            }
            TypeFields::Tuple(fields) => {
                let mut types = Vec::new();
                for type_sig in fields {
                    let ty = self.check_type_sig(type_sig)?;
                    types.push(ty);
                }

                Some(FieldsSchema::Tuple(types))
            }
            TypeFields::Empty => Some(FieldsSchema::Empty),
        }
    }

    fn add_type_declaration(&mut self, type_declaration: &TypeDeclaration) -> Option<()> {
        match type_declaration {
            TypeDeclaration::Struct {
                name,
                type_parameters,
                fields,
            } => {
                self.add_type_parameters(type_parameters);
                let fields = self.add_type_fields(fields)?;

                let schema = StructSchema {
                    name: name.0.clone(),
                    fields,
                    type_parameters: type_parameters
                        .as_ref()
                        .map(|params| params.0.iter().map(|param| param.0.clone()).collect())
                        .unwrap_or_default(),
                };

                self.symbol_table.insert(
                    name.0.clone(),
                    SymbolTableEntry::Struct {
                        schema: Arc::new(schema),
                    },
                );
            }
            TypeDeclaration::Enum {
                name,
                type_parameters,
                variants,
            } => {
                self.add_type_parameters(type_parameters);

                let mut variants_map = HashMap::new();
                for (variant_name, fields) in variants {
                    let fields = self.add_type_fields(fields)?;

                    variants_map.insert(
                        variant_name.0.clone(),
                        Arc::new(StructSchema {
                            name: variant_name.0.clone(),
                            fields,
                            type_parameters: type_parameters
                                .as_ref()
                                .map(|params| {
                                    params.0.iter().map(|param| param.0.clone()).collect()
                                })
                                .unwrap_or_default(),
                        }),
                    );
                }

                let schema = Arc::new(EnumSchema {
                    name: name.0.clone(),
                    variants: variants_map,
                    type_parameters: type_parameters
                        .as_ref()
                        .map(|params| params.0.iter().map(|param| param.0.clone()).collect())
                        .unwrap_or_default(),
                });

                self.symbol_table
                    .insert(name.0.clone(), SymbolTableEntry::Enum { schema });
            }
        }

        Some(())
    }

    fn check_block(&mut self, stmts: &[Span<Stmt>]) -> Option<()> {
        for stmt in stmts {
            if let Stmt::Function(Function {
                name,
                return_type,
                params,
                type_parameters,
                ..
            }) = &stmt.0
            {
                let return_type = if let Some(return_type) = return_type {
                    self.check_type_sig(return_type)?
                } else {
                    Type::Void
                };

                self.add_type_parameters(type_parameters);

                let ty = Type::Function {
                    param_types: params
                        .iter()
                        .map(|(_, ty)| self.check_type_sig(ty))
                        .collect::<Option<Vec<_>>>()?,
                    return_type: Box::new(return_type),
                };
                self.symbol_table
                    .insert(name.0.clone(), SymbolTableEntry::Variable { ty });
            }
        }

        for stmt in stmts {
            self.check_stmt(stmt);
        }

        Some(())
    }

    fn check_stmt(&mut self, stmt: &Span<Stmt>) -> Option<()> {
        match &stmt.0 {
            Stmt::Let(name, rhs) => {
                let ty = self.check_expr(rhs)?;
                self.symbol_table
                    .insert(name.0.clone(), SymbolTableEntry::Variable { ty });
            }
            Stmt::Expr(expr) => {
                self.check_expr(expr)?;
            }
            Stmt::Function(Function {
                name,
                type_parameters,
                params,
                body,
                return_type,
            }) => {
                debug!("checking function {}", name.0);
                self.enter_scope();
                self.add_type_parameters(type_parameters);

                for (name, type_sig) in params {
                    let ty = self.check_type_sig(type_sig)?;
                    self.symbol_table
                        .insert(name.0.clone(), SymbolTableEntry::Variable { ty });
                }

                let return_type = if let Some(return_type) = &return_type {
                    self.check_type_sig(return_type)?
                } else {
                    Type::Void
                };

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
                    self.symbol_table.insert(
                        default_import.0.clone(),
                        SymbolTableEntry::Variable { ty: Type::Js },
                    );
                }

                for name in named_imports {
                    self.symbol_table
                        .insert(name.0.clone(), SymbolTableEntry::Variable { ty: Type::Js });
                }
            }
            Stmt::Type(decl) => {
                self.add_type_declaration(decl);
            }
            Stmt::Use { module, name } => {
                let Some(SymbolTableEntry::Enum { schema }) = self.symbol_table.lookup(&module.0).cloned() else {
                    self.errors.push(TypeError::NotEnum(module.0.clone(), stmt.1.clone()));
                    return None;
                };

                if &name.0 == "*" {
                    for (variant_name, variant) in &schema.variants {
                        self.symbol_table.insert(
                            variant_name.clone(),
                            SymbolTableEntry::Struct {
                                schema: variant.clone(),
                            },
                        );
                    }
                } else {
                    let Some(variant) = schema.variants.get(&name.0) else {
                        self.errors.push(TypeError::UndefinedVariant {
                            enum_name: module.0.clone(),
                            variant_name: name.0.clone(),
                            span: stmt.1.clone(),
                        });
                        return None;
                    };

                    self.symbol_table.insert(
                        name.0.clone(),
                        SymbolTableEntry::Struct {
                            schema: variant.clone(),
                        },
                    );
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
                if let Some(SymbolTableEntry::Variable { ty }) = self.symbol_table.lookup(name) {
                    Some(ty.clone())
                } else {
                    self.errors
                        .push(TypeError::UndefinedVariable(name.clone(), expr.1.clone()));
                    None
                }
            }
            Expr::PostFix(callee, Span(PostFix::Args(args), args_span)) => {
                if let Expr::Variable(name) = &callee.0 {
                    if let Some(SymbolTableEntry::Struct { schema }) =
                        self.symbol_table.lookup(name).cloned()
                    {
                        if let FieldsSchema::Tuple(schema_fields) = &schema.fields {
                            let mut instantiated_variables = HashMap::new();
                            self.compare_tuple_fields(
                                &mut instantiated_variables,
                                args,
                                schema_fields,
                                args_span.clone(),
                            )?;
                            let type_arguments = self.get_type_arguments(
                                &instantiated_variables,
                                &schema,
                                expr.1.clone(),
                            )?;

                            return Some(Type::Struct {
                                schema: schema.clone(),
                                type_arguments,
                            });
                        }
                    }
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
                let Type::Struct {
                    schema,
                    type_arguments,
                } = callee_ty else {
                    self.errors
                        .push(TypeError::NotStruct(callee_ty, callee.1.clone()));
                    return None;
                };
                let field_type = match &schema.fields {
                    FieldsSchema::Tuple(_) => {
                        // Unclear what I should do here. I could allow for number fields,
                        // like in Rust, e.g. `foo.1`, `bar.0.2`, but that leads to parsing
                        // ambiguity in the case of `foo.1.2` where you have to explicitly
                        // not parse it as `foo.(1.2)` but as `(foo.1).2`.
                        // I could also use indexing like `foo[0]`. That might be preferable
                        // and honestly better looking IMO.
                        todo!()
                    }
                    FieldsSchema::Named(fields) => fields.get(&field.0),
                    FieldsSchema::Empty => None,
                };

                match field_type {
                    Some(Type::Variable(var_name)) => {
                        let idx = schema
                            .type_parameters
                            .iter()
                            .position(|name| name == var_name)
                            .expect("type var should be in type parameters");

                        Some(type_arguments[idx].clone())
                    }
                    Some(ty) => Some(ty.clone()),
                    None => {
                        self.errors.push(TypeError::UndefinedField {
                            struct_name: schema.name.clone(),
                            field_name: field.0.clone(),
                            span: field_span.clone(),
                        });

                        None
                    }
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
                let Some(SymbolTableEntry::Struct { schema }) = self.symbol_table.lookup(&name.0).cloned() else {
                    self.errors.push(TypeError::UndefinedType(name.0.clone(), expr.1.clone()));
                    return None;
                };

                let type_arguments =
                    self.compare_fields(&name.0, expr.1.clone(), literal_fields, &schema)?;

                Some(Type::Struct {
                    schema: schema.clone(),
                    type_arguments,
                })
            }
            Expr::Enum {
                enum_name,
                variant_name,
                fields,
            } => {
                let Some(SymbolTableEntry::Enum { schema }) = self.symbol_table.lookup(&enum_name.0) .cloned() else {
                    self.errors.push(TypeError::UndefinedType(enum_name.0.clone(), expr.1.clone()));
                    return None;
                };

                let Some(variant_fields) = schema.variants.get(&variant_name.0) else {
                    self.errors.push(TypeError::UndefinedVariant {
                        enum_name: enum_name.0.clone(),
                        variant_name: variant_name.0.clone(),
                        span: variant_name.1.clone(),
                    });
                    return None;
                };

                let type_arguments =
                    self.compare_fields(&variant_name.0, expr.1.clone(), fields, &variant_fields)?;

                Some(Type::Enum {
                    schema: schema.clone(),
                    type_arguments,
                })
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
                let Type::Enum { schema, .. } = expr_ty else {
                    self.errors.push(TypeError::NotEnum(expr_ty.to_string(), expr.1.clone()));
                    return None;
                };

                let enum_name = &schema.name;
                // Type of each case block must be the same
                let mut case_type = None;

                for (case, block) in cases {
                    let Some(variant_schema) = schema.variants.get(&case.0.variant_name.0).cloned() else {
                        self.errors.push(TypeError::UndefinedVariant {
                            enum_name: schema.name.clone(),
                            variant_name: case.0.variant_name.0.clone(),
                            span: case.0.variant_name.1.clone(),
                        });
                        return None;
                    };

                    let Some(received_fields) = &case.0.fields else {
                        if !variant_schema.fields.is_empty() {
                            self.errors.push(TypeError::FieldsMismatch {
                                expected_fields: variant_schema.fields.clone(),
                                received_fields: Fields::Empty,
                                span: case.0.variant_name.1.clone(),
                            });
                        }
                        continue;
                    };

                    // TODO: Handle rest expressions
                    if variant_schema.fields.len() != received_fields.len() {
                        let received_fields = match received_fields {
                            MatchBindings::Tuple(fields) => Fields::Tuple(
                                fields
                                    .into_iter()
                                    .map(|field| Span(InferredType::Unknown, field.1.clone()))
                                    .collect(),
                            ),
                            MatchBindings::Named(fields) => Fields::Named(
                                fields
                                    .into_iter()
                                    .map(|(name, _)| {
                                        (name.clone(), Span(InferredType::Unknown, name.1.clone()))
                                    })
                                    .collect(),
                            ),
                        };

                        self.errors.push(TypeError::FieldsMismatch {
                            expected_fields: variant_schema.fields.clone(),
                            received_fields,
                            span: case.0.variant_name.1.clone(),
                        });
                    }

                    self.symbol_table.enter_scope();

                    match received_fields {
                        MatchBindings::Tuple(fields) => {
                            let FieldsSchema::Tuple(expected_fields) = &variant_schema.fields else {
                                let ty = if matches!(variant_schema.fields, FieldsSchema::Named(_)) {
                                    "named"
                                } else {
                                    "empty"
                                };

                                self.errors.push(TypeError::WrongPattern {
                                    pattern: "tuple",
                                    ty,
                                    span: Default::default(),
                                });
                                return None;
                            };

                            for (idx, field) in fields.into_iter().enumerate() {
                                if let Some(expected_field_ty) = expected_fields.get(idx) {
                                    self.symbol_table.insert(
                                        field.0.clone(),
                                        SymbolTableEntry::Variable {
                                            ty: expected_field_ty.clone(),
                                        },
                                    );
                                } else {
                                    self.errors.push(TypeError::UndefinedField {
                                        struct_name: format!(
                                            "{}::{}",
                                            enum_name, case.0.variant_name.0
                                        ),
                                        field_name: field.0.clone(),
                                        span: field.1.clone(),
                                    });
                                }
                            }
                        }
                        MatchBindings::Named(fields) => {
                            let FieldsSchema::Named(expected_fields) = &variant_schema.fields else {
                                let ty = if matches!(variant_schema.fields, FieldsSchema::Tuple(_)) {
                                    "tuple"
                                } else {
                                    "empty"
                                };

                                self.errors.push(TypeError::WrongPattern {
                                    pattern: "named",
                                    ty,
                                    span: Default::default(),
                                });
                                return None;
                            };

                            for (field, rename) in fields {
                                if let Some(expected_field_ty) = expected_fields.get(&field.0) {
                                    let name = rename.as_ref().unwrap_or(&field).0.clone();
                                    self.symbol_table.insert(
                                        name,
                                        SymbolTableEntry::Variable {
                                            ty: expected_field_ty.clone(),
                                        },
                                    );
                                } else {
                                    self.errors.push(TypeError::UndefinedField {
                                        struct_name: format!(
                                            "{}::{}",
                                            enum_name, case.0.variant_name.0
                                        ),
                                        field_name: field.0.clone(),
                                        span: field.1.clone(),
                                    });
                                }
                            }
                        }
                    }

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
    fn get_field_types(&mut self, fields: &ExprFields) -> Fields<InferredType> {
        fields.map(|expr| match self.check_expr(expr) {
            Some(ty) => Span(InferredType::Known(ty), expr.1.clone()),
            None => Span(InferredType::Unknown, expr.1.clone()),
        })
    }

    fn instantiate_variable(
        &mut self,
        var_name: &str,
        expr_ty: Type,
        expr_span: Range<usize>,
        instantiated_variables: &mut HashMap<Name, Type>,
    ) {
        if let Some(instantiated_ty) = instantiated_variables.get(var_name) {
            if &expr_ty != instantiated_ty {
                self.errors.push(TypeError::TypeMismatch {
                    expected_ty: instantiated_ty.clone(),
                    received_ty: expr_ty.clone(),
                    span: expr_span,
                });
            }
        } else {
            instantiated_variables.insert(var_name.to_string(), expr_ty);
        }
    }

    fn compare_tuple_fields(
        &mut self,
        instantiated_variables: &mut HashMap<Name, Type>,
        tuple_entries: &[Span<Expr>],
        schema_fields: &[Type],
        struct_span: Range<usize>,
    ) -> Option<()> {
        for (i, entry) in tuple_entries.iter().enumerate() {
            let expr_ty = self.check_expr(entry)?;
            match schema_fields.get(i) {
                Some(Type::Variable(name)) => {
                    self.instantiate_variable(
                        &name,
                        expr_ty,
                        entry.1.clone(),
                        instantiated_variables,
                    );
                }
                Some(expected_ty) => {
                    if &expr_ty != expected_ty {
                        self.errors.push(TypeError::TypeMismatch {
                            expected_ty: expected_ty.clone(),
                            received_ty: expr_ty.clone(),
                            span: entry.1.clone(),
                        });
                    }
                }
                None => {
                    self.errors.push(TypeError::ArityMismatch(
                        schema_fields.len(),
                        tuple_entries.len(),
                        struct_span.clone(),
                    ));
                }
            }
        }

        Some(())
    }

    fn get_type_arguments(
        &mut self,
        instantiated_variables: &HashMap<Name, Type>,
        schema: &StructSchema,
        struct_span: Range<usize>,
    ) -> Option<Vec<Type>> {
        let mut type_arguments = vec![];
        for type_param in &schema.type_parameters {
            let Some(type_arg) = instantiated_variables.get(type_param) else {
                self.errors.push(TypeError::UnusedTypeParameter {
                    name: type_param.clone(),
                    span: struct_span,
                });
                return None
            };
            type_arguments.push(type_arg.clone());
        }

        Some(type_arguments)
    }

    fn compare_fields(
        &mut self,
        struct_name: &str,
        struct_span: Range<usize>,
        literal_fields: &ExprFields,
        struct_schema: &StructSchema,
    ) -> Option<Vec<Type>> {
        if literal_fields.len() != struct_schema.fields.len() {
            let struct_type_fields = struct_schema.fields.clone();
            let literal_field_types = self.get_field_types(literal_fields);

            self.errors.push(TypeError::FieldsMismatch {
                expected_fields: struct_type_fields,
                received_fields: literal_field_types,
                span: struct_span,
            });
            return None;
        }

        // We keep track of the instantiated variables to make sure
        // we don't have conflicting instantiations.
        let mut instantiated_variables = HashMap::new();

        match (literal_fields, &struct_schema.fields) {
            (Fields::Named(literal_fields), FieldsSchema::Named(schema_fields)) => {
                for (field_name, field_value) in literal_fields {
                    let expr_ty = self.check_expr(field_value)?;
                    match schema_fields.get(&field_name.0) {
                        Some(Type::Variable(name)) => {
                            self.instantiate_variable(
                                name,
                                expr_ty,
                                field_value.1.clone(),
                                &mut instantiated_variables,
                            );
                        }
                        Some(expected_ty) => {
                            if &expr_ty != expected_ty {
                                self.errors.push(TypeError::TypeMismatch {
                                    expected_ty: expected_ty.clone(),
                                    received_ty: expr_ty.clone(),
                                    span: field_value.1.clone(),
                                });
                            }
                        }
                        None => {
                            self.errors.push(TypeError::MissingField {
                                struct_name: struct_name.to_string(),
                                field_name: field_name.0.clone(),
                                span: field_name.1.clone(),
                            });
                        }
                    }
                }
            }
            (Fields::Tuple(entries), FieldsSchema::Tuple(schema_fields)) => {
                self.compare_tuple_fields(
                    &mut instantiated_variables,
                    entries,
                    schema_fields,
                    struct_span.clone(),
                )?;
            }
            (Fields::Empty, FieldsSchema::Empty) => {}
            _ => {
                let received_fields = self.get_field_types(literal_fields);
                self.errors.push(TypeError::FieldsMismatch {
                    expected_fields: struct_schema.fields.clone(),
                    received_fields,
                    span: struct_span,
                });
                return None;
            }
        }

        let type_arguments =
            self.get_type_arguments(&instantiated_variables, struct_schema, struct_span)?;
        Some(type_arguments)
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
