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
    Program, Span, Stmt, TypeDeclaration, TypeFields, TypeSig, UnaryOp, Value,
};
use crate::symbol_table::{SymbolTable, SymbolTableEntry};
use id_arena::{Arena, Id};
use miette::Diagnostic;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;
use std::sync::Arc;
use std::{fmt, mem};
use thiserror::Error;
use tracing::{debug, instrument};

// TODO: Intern strings
pub type Name = String;

pub type TypeId = Id<Type>;
pub type StructSchemaId = Id<StructSchema>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I32,
    F32,
    Bool,
    Void,
    String,
    /// Generic types
    Variable(Name),
    /// A type for JS values
    Js,
    Array(Box<TypeId>),
    Function {
        type_parameters: Vec<Name>,
        param_types: Vec<TypeId>,
        return_type: Box<TypeId>,
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
        schema_id: StructSchemaId,
        type_arguments: Vec<TypeId>,
    },
    Enum {
        schema: Arc<EnumSchema>,
        type_arguments: Vec<TypeId>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum InferredType {
    Known(TypeId),
    Unknown,
}

impl DisplayWithArena<'_> for InferredType {
    fn fmt_with_arena(
        &self,
        f: &mut Formatter<'_>,
        types: &Arena<Type>,
        struct_schemas: &Arena<StructSchema>,
    ) -> fmt::Result {
        match self {
            InferredType::Known(ty) => ty.fmt_with_arena(f, types, struct_schemas),
            InferredType::Unknown => write!(f, "<unknown type>"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// A hack to get Display working on arena allocated values
struct WithArena<'a, T: DisplayWithArena<'a>> {
    value: T,
    types: &'a Arena<Type>,
    struct_schemas: &'a Arena<StructSchema>,
}

impl<T: for<'a> DisplayWithArena<'a>> Display for WithArena<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.value
            .fmt_with_arena(f, self.types, self.struct_schemas)
    }
}

trait DisplayWithArena<'a> {
    fn fmt_with_arena(
        &self,
        f: &mut Formatter<'a>,
        types: &Arena<Type>,
        struct_schemas: &Arena<StructSchema>,
    ) -> fmt::Result;
}

impl DisplayWithArena<'_> for Id<Type> {
    fn fmt_with_arena(
        &self,
        f: &mut Formatter<'_>,
        types: &Arena<Type>,
        struct_schemas: &Arena<StructSchema>,
    ) -> fmt::Result {
        types[*self].fmt_with_arena(f, types, struct_schemas)
    }
}

impl DisplayWithArena<'_> for FieldsSchema {
    fn fmt_with_arena(
        &self,
        f: &mut Formatter<'_>,
        types: &Arena<Type>,
        struct_schemas: &Arena<StructSchema>,
    ) -> fmt::Result {
        match self {
            FieldsSchema::Named(fields) => {
                write!(f, "{{ ")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ", name)?;
                    ty.fmt_with_arena(f, types, struct_schemas)?;
                }
                write!(f, " }}")
            }
            FieldsSchema::Tuple(fields) => {
                write!(f, "(")?;
                for (i, ty) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    ty.fmt_with_arena(f, types, struct_schemas)?;
                }
                write!(f, ")")
            }
            FieldsSchema::Empty => Ok(()),
        }
    }
}

impl DisplayWithArena<'_> for Type {
    fn fmt_with_arena(
        &self,
        f: &mut Formatter<'_>,
        types: &Arena<Type>,
        struct_schemas: &Arena<StructSchema>,
    ) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::String => write!(f, "string"),
            Type::Js => write!(f, "<js value>"),
            Type::Variable(name) => write!(f, "{}", name),
            Type::Array(ty) => write!(f, "{}[]", types[**ty]),
            Type::Function {
                param_types,
                return_type,
                type_parameters,
            } => {
                if !type_parameters.is_empty() {
                    write!(f, "<")?;
                    for (i, type_parameter) in type_parameters.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", type_parameter)?;
                    }
                    write!(f, ">")?;
                }
                write!(f, "(")?;
                for (i, param_type) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", types[*param_type])?;
                }
                write!(f, ") -> {}", types[**return_type])
            }
            Type::Struct {
                schema_id: schema,
                type_arguments,
            } => {
                let schema = &struct_schemas[*schema];
                write!(f, "struct {}<", schema.name)?;
                for (i, type_argument) in type_arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    types[*type_argument].fmt_with_arena(f, types, struct_schemas)?;
                }
                write!(f, ">")?;

                write!(f, " {{ ")?;
                schema.fields.fmt_with_arena(f, types, struct_schemas)?;
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
                    write!(f, "{}", &types[*type_argument])?;
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

#[derive(Debug, Clone, PartialEq)]
pub enum FieldsSchema {
    Tuple(Vec<TypeId>),
    Named(HashMap<Name, TypeId>),
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

#[derive(Debug, Clone, PartialEq)]
pub struct StructSchema {
    name: Name,
    fields: FieldsSchema,
    // Generic parameters used in struct
    type_parameters: Vec<Name>,
}

#[derive(Debug, PartialEq)]
pub struct EnumSchema {
    name: Name,
    variants: HashMap<Name, StructSchemaId>,
    type_parameters: Vec<Name>,
}

pub struct TypeChecker {
    symbol_table: SymbolTable,
    type_variables: Vec<Option<TypeId>>,
    types: Arena<Type>,
    struct_schemas: Arena<StructSchema>,
    return_type: Option<TypeId>,
    pub(crate) errors: Vec<TypeError>,
    i32_ty: TypeId,
    f32_ty: TypeId,
    bool_ty: TypeId,
    void_ty: TypeId,
    string_ty: TypeId,
    js_ty: TypeId,
}

impl Debug for TypeChecker {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeChecker").finish()
    }
}

#[derive(Debug, Clone, PartialEq, Diagnostic, Error)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected_ty}, got {received_ty}")]
    #[diagnostic(code(type_error::type_mismatch))]
    TypeMismatch {
        expected_ty: String,
        received_ty: String,
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
        let mut type_arena = Arena::new();
        let i32_ty = type_arena.alloc(Type::I32);
        let f32_ty = type_arena.alloc(Type::F32);
        let bool_ty = type_arena.alloc(Type::Bool);
        let void_ty = type_arena.alloc(Type::Void);
        let string_ty = type_arena.alloc(Type::String);
        let js_ty = type_arena.alloc(Type::Js);

        Self {
            symbol_table: SymbolTable::new(),
            type_variables: Vec::new(),
            types: type_arena,
            struct_schemas: Arena::new(),
            return_type: None,
            errors: Vec::new(),
            i32_ty,
            f32_ty,
            bool_ty,
            void_ty,
            string_ty,
            js_ty,
        }
    }

    // Shorthand for creating a WithArena
    fn p<'a, T: DisplayWithArena<'a>>(&'a self, value: T) -> WithArena<'a, T> {
        WithArena {
            value,
            types: &self.types,
            struct_schemas: &self.struct_schemas,
        }
    }

    fn enter_scope(&mut self) {
        self.symbol_table.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.symbol_table.exit_scope();
    }

    #[allow(dead_code)]
    fn print_checker_state(&self) {
        println!("symbol table:");
        println!("{:?}", self.symbol_table);
        println!("type variables:");
        println!("{:?}", self.type_variables);
        println!("return type:");
        if let Some(ty) = &self.return_type {
            println!("{:?}", ty);
        } else {
            println!("None");
        }
        println!("errors:");
        println!("{:?}", self.errors);
    }

    pub fn check(mut self, program: &Program) -> Vec<TypeError> {
        debug!("checking program");
        self.check_block(&program.statements);
        self.errors
    }

    fn add_type_parameters(&mut self, type_parameters: &[String]) {
        for param in type_parameters {
            self.type_variables.push(None);
            let idx = self.type_variables.len() - 1;
            self.symbol_table
                .insert(param.clone(), SymbolTableEntry::TypeVariable { idx })
        }
    }

    fn check_type_sig(&mut self, type_sig: &Span<TypeSig>) -> Option<TypeId> {
        match &type_sig.0 {
            TypeSig::I32 => Some(self.i32_ty),
            TypeSig::F32 => Some(self.f32_ty),
            TypeSig::Bool => Some(self.bool_ty),
            TypeSig::String => Some(self.string_ty),
            TypeSig::Named(name, type_args) => match self.symbol_table.lookup(&name.0) {
                Some(SymbolTableEntry::Struct { schema_id })
                    if self.struct_schemas[*schema_id].type_parameters.len() == type_args.len() =>
                {
                    let ty = Type::Struct {
                        schema_id: *schema_id,
                        type_arguments: type_args
                            .iter()
                            .map(|type_sig| self.check_type_sig(type_sig))
                            .collect::<Option<Vec<_>>>()?,
                    };
                    Some(self.types.alloc(ty))
                }
                Some(SymbolTableEntry::Enum { schema })
                    if schema.type_parameters.len() == type_args.len() =>
                {
                    let ty = Type::Enum {
                        schema: schema.clone(),
                        type_arguments: type_args
                            .iter()
                            .map(|type_sig| self.check_type_sig(type_sig))
                            .collect::<Option<Vec<_>>>()?,
                    };
                    Some(self.types.alloc(ty))
                }
                Some(SymbolTableEntry::Enum { schema }) => {
                    self.errors.push(TypeError::TypeArityMismatch {
                        expected: schema.type_parameters.len(),
                        received: type_args.len(),
                    });

                    None
                }
                Some(SymbolTableEntry::Struct { schema_id }) => {
                    self.errors.push(TypeError::TypeArityMismatch {
                        expected: self.struct_schemas[*schema_id].type_parameters.len(),
                        received: type_args.len(),
                    });

                    None
                }
                Some(SymbolTableEntry::TypeVariable { idx }) => {
                    let ty = self
                        .type_variables
                        .get(*idx)
                        .cloned()
                        .flatten()
                        .unwrap_or_else(|| self.types.alloc(Type::Variable(name.0.clone())));

                    Some(ty)
                }
                Some(SymbolTableEntry::AbstractTypeVariable) => {
                    Some(self.types.alloc(Type::Variable(name.0.clone())))
                }
                _ => {
                    self.errors
                        .push(TypeError::UndefinedType(name.0.clone(), type_sig.1.clone()));

                    None
                }
            },
        }
    }

    /// Checks the fields of a type declaration and returns the schema for the fields if the check
    /// succeeds. Requires the type id to be already allocated so we can do recursive types.
    ///
    /// # Arguments
    ///
    /// * `type_id`: The already allocated type id for the type we are checking.
    /// * `fields`: The unchecked type fields.
    ///
    /// returns: Option<FieldsSchema>
    ///
    /// # Examples
    ///
    /// ```
    ///
    /// ```
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
                let type_parameters: Vec<Name> = type_parameters
                    .as_ref()
                    .map(|params| params.0.iter().map(|param| param.0.clone()).collect())
                    .unwrap_or_default();

                let schema = StructSchema {
                    name: name.0.clone(),
                    fields: FieldsSchema::Empty,
                    type_parameters: type_parameters.clone(),
                };

                let schema_id = self.struct_schemas.alloc(schema);

                self.symbol_table
                    .insert(name.0.clone(), SymbolTableEntry::Struct { schema_id });

                self.symbol_table.enter_scope();

                for param in type_parameters {
                    self.symbol_table
                        .insert(param, SymbolTableEntry::AbstractTypeVariable);
                }

                let fields = self.add_type_fields(fields)?;

                self.symbol_table.exit_scope();

                self.struct_schemas[schema_id].fields = fields;
            }
            TypeDeclaration::Enum {
                name,
                type_parameters,
                variants,
            } => {
                let type_parameters: Vec<_> = type_parameters
                    .iter()
                    .flat_map(|params| params.0.iter())
                    .map(|param| param.0.clone())
                    .collect();

                self.add_type_parameters(&type_parameters);

                let mut variants_map = HashMap::new();
                for (variant_name, fields) in variants {
                    let fields = self.add_type_fields(fields)?;

                    let variant_schema_id = self.struct_schemas.alloc(StructSchema {
                        name: variant_name.0.clone(),
                        fields,
                        type_parameters: type_parameters.clone(),
                    });

                    variants_map.insert(variant_name.0.clone(), variant_schema_id);
                }

                let schema = Arc::new(EnumSchema {
                    name: name.0.clone(),
                    variants: variants_map,
                    type_parameters,
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
                    self.void_ty
                };

                // We enter a scope to add the type parameters;
                self.enter_scope();

                let type_parameters = type_parameters
                    .iter()
                    .flat_map(|p| &p.0)
                    .map(|param| {
                        self.symbol_table
                            .insert(param.0.clone(), SymbolTableEntry::AbstractTypeVariable);
                        param.0.clone()
                    })
                    .collect::<Vec<_>>();

                let fn_ty = Type::Function {
                    param_types: params
                        .iter()
                        .map(|(_, ty)| self.check_type_sig(ty))
                        .collect::<Option<Vec<_>>>()?,
                    return_type: Box::new(return_type),
                    type_parameters,
                };

                let ty = self.types.alloc(fn_ty);

                // Exit it to add the function definition;
                self.exit_scope();

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
                type_parameters: _,
                params,
                body,
                return_type: _,
            }) => {
                debug!("checking function {}", name.0);
                self.enter_scope();

                // The clone here is pretty expensive, so I should probably
                // make it cheaper by interning types.
                let Some(SymbolTableEntry::Variable { ty }) = self.symbol_table.lookup(&name.0).cloned() else {
                    panic!("function should be defined in symbol table");
                };

                let Type::Function {
                    type_parameters,
                    param_types,
                    return_type,
                } = &self.types[ty] else {
                    panic!("function should have function type");
                };

                for type_param in type_parameters {
                    self.symbol_table
                        .insert(type_param.clone(), SymbolTableEntry::AbstractTypeVariable);
                }

                for ((param_name, _), param_ty) in params.iter().zip(param_types.iter()) {
                    self.symbol_table.insert(
                        param_name.0.clone(),
                        SymbolTableEntry::Variable { ty: *param_ty },
                    );
                }

                let old_return_type =
                    mem::replace(&mut self.return_type, Some(*return_type.clone()));

                self.check_block(&body.0.stmts);

                let return_type = mem::replace(&mut self.return_type, old_return_type)
                    .expect("return type should be set");

                if let Some(end_expr) = &body.0.end_expr {
                    let end_expr_ty = self.check_expr(end_expr)?;

                    if end_expr_ty != return_type {
                        self.errors.push(TypeError::TypeMismatch {
                            expected_ty: self.p(return_type).to_string(),
                            received_ty: self.p(end_expr_ty).to_string(),
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
                if condition_ty != self.bool_ty {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: "bool".to_string(),
                        received_ty: self.p(condition_ty).to_string(),
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
                    self.void_ty
                };

                if let Some(return_type) = &self.return_type {
                    if ty != *return_type {
                        self.errors.push(TypeError::TypeMismatch {
                            expected_ty: self.p(*return_type).to_string(),
                            received_ty: self.p(ty).to_string(),
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
                        SymbolTableEntry::Variable { ty: self.js_ty },
                    );
                }

                for name in named_imports {
                    self.symbol_table.insert(
                        name.0.clone(),
                        SymbolTableEntry::Variable { ty: self.js_ty },
                    );
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
                                schema_id: variant.clone(),
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
                            schema_id: variant.clone(),
                        },
                    );
                }
            }
            Stmt::Import { .. } => todo!("internal imports not implemented yet"),
        }

        Some(())
    }

    fn check_expr(&mut self, expr: &Span<Expr>) -> Option<TypeId> {
        debug!("checking expr: {:?}", expr.0);
        match &expr.0 {
            Expr::Binary(op, lhs, rhs) => {
                let lhs_ty = self.check_expr(lhs)?;
                let rhs_ty = self.check_expr(rhs)?;
                match op.0 {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        if lhs_ty == self.i32_ty && rhs_ty == self.i32_ty {
                            Some(self.i32_ty)
                        } else if lhs_ty == self.f32_ty && rhs_ty == self.f32_ty {
                            Some(self.f32_ty)
                        } else {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: self.p(lhs_ty).to_string(),
                                received_ty: self.p(rhs_ty).to_string(),
                                span: rhs.1.clone(),
                            });
                            None
                        }
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        if lhs_ty != rhs_ty {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: self.p(lhs_ty).to_string(),
                                received_ty: self.p(rhs_ty).to_string(),
                                span: rhs.1.clone(),
                            });
                        }

                        Some(self.bool_ty)
                    }
                    BinaryOp::GreaterThan
                    | BinaryOp::GreaterThanOrEqual
                    | BinaryOp::LessThan
                    | BinaryOp::LessThanOrEqual => {
                        let is_number = (lhs_ty == self.i32_ty && rhs_ty == self.i32_ty)
                            || (lhs_ty == self.f32_ty && rhs_ty == self.f32_ty);
                        if !is_number {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: self.p(lhs_ty).to_string(),
                                received_ty: self.p(rhs_ty).to_string(),
                                span: rhs.1.clone(),
                            });
                        }

                        Some(self.bool_ty)
                    }
                }
            }
            Expr::Unary(op, rhs) => {
                let rhs_ty = self.check_expr(rhs)?;
                match op.0 {
                    UnaryOp::Not => {
                        if rhs_ty != self.bool_ty {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: "bool".to_string(),
                                received_ty: self.p(rhs_ty).to_string(),
                                span: expr.1.clone(),
                            });
                        }

                        Some(self.bool_ty)
                    }
                    UnaryOp::Negate => {
                        if rhs_ty != self.i32_ty {
                            self.errors.push(TypeError::TypeMismatch {
                                expected_ty: "i32".to_string(),
                                received_ty: self.p(rhs_ty).to_string(),
                                span: expr.1.clone(),
                            });
                        }

                        Some(self.i32_ty)
                    }
                }
            }
            Expr::Value(value) => match value {
                Value::I32(_) => Some(self.i32_ty),
                Value::Bool(_) => Some(self.bool_ty),
                Value::String(_) => Some(self.string_ty),
                Value::F32(_) => Some(self.f32_ty),
            },
            Expr::Variable(name) => {
                if let Some(SymbolTableEntry::Variable { ty }) = self.symbol_table.lookup(name) {
                    Some(*ty)
                } else {
                    self.errors
                        .push(TypeError::UndefinedVariable(name.clone(), expr.1.clone()));
                    None
                }
            }
            Expr::PostFix(callee, Span(PostFix::Args(args), args_span)) => {
                if let Expr::Variable(name) = &callee.0 {
                    if let Some(SymbolTableEntry::Struct { schema_id }) =
                        self.symbol_table.lookup(name).cloned()
                    {
                        // TODO: Avoid this expensive clone.
                        let schema = self.struct_schemas[schema_id].clone();

                        if let FieldsSchema::Tuple(schema_fields) = &schema.fields {
                            self.add_type_parameters(&schema.type_parameters);

                            self.compare_tuple_fields(args, schema_fields, args_span.clone())?;
                            let type_arguments =
                                self.get_type_arguments(&schema, expr.1.clone())?;

                            return Some(self.types.alloc(Type::Struct {
                                schema_id,
                                type_arguments,
                            }));
                        }
                    }
                    if name == "print" {
                        for arg in args {
                            self.check_expr(arg)?;
                        }
                        return Some(self.void_ty);
                    }
                }

                let callee_ty = self.check_expr(callee)?;
                // TODO: Avoid this expensive clone.
                let callee_ty = &self.types[callee_ty].clone();

                let Type::Function {
                    param_types,
                    return_type,
                    type_parameters
                } = callee_ty else {
                    self.errors
                        .push(TypeError::NotCallable(callee_ty.clone(), callee.1.clone()));
                    return None;
                };

                if args.len() != param_types.len() {
                    self.errors.push(TypeError::ArityMismatch(
                        param_types.len(),
                        args.len(),
                        args_span.clone(),
                    ));
                }

                self.add_type_parameters(type_parameters);

                for (arg, param_type) in args.iter().zip(param_types) {
                    let arg_ty = self.check_expr(arg)?;
                    if let Type::Variable(name) = &self.types[*param_type] {
                        let entry = self
                            .symbol_table
                            .lookup(name)
                            .expect("variable not in symbol table");

                        if let SymbolTableEntry::TypeVariable { idx } = entry {
                            if let Some(existing_ty) = &self.type_variables[*idx] {
                                if existing_ty != &arg_ty {
                                    self.errors.push(TypeError::TypeMismatch {
                                        expected_ty: self.p(*existing_ty).to_string(),
                                        received_ty: self.p(arg_ty).to_string(),
                                        span: arg.1.clone(),
                                    });
                                }
                            } else {
                                self.type_variables[*idx] = Some(arg_ty);
                            }
                        }
                    } else if arg_ty != *param_type {
                        self.errors.push(TypeError::TypeMismatch {
                            expected_ty: self.p(*param_type).to_string(),
                            received_ty: self.p(arg_ty).to_string(),
                            span: arg.1.clone(),
                        });
                    }
                }

                Some(self.apply_substitutions(**return_type))
            }
            Expr::PostFix(callee, Span(PostFix::Field(field), field_span)) => {
                let callee_ty = self.check_expr(callee)?;
                let callee_ty = &self.types[callee_ty];
                debug!("callee type: {:?}", callee);
                let Type::Struct {
                    schema_id,
                    type_arguments,
                } = callee_ty else {
                    self.errors
                        .push(TypeError::NotStruct(callee_ty.clone(), callee.1.clone()));
                    return None;
                };

                let schema = &self.struct_schemas[*schema_id];

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

                let Some(field_type) = field_type else {
                    self.errors.push(TypeError::UndefinedField {
                        struct_name: schema.name.clone(),
                        field_name: field.0.clone(),
                        span: field_span.clone(),
                    });

                    return None
                };

                if let Type::Variable(var_name) = &self.types[*field_type] {
                    let idx = schema
                        .type_parameters
                        .iter()
                        .position(|name| name == var_name)
                        .expect("type var should be in type parameters");

                    Some(type_arguments[idx])
                } else {
                    Some(*field_type)
                }
            }
            Expr::PostFix(callee, Span(PostFix::Index(index), _)) => {
                let callee_ty = self.check_expr(callee)?;
                let index_ty = self.check_expr(index)?;

                let callee_ty = &self.types[callee_ty];

                if index_ty != self.i32_ty {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: "i32".to_string(),
                        received_ty: self.p(index_ty).to_string(),
                        span: index.1.clone(),
                    });
                }

                if let Type::Array(ty) = callee_ty {
                    Some(**ty)
                } else {
                    self.errors
                        .push(TypeError::NotArray(callee_ty.clone(), callee.1.clone()));
                    None
                }
            }
            Expr::Struct(name, literal_fields) => {
                let Some(SymbolTableEntry::Struct { schema_id }) = self.symbol_table.lookup(&name.0).cloned() else {
                    self.errors.push(TypeError::UndefinedType(name.0.clone(), expr.1.clone()));
                    return None;
                };

                // TODO: Avoid this clone.
                let schema = self.struct_schemas[schema_id].clone();
                self.add_type_parameters(&schema.type_parameters);

                let type_arguments =
                    self.check_fields(&name.0, expr.1.clone(), literal_fields, schema_id)?;

                Some(self.types.alloc(Type::Struct {
                    schema_id,
                    type_arguments,
                }))
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

                self.add_type_parameters(&schema.type_parameters);

                let Some(variant_schema_id) = schema.variants.get(&variant_name.0) else {
                    self.errors.push(TypeError::UndefinedVariant {
                        enum_name: enum_name.0.clone(),
                        variant_name: variant_name.0.clone(),
                        span: variant_name.1.clone(),
                    });
                    return None;
                };

                let type_arguments =
                    self.check_fields(&variant_name.0, expr.1.clone(), fields, *variant_schema_id)?;

                Some(self.types.alloc(Type::Enum {
                    schema: schema.clone(),
                    type_arguments,
                }))
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                let condition_ty = self.check_expr(condition)?;
                if condition_ty != self.bool_ty {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: "bool".to_string(),
                        received_ty: self.p(condition_ty).to_string(),
                        span: condition.1.clone(),
                    });
                }

                let then_ty = self.check_expression_block(then_block)?;
                let else_ty = self.check_expression_block(else_block)?;

                if then_ty != else_ty {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: self.p(then_ty).to_string(),
                        received_ty: self.p(else_ty).to_string(),
                        span: expr.1.clone(),
                    });
                }

                Some(then_ty)
            }
            Expr::Match { expr, cases } => {
                let expr_ty = self.check_expr(expr)?;
                // TODO: Handle non-enum pattern matching
                let Type::Enum { schema, .. } = &self.types[expr_ty] else {
                    self.errors.push(TypeError::NotEnum(self.p(expr_ty).to_string(), expr.1.clone()));
                    return None;
                };

                let schema = schema.clone();

                let enum_name = &schema.name;
                // Type of each case block must be the same
                let mut case_type = None;

                for (case, block) in cases {
                    let Some(variant_schema_id) = schema.variants.get(&case.0.variant_name.0).cloned() else {
                        self.errors.push(TypeError::UndefinedVariant {
                            enum_name: schema.name.clone(),
                            variant_name: case.0.variant_name.0.clone(),
                            span: case.0.variant_name.1.clone(),
                        });
                        return None;
                    };

                    let variant_schema = &self.struct_schemas[variant_schema_id];

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
                                    .iter()
                                    .map(|field| Span(InferredType::Unknown, field.1.clone()))
                                    .collect(),
                            ),
                            MatchBindings::Named(fields) => Fields::Named(
                                fields
                                    .iter()
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

                            for (idx, field) in fields.iter().enumerate() {
                                if let Some(expected_field_ty) = expected_fields.get(idx) {
                                    self.symbol_table.insert(
                                        field.0.clone(),
                                        SymbolTableEntry::Variable {
                                            ty: *expected_field_ty,
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
                                    let name = rename.as_ref().unwrap_or(field).0.clone();
                                    self.symbol_table.insert(
                                        name,
                                        SymbolTableEntry::Variable {
                                            ty: *expected_field_ty,
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
                                expected_ty: self.p(*expected_ty).to_string(),
                                received_ty: self.p(ty).to_string(),
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

    fn check_expression_block(&mut self, block: &Span<ExprBlock>) -> Option<TypeId> {
        for stmt in &block.0.stmts {
            self.check_stmt(stmt)?;
        }
        if let Some(expr) = &block.0.end_expr {
            self.check_expr(expr)
        } else {
            Some(self.void_ty)
        }
    }

    // Gets types from the fields of a struct literal. Used to produce an error
    fn get_field_types(&mut self, fields: &ExprFields) -> Fields<InferredType> {
        fields.map(|expr| match self.check_expr(expr) {
            Some(ty) => Span(InferredType::Known(ty), expr.1.clone()),
            None => Span(InferredType::Unknown, expr.1.clone()),
        })
    }

    fn apply_substitutions(&mut self, ty: TypeId) -> TypeId {
        match self.types[ty].clone() {
            Type::Variable(name) => {
                let Some(SymbolTableEntry::TypeVariable { idx }) = self.symbol_table.lookup(&name) else {
                    return ty
                };

                self.type_variables[*idx].unwrap_or(ty)
            }
            Type::Array(inner_ty) => {
                let inner_ty = self.apply_substitutions(*inner_ty);
                self.types.alloc(Type::Array(Box::new(inner_ty)))
            }
            Type::Struct {
                schema_id: schema,
                type_arguments,
            } => {
                let ty = Type::Struct {
                    schema_id: schema,
                    type_arguments: type_arguments
                        .into_iter()
                        .map(|arg| self.apply_substitutions(arg))
                        .collect(),
                };

                self.types.alloc(ty)
            }
            Type::Function {
                param_types,
                return_type,
                type_parameters,
            } => {
                let ty = Type::Function {
                    param_types: param_types
                        .into_iter()
                        .map(|arg| self.apply_substitutions(arg))
                        .collect(),
                    return_type: Box::new(self.apply_substitutions(*return_type)),
                    type_parameters,
                };

                self.types.alloc(ty)
            }
            _ => ty,
        }
    }

    #[instrument]
    fn instantiate_type_variable(
        &mut self,
        type_var_name: &str,
        expr_ty: TypeId,
        expr_span: Range<usize>,
    ) {
        debug!(
            "instantiating type variable `{}` with type {}",
            type_var_name,
            self.p(expr_ty)
        );
        let Some(SymbolTableEntry::TypeVariable { idx }) = self.symbol_table.lookup(type_var_name) else {
            self.errors.push(TypeError::UndefinedType(type_var_name.to_string(), expr_span));
            return;
        };

        if let Some(instantiated_ty) = &self.type_variables[*idx] {
            if &self.types[expr_ty] != &self.types[*instantiated_ty] {
                self.errors.push(TypeError::TypeMismatch {
                    expected_ty: self.p(*instantiated_ty).to_string(),
                    received_ty: self.p(expr_ty).to_string(),
                    span: expr_span,
                });
            }
        } else {
            self.type_variables[*idx] = Some(expr_ty);
        }
    }

    #[instrument]
    fn compare_tuple_fields(
        &mut self,
        tuple_entries: &[Span<Expr>],
        schema_fields: &[TypeId],
        struct_span: Range<usize>,
    ) -> Option<()> {
        for (i, entry) in tuple_entries.iter().enumerate() {
            let expr_ty = self.check_expr(entry)?;
            let Some(expected_ty) = schema_fields.get(i) else {
                self.errors.push(TypeError::ArityMismatch(
                    schema_fields.len(),
                    tuple_entries.len(),
                    struct_span.clone(),
                ));
                continue
            };

            if let Type::Variable(name) = &self.types[*expected_ty] {
                let name = name.clone();
                self.instantiate_type_variable(&name, expr_ty, entry.1.clone());
            } else if expr_ty != *expected_ty {
                self.errors.push(TypeError::TypeMismatch {
                    expected_ty: self.p(*expected_ty).to_string(),
                    received_ty: self.p(expr_ty).to_string(),
                    span: entry.1.clone(),
                });
            }
        }

        Some(())
    }

    fn get_type_arguments(
        &mut self,
        schema: &StructSchema,
        struct_span: Range<usize>,
    ) -> Option<Vec<TypeId>> {
        let mut type_arguments = vec![];
        for type_param in &schema.type_parameters {
            let Some(SymbolTableEntry::TypeVariable { idx }) = self.symbol_table.lookup(type_param) else {
                self.errors.push(TypeError::UndefinedType(type_param.clone(), struct_span));
                return None
            };
            if let Some(type_arg) = &self.type_variables[*idx] {
                type_arguments.push(*type_arg);
            } else {
                self.errors.push(TypeError::UnusedTypeParameter {
                    name: type_param.clone(),
                    span: struct_span,
                });
                return None;
            }
        }

        Some(type_arguments)
    }

    #[instrument]
    fn unify(&mut self, t1: TypeId, t2: TypeId, span: Range<usize>) {
        match (&self.types[t1], &self.types[t2]) {
            (Type::Variable(name), _) => {
                let name = name.clone();
                self.instantiate_type_variable(&name, t1, span);
            }
            (_, Type::Variable(name)) => {
                let name = name.clone();
                self.instantiate_type_variable(&name, t2, span);
            }
            (
                Type::Struct {
                    schema_id: schema_id1,
                    type_arguments: type_arguments1,
                },
                Type::Struct {
                    schema_id: schema_id2,
                    type_arguments: type_arguments2,
                },
            ) => {
                if schema_id1 != schema_id2 {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: self.p(t1).to_string(),
                        received_ty: self.p(t2).to_string(),
                        span: span.clone(),
                    });
                    return;
                }

                let type_arguments1 = type_arguments1.clone();
                let type_arguments2 = type_arguments2.clone();

                for (t1_arg, t2_arg) in type_arguments1.into_iter().zip(type_arguments2) {
                    self.unify(t1_arg, t2_arg, span.clone());
                }
            }
            (_, _) => {
                if t1 != t2 {
                    self.errors.push(TypeError::TypeMismatch {
                        expected_ty: self.p(t1).to_string(),
                        received_ty: self.p(t2).to_string(),
                        span,
                    });
                }
            }
        }
    }

    fn check_fields(
        &mut self,
        struct_name: &str,
        struct_span: Range<usize>,
        literal_fields: &ExprFields,
        struct_schema_id: StructSchemaId,
    ) -> Option<Vec<TypeId>> {
        let struct_schema = self.struct_schemas[struct_schema_id].clone();
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

        match (literal_fields, &struct_schema.fields) {
            (Fields::Named(literal_fields), FieldsSchema::Named(schema_fields)) => {
                for (field_name, field_value) in literal_fields {
                    let expr_ty = self.check_expr(field_value)?;
                    let Some(expected_ty) = schema_fields.get(&field_name.0) else {
                        self.errors.push(TypeError::MissingField {
                            struct_name: struct_name.to_string(),
                            field_name: field_name.0.clone(),
                            span: field_name.1.clone(),
                        });
                        continue
                    };

                    self.unify(*expected_ty, expr_ty, field_name.1.clone());
                }
            }
            (Fields::Tuple(entries), FieldsSchema::Tuple(schema_fields)) => {
                self.compare_tuple_fields(entries, schema_fields, struct_span.clone())?;
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

        let type_arguments = self.get_type_arguments(&struct_schema, struct_span)?;
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
        let expr_ty = checker.check_expr(&expr).unwrap();

        assert_eq!(checker.types[expr_ty], Type::I32);
        insta::assert_debug_snapshot!(checker.errors);

        let expr = Span(
            Expr::Binary(
                Span(BinaryOp::Add, 3..4),
                Box::new(Span(Expr::Value(Value::I32(1)), 0..1)),
                Box::new(Span(Expr::Value(Value::Bool(true)), 6..9)),
            ),
            0..9,
        );

        assert_eq!(checker.check_expr(&expr), None);
        insta::assert_debug_snapshot!(checker.errors);
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

        insta::assert_debug_snapshot!(checker.check(&program));

        let checker = TypeChecker::new();
        let program = parse_program(
            "
        let x = 1;
        let y = 2;
        let z = if true { a } else { b }
        c + y;
        ",
        );

        insta::assert_debug_snapshot!(checker.check(&program))
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

        insta::assert_debug_snapshot!(checker.check(&program));

        let checker = TypeChecker::new();
        let program = parse_program("let x = f();");

        insta::assert_debug_snapshot!(checker.check(&program));
    }
}
