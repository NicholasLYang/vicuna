use crate::type_checker::{EnumSchema, Name, StructSchemaId, TypeId};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

/// We have multiple symbols that need to be tracked and scoped accordingly.
#[derive(Debug, Clone)]
pub enum SymbolTableEntry {
    /// Normal variables like parameters or let bindings
    Variable { ty: TypeId },
    /// Generic type variables that is meant to be solved. I.e. you call a generic function
    /// `<T>(T) -> T` with a concrete type `i32` and the type checker will solve
    /// the type variable `T = i32`.
    TypeVariable {
        /// Index into the `type_variables` vector
        idx: usize,
    },
    /// Generic type variables that are not meant to be solved. I.e. you are creating a
    /// generic function `<T>(T) -> T` and are type checking the function's body.
    AbstractTypeVariable,
    /// Structs
    Struct { schema_id: StructSchemaId },
    /// Enums
    Enum { schema: Arc<EnumSchema> },
}

pub struct SymbolTable {
    scopes: Vec<HashMap<Name, SymbolTableEntry>>,
    current_scope: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            current_scope: 0,
        }
    }

    pub fn enter_scope(&mut self) {
        self.current_scope += 1;
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.current_scope -= 1;
        self.scopes.pop();
    }

    pub(crate) fn insert(&mut self, name: Name, value: SymbolTableEntry) {
        self.scopes[self.current_scope].insert(name, value);
    }

    pub fn lookup(&self, name: impl AsRef<str>) -> Option<&SymbolTableEntry> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name.as_ref()) {
                return Some(ty);
            }
        }

        None
    }

    #[allow(dead_code)]
    pub fn lookup_mut(&mut self, name: &Name) -> Option<&mut SymbolTableEntry> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(ty) = scope.get_mut(name) {
                return Some(ty);
            }
        }

        None
    }
}

impl Debug for SymbolTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for scope in &self.scopes {
            writeln!(f, "---------------------")?;
            for (name, entry) in scope {
                writeln!(f, "{}: {:?}", name, entry)?;
            }
            writeln!(f, "---------------------")?;
        }

        Ok(())
    }
}
