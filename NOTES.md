How to compile if expressions? Maybe do not allow them to be nested inside stuff? Only assigned to variable or
as return value?

# AST Generation

Currently, I do AST generation manually by traversing the CST and generating AST nodes. This could be improved 
significantly. One option is to create an extension to tree-sitter that produces much higher quality bindings. 
A sketch of the idea is to map tree-sitter rules to Rust data structures. For instance, `seq` could be a tuple,
`repeat` could be a `Vec`, `token` could become a unit struct, `choice` is an enum, a rule becomes a struct, etc.

As an example:
```javascript
{
  letDeclaration: $ => seq(token("let"), $.name, token("="), $.expression)
}
```

```rust
struct LetDeclaration(LetToken(), Name, EqualsToken(), Expression);
```

Or even with fields:

```javascript
{
  letDeclaration: $ => seq(field("let_token", token("let")), field("name", $.name), field("equals_token", token("=")), field("rhs", $.expression))
}
```

```rust
struct LetDeclaration {
    let_token: LetToken(),
    name: Name,
    equals_token: EqualsToken(),
    rhs: Expression
}
```

This might require some rules on tree-sitter grammars such as `choice` must always be top level 
(although anonymous structs is a reasonable solution)

# Tree-Sitter WebAssembly

Currently, tree-sitter compiles to WebAssembly via a custom build script. I'd like to change that, so it can be compiled 
via the Rust build.rs script. That way it can be used with the rest of a Rust codebase that is compiled to WebAssembly.

The current idea is to fix up the build.rs script. However, a longer term idea is to just compile tree-sitter grammars 
to Rust instead of C.

# Type System
The basic types in Vicuna are `i32`, `f32`, `bool`, `string`.

We have structs:
```
struct Foo {
    a: i32,
    b: i32
}
```

and enums:

```
enum Bar {
    A(i32),
    B { 
      baz: string
    }
}
```

There are traits:

```
trait Foo {
    fn bar() -> i32;
}

impl Foo for i32 {
    fn bar() -> i32 {
        42
    }
}
```

# TODO
- Add nested pattern matching
- Add completeness checking for pattern matching
- ~~Add tuple enum variants~~
- ~~Add empty enum variants~~

# Consolidate Structs and Emums

Have a single struct rule that can take either one identifier or two identifiers separated by `::`.
In the symbol table we index by the full name, i.e. `Foo::Bar` or `Bar`. Somewhere we have to 
store the other variants in an enum.

Do we want to encode this in the `Type` type? Because right now we just say it's a named type,
which doesn't give any sort of info about the type itself, and is more a syntactic 
representation. Perhaps it should be a more sophisticated struct/enum type with type
substitutions.

# TODO:
- Add type checker output printing/playground
- Figure out how tuple enums work (should we bite the bullet and use type checker information?)
  - Or define functions that produce an enum representation? 
  - Ended up defining functions
  
