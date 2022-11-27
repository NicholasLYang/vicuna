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
