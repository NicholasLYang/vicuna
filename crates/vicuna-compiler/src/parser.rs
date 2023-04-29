use crate::ast::{
    BinaryOp, Expr, ExprBlock, Function, ImportType, PostFix, Program, Span, Stmt, TypeDeclaration,
    TypeSig, UnaryOp, Value,
};
use chumsky::prelude::*;
use std::collections::HashMap;

fn string() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    let string_char = none_of('"').or(just('\\').ignore_then(any()));
    just('"')
        .ignore_then(string_char.repeated())
        .then_ignore(just('"'))
        .map(|chars| chars.into_iter().collect())
}

pub(crate) fn expr() -> impl Parser<char, Span<Expr>, Error = Simple<char>> + Clone {
    let ident = text::ident().padded();
    recursive(|expr| {
        let int = text::int(10)
            .map_with_span(|s: String, span| {
                Span(Expr::Value(Value::I32(s.parse().unwrap())), span)
            })
            .padded();

        let float = text::int(10)
            .then_ignore(just("."))
            .then(text::int(10))
            .map_with_span(|(l, r), span| {
                Span(
                    Expr::Value(Value::F32(format!("{}.{}", l, r).parse().unwrap())),
                    span,
                )
            });

        let bool = text::keyword("true")
            .to(true)
            .or(text::keyword("false").to(false))
            .map_with_span(|b, span| Span(Expr::Value(Value::Bool(b)), span));

        let string = string().map_with_span(|s, span| Span(Expr::Value(Value::String(s)), span));

        let fields = ident
            .map_with_span(|s, span| Span(s, span))
            .then_ignore(just(':'))
            .then(expr.clone().map_with_span(|e, span| Span(e, span)))
            .separated_by(just(','))
            .delimited_by(just('{'), just('}'));

        let enum_literal = ident
            .clone()
            .map_with_span(|s, span| Span(s, span))
            .then_ignore(just("::"))
            .then(ident.map_with_span(|s, span| Span(s, span)))
            .then(fields.clone())
            .map_with_span(|((enum_name, variant_name), v), span| {
                Span(
                    Expr::Enum {
                        enum_name,
                        variant_name,
                        fields: v.into_iter().collect(),
                    },
                    span,
                )
            });

        let struct_literal = ident
            .map_with_span(|i, span| Span(i, span))
            .then(fields)
            .map_with_span(|(name, fields), span| {
                Span(Expr::Struct(name, fields.into_iter().collect()), span)
            });

        let atom = float
            .or(int)
            .or(expr.clone().delimited_by(just('('), just(')')))
            .or(bool)
            .or(string)
            .or(enum_literal)
            .or(struct_literal)
            .or(ident.map_with_span(|i, span| Span(Expr::Variable(i), span)))
            .padded();

        let args = expr
            .clone()
            .map_with_span(|e, span| Span(e, span))
            .separated_by(just(','))
            .allow_trailing()
            .delimited_by(just('('), just(')'))
            .map_with_span(|args, span| Span(PostFix::Args(args), span));

        let field = just('.')
            .ignore_then(ident.map_with_span(|e, span| Span(e, span)))
            .map_with_span(|field, span| Span(PostFix::Field(field), span));

        let index = just('[')
            .ignore_then(expr.clone().map_with_span(|e, span| Span(e, span)))
            .then_ignore(just(']'))
            .map_with_span(|index, span| Span(PostFix::Index(Box::new(index)), span));

        let call = atom
            .clone()
            .map_with_span(|e, span| Span(e, span))
            .then(
                args.or(field)
                    .or(index)
                    .repeated()
                    .map_with_span(|e, span| Span(e, span)),
            )
            .foldl(|callee, post_fix| {
                let span = (callee.1.start())..(post_fix.1.end());
                Span(Expr::PostFix(Box::new(callee), post_fix), span)
            });

        let op = |c| just(c).padded();

        let unary = op('-')
            .to(UnaryOp::Negate)
            .or(op('!').to(UnaryOp::Not))
            .repeated()
            .then(call.or(atom))
            .foldr(|op, rhs| Expr::Unary(op, Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                op('*')
                    .map_with_span(|_, span| Span(BinaryOp::Multiply, span))
                    .or(op('/').map_with_span(|_, span| Span(BinaryOp::Divide, span)))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

        product
            .clone()
            .then(
                op('+')
                    .map_with_span(|_, span| Span(BinaryOp::Add, span))
                    .or(op('-').map_with_span(|_, span| Span(BinaryOp::Subtract, span)))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)))
    })
}

fn type_signature() -> impl Parser<char, Span<TypeSig>, Error = Simple<char>> + Clone {
    ident().map_with_span(|id, span| {
        let sig = match id.as_str() {
            "i32" => TypeSig::I32,
            "f32" => TypeSig::F32,
            "string" => TypeSig::String,
            "bool" => TypeSig::Bool,
            _ => TypeSig::Named(id),
        };

        Span(sig, span)
    })
}

fn ident() -> impl Parser<char, String, Error = Simple<char>> + Clone + Copy {
    text::ident().padded()
}

fn type_declaration() -> impl Parser<char, TypeDeclaration, Error = Simple<char>> {
    let ident = ident();

    let field = ident.then_ignore(just(':')).then(type_signature()).padded();

    let fields = field
        .separated_by(just(','))
        .allow_trailing()
        .padded()
        .delimited_by(just('{'), just('}'))
        .map(|fields| fields.into_iter().collect::<HashMap<_, _>>());

    let struct_declaration = text::keyword("struct")
        .ignore_then(ident.clone().map_with_span(|i, span| Span(i, span)))
        .then(fields.clone())
        .map(|(name, fields)| TypeDeclaration::Struct { name, fields });

    let enum_variant = ident.then(fields.or(empty().to(HashMap::new()))).padded();

    let enum_declaration = text::keyword("enum")
        .ignore_then(ident)
        .then(
            enum_variant
                .separated_by(just(','))
                .allow_trailing()
                .delimited_by(just('{'), just('}')),
        )
        .map(|(name, variants)| TypeDeclaration::Enum {
            name,
            variants: variants.into_iter().collect(),
        });

    enum_declaration.or(struct_declaration)
}

fn stmt() -> impl Parser<char, Span<Stmt>, Error = Simple<char>> {
    recursive(|stmt| {
        let ident = ident();

        let expr_block = stmt
            .clone()
            .repeated()
            .then(expr().map(Some).or(empty().to(None)))
            .delimited_by(just('{'), just('}'))
            .map(|(stmts, end_expr)| ExprBlock { stmts, end_expr })
            .padded();

        let function_parameters = ident
            .map_with_span(|id, span| Span(id, span))
            .then_ignore(just(':'))
            .then(type_signature())
            .padded()
            .separated_by(just(','))
            .delimited_by(just('('), just(')'));

        let return_type = just("->")
            .padded()
            .ignore_then(type_signature())
            .map(Some)
            .padded();

        let optional_return_type = return_type.or(empty().to(None));

        let function_decl = text::keyword("fn")
            .ignore_then(ident.map_with_span(|id, span| Span(id, span)))
            .then(function_parameters)
            .then(optional_return_type)
            .then(
                expr_block
                    .clone()
                    .map_with_span(|expr, span| Span(expr, span)),
            )
            .map_with_span(|(((name, params), return_type), body), span| {
                Span(
                    Stmt::Function(Function {
                        name,
                        params,
                        return_type,
                        body,
                    }),
                    span,
                )
            });

        let let_decl = text::keyword("let")
            .ignore_then(ident.map_with_span(|id, span| Span(id, span)))
            .then_ignore(just('='))
            .then(expr().padded())
            .then_ignore(just(';'))
            .map_with_span(|(ident, expr), span| Span(Stmt::Let(ident, expr), span));

        let let_if_decl = text::keyword("let")
            .ignore_then(ident.map_with_span(|id, span| Span(id, span)))
            .then_ignore(just('='))
            .then_ignore(text::keyword("if").padded())
            .then(expr())
            .then(
                expr_block
                    .clone()
                    .map_with_span(|block, span| Span(block, span)),
            )
            .then_ignore(text::keyword("else"))
            .then(expr_block.map_with_span(|block, span| Span(block, span)))
            .map_with_span(|(((name, condition), then_block), else_block), span| {
                Span(
                    Stmt::LetIf {
                        name,
                        condition,
                        then_block,
                        else_block,
                    },
                    span,
                )
            });

        let block = stmt.repeated().delimited_by(just('{'), just('}'));

        let optional_else = text::keyword("else")
            .padded()
            .ignore_then(block.clone())
            .or(empty().to(Vec::new()));

        let if_stmt = text::keyword("if")
            .ignore_then(expr())
            .then(block)
            .then(optional_else)
            .map_with_span(|((condition, then_block), else_block), span| {
                Span(
                    Stmt::If {
                        condition,
                        then_block,
                        else_block,
                    },
                    span,
                )
            });

        let import_stmt = text::keyword("import")
            .ignore_then(
                text::keyword("extern")
                    .to(ImportType::External)
                    .or(empty().to(ImportType::Internal))
                    .map_with_span(|ty, span| Span(ty, span)),
            )
            .then(
                ident
                    .clone()
                    .map_with_span(|i, span| Some(Span(i, span)))
                    .or(empty().to(None)),
            )
            .then_ignore(just(',').to(()).or(empty()))
            .then(
                ident
                    .clone()
                    .map_with_span(|i, span| Span(i, span))
                    .separated_by(just(','))
                    .allow_trailing()
                    .delimited_by(just('{'), just('}'))
                    .or(empty().to(Vec::new())),
            )
            .then_ignore(text::keyword("from"))
            .then(string().map_with_span(|s, span| Span(s, span)))
            .then_ignore(just(';'))
            .map_with_span(
                |(((import_type, default_import), named_imports), module), span| {
                    Span(
                        Stmt::Import {
                            ty: import_type,
                            default_import,
                            named_imports,
                            path: module,
                        },
                        span,
                    )
                },
            );

        let type_declaration =
            type_declaration().map_with_span(|decl, span| Span(Stmt::Type(decl), span));

        let return_stmt = text::keyword("return")
            .ignore_then(expr().padded().map(Some).or(empty().to(None)))
            .then_ignore(just(';'))
            .map_with_span(|expr, span| Span(Stmt::Return(expr), span));

        function_decl
            .or(let_if_decl)
            .or(let_decl)
            .or(if_stmt)
            .or(return_stmt)
            .or(import_stmt)
            .or(type_declaration)
            .or(expr()
                .then_ignore(just(';'))
                .map_with_span(|expr, span| Span(Stmt::Expr(expr), span)))
            .padded()
    })
}

fn parser() -> impl Parser<char, Vec<Span<Stmt>>, Error = Simple<char>> {
    stmt().repeated().then_ignore(end())
}

pub fn parse(source: &str) -> (Option<Program>, Vec<Simple<char>>) {
    let (output, errors) = parser().parse_recovery(source);
    let program = output.map(|statements| Program { statements });

    (program, errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_parse_bool() {
        assert_eq!(expr().parse("true"), Ok(Expr::Value(Value::Bool(true))));
        assert_eq!(expr().parse("false"), Ok(Expr::Value(Value::Bool(false))));
    }

    #[test]
    fn test_parse_int() {
        assert_eq!(expr().parse("10"), Ok(Expr::Value(Value::I32(10))));
        assert_eq!(expr().parse("2"), Ok(Expr::Value(Value::I32(2))));
    }

    #[test]
    fn test_parse_float() {
        assert_eq!(expr().parse("10.5"), Ok(Expr::Value(Value::F32(10.5))));
        assert_eq!(expr().parse("2.1"), Ok(Expr::Value(Value::F32(2.1))));
    }

    #[test]
    fn test_parse_variable() {
        assert_eq!(expr().parse("abcd"), Ok(Expr::Variable("abcd".to_string())));
        assert_eq!(
            expr().parse("foo_bar"),
            Ok(Expr::Variable("foo_bar".to_string()))
        );
        assert_eq!(
            expr().parse("fooBar"),
            Ok(Expr::Variable("fooBar".to_string()))
        );
        assert_eq!(
            expr().parse("_fooBar"),
            Ok(Expr::Variable("_fooBar".to_string()))
        );
        assert_eq!(expr().parse("_"), Ok(Expr::Variable("_".to_string())));
        assert_eq!(expr().parse("a3"), Ok(Expr::Variable("a3".to_string())));
    }

    #[test]
    fn test_parse_struct_literal() {
        assert_eq!(
            expr().parse("Foo { a: 10, b: 20 }"),
            Ok(Expr::Struct(
                "Foo".to_string(),
                vec![
                    ("a".to_string(), Expr::Value(Value::I32(10))),
                    ("b".to_string(), Expr::Value(Value::I32(20))),
                ]
                .into_iter()
                .collect()
            ))
        );

        assert_eq!(
            expr().parse("Foo::Bar { a: 10, b: 20 }"),
            Ok(Expr::Enum {
                enum_name: "Foo".to_string(),
                variant_name: "Bar".to_string(),
                fields: vec![
                    ("a".to_string(), Expr::Value(Value::I32(10))),
                    ("b".to_string(), Expr::Value(Value::I32(20))),
                ]
                .into_iter()
                .collect()
            })
        );
    }

    #[test]
    fn test_parse_unary() {
        assert_eq!(
            expr().parse("-10"),
            Ok(Expr::Unary(
                UnaryOp::Negate,
                Box::new(Expr::Value(Value::I32(10)))
            ))
        );

        assert_eq!(
            expr().parse("!bar"),
            Ok(Expr::Unary(
                UnaryOp::Not,
                Box::new(Expr::Variable("bar".to_string()))
            ))
        );

        assert_eq!(
            expr().parse("-!10"),
            Ok(Expr::Unary(
                UnaryOp::Negate,
                Box::new(Expr::Unary(
                    UnaryOp::Not,
                    Box::new(Expr::Value(Value::I32(10)))
                ))
            ))
        );
    }

    #[test]
    fn test_parse_multiply() {
        assert_eq!(
            expr().parse("10 * 11"),
            Ok(Expr::Binary(
                BinaryOp::Multiply,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)))
            ))
        );

        assert_eq!(
            expr().parse("10 / 11"),
            Ok(Expr::Binary(
                BinaryOp::Divide,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)))
            ))
        );

        assert_eq!(
            expr().parse("10 / 11 / 12"),
            Ok(Expr::Binary(
                BinaryOp::Divide,
                Box::new(Expr::Binary(
                    BinaryOp::Divide,
                    Box::new(Expr::Value(Value::I32(10))),
                    Box::new(Expr::Value(Value::I32(11)))
                )),
                Box::new(Expr::Value(Value::I32(12)),)
            ))
        )
    }

    #[test]
    fn test_parse_addition() {
        assert_eq!(
            expr().parse("10 + 11"),
            Ok(Expr::Binary(
                BinaryOp::Add,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)),)
            ))
        );

        assert_eq!(
            expr().parse("10 - 11"),
            Ok(Expr::Binary(
                BinaryOp::Subtract,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)),)
            ))
        );
    }

    #[test]
    fn test_parse_parens() {
        assert_eq!(
            expr().parse("foo + (11 * 2)"),
            Ok(Expr::Binary(
                BinaryOp::Add,
                Box::new(Expr::Variable("foo".to_string())),
                Box::new(Expr::Binary(
                    BinaryOp::Multiply,
                    Box::new(Expr::Value(Value::I32(11))),
                    Box::new(Expr::Value(Value::I32(2)))
                ))
            ))
        );

        assert_eq!(
            expr().parse("(10 - 11)"),
            Ok(Expr::Binary(
                BinaryOp::Subtract,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)),)
            ))
        );
    }

    #[test]
    fn test_parse_call() {
        assert_eq!(
            expr().parse("foo()"),
            Ok(Expr::PostFix(
                Box::new(Expr::Variable("foo".to_string())),
                PostFix::Args(vec![]),
            ))
        );

        assert_eq!(
            expr().parse("foo(10)"),
            Ok(Expr::PostFix(
                Box::new(Expr::Variable("foo".to_string())),
                PostFix::Args(vec![Expr::Value(Value::I32(10))]),
            ))
        );

        assert_eq!(
            expr().parse("foo(10)(20)"),
            Ok(Expr::PostFix(
                Box::new(Expr::PostFix(
                    Box::new(Expr::Variable("foo".to_string())),
                    PostFix::Args(vec![Expr::Value(Value::I32(10))]),
                )),
                PostFix::Args(vec![Expr::Value(Value::I32(20))]),
            ))
        );
    }

    #[test]
    fn test_parse_function() {
        assert_eq!(
            stmt().parse("fn foo() { 20 }"),
            Ok(Stmt::Function(Function {
                name: "foo".to_string(),
                params: vec![],
                return_type: None,
                body: ExprBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Value(Value::I32(20))),
                },
            }))
        );

        assert_eq!(
            stmt().parse("fn foo() -> i32 { 20 }"),
            Ok(Stmt::Function(Function {
                name: "foo".to_string(),
                params: vec![],
                return_type: Some(TypeSig::I32),
                body: ExprBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Value(Value::I32(20))),
                },
            }))
        );

        assert_eq!(
            stmt().parse("fn foo() { let name = 10; }"),
            Ok(Stmt::Function(Function {
                name: "foo".to_string(),
                params: vec![],
                return_type: None,
                body: ExprBlock {
                    stmts: vec![Stmt::Let("name".to_string(), Expr::Value(Value::I32(10)))],
                    end_expr: None,
                },
            }))
        );
    }

    #[test]
    fn test_parse_statement() {
        assert_eq!(
            stmt().parse("let a = 10;"),
            Ok(Stmt::Let("a".to_string(), Expr::Value(Value::I32(10))))
        );

        assert_eq!(
            stmt().parse("let a = if b { 10 } else { 20 }"),
            Ok(Stmt::LetIf {
                name: "a".to_string(),
                condition: Expr::Variable("b".to_string()),
                then_block: ExprBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Value(Value::I32(10)))
                },
                else_block: ExprBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Value(Value::I32(20)))
                },
            })
        );

        assert_eq!(
            stmt().repeated().parse(
                "let a = if b { 10 } else { 20 }
            10 + 11;
            let h = foobar;"
            ),
            Ok(vec![
                Stmt::LetIf {
                    name: "a".to_string(),
                    condition: Expr::Variable("b".to_string()),
                    then_block: ExprBlock {
                        stmts: vec![],
                        end_expr: Some(Expr::Value(Value::I32(10)))
                    },
                    else_block: ExprBlock {
                        stmts: vec![],
                        end_expr: Some(Expr::Value(Value::I32(20)))
                    },
                },
                Stmt::Expr(Expr::Binary(
                    BinaryOp::Add,
                    Box::new(Expr::Value(Value::I32(10))),
                    Box::new(Expr::Value(Value::I32(11)))
                )),
                Stmt::Let("h".to_string(), Expr::Variable("foobar".to_string()))
            ])
        )
    }

    #[test]
    fn test_parse_if_statement() {
        assert_eq!(
            stmt().parse(
                "if b {
               let a = 10;
             } else {
               let b = 20;
             }"
            ),
            Ok(Stmt::If {
                condition: Expr::Variable("b".to_string()),
                then_block: vec![Stmt::Let("a".to_string(), Expr::Value(Value::I32(10)))],
                else_block: vec![Stmt::Let("b".to_string(), Expr::Value(Value::I32(20)))],
            })
        );

        assert_eq!(
            stmt().parse("if b { 10; }"),
            Ok(Stmt::If {
                condition: Expr::Variable("b".to_string()),
                then_block: vec![Stmt::Expr(Expr::Value(Value::I32(10)))],
                else_block: vec![],
            })
        );
    }

    #[test]
    fn test_parse_import_statement() {
        assert_eq!(
            stmt().parse(r#"import foo from "./bar";"#),
            Ok(Stmt::Import {
                ty: ImportType::Internal,
                default_import: Some("foo".to_string()),
                named_imports: Vec::new(),
                path: "./bar".to_string(),
            })
        );

        assert_eq!(
            stmt().parse(r#"import { foo, bar } from "./baz";"#),
            Ok(Stmt::Import {
                ty: ImportType::Internal,
                default_import: None,
                named_imports: vec!["foo".to_string(), "bar".to_string()],
                path: "./baz".to_string(),
            })
        );

        assert_eq!(
            stmt().parse(r#"import qux, { foo, bar } from "./baz";"#),
            Ok(Stmt::Import {
                ty: ImportType::Internal,
                default_import: Some("qux".to_string()),
                named_imports: vec!["foo".to_string(), "bar".to_string()],
                path: "./baz".to_string(),
            })
        );

        assert_eq!(
            stmt().parse(r#"import extern { foo, bar } from "https://example.com/baz";"#),
            Ok(Stmt::Import {
                ty: ImportType::External,
                default_import: None,
                named_imports: vec!["foo".to_string(), "bar".to_string()],
                path: "https://example.com/baz".to_string(),
            })
        );
    }

    #[test]
    fn test_parse_return_statement() {
        assert_eq!(
            stmt().parse("return 10;"),
            Ok(Stmt::Return(Some(Expr::Value(Value::I32(10)))))
        );

        assert_eq!(stmt().parse("return;"), Ok(Stmt::Return(None)));
    }

    #[test]
    fn test_parse_struct_declaration() {
        assert_eq!(
            type_declaration().parse("struct Foo { a: i32, b: i32 }"),
            Ok(TypeDeclaration::Struct {
                name: "Foo".to_string(),
                fields: vec![
                    ("a".to_string(), TypeSig::I32),
                    ("b".to_string(), TypeSig::I32),
                ]
                .into_iter()
                .collect(),
            })
        );

        assert_eq!(
            type_declaration().parse("struct Foo {}"),
            Ok(TypeDeclaration::Struct {
                name: "Foo".to_string(),
                fields: HashMap::new(),
            })
        );

        assert_eq!(
            type_declaration().parse("struct Foo { a: i32 }"),
            Ok(TypeDeclaration::Struct {
                name: "Foo".to_string(),
                fields: vec![("a".to_string(), TypeSig::I32)].into_iter().collect(),
            })
        );

        assert_eq!(
            type_declaration().parse("struct Foo { a: i32, }"),
            Ok(TypeDeclaration::Struct {
                name: "Foo".to_string(),
                fields: vec![("a".to_string(), TypeSig::I32)].into_iter().collect(),
            })
        );
    }

    #[test]
    fn test_parse_enum_declaration() {
        assert_eq!(
            type_declaration().parse("enum Foo { A, B, C }"),
            Ok(TypeDeclaration::Enum {
                name: "Foo".to_string(),
                variants: vec![
                    ("A".to_string(), HashMap::new()),
                    ("B".to_string(), HashMap::new()),
                    ("C".to_string(), HashMap::new())
                ]
                .into_iter()
                .collect()
            })
        );

        assert_eq!(
            type_declaration().parse("enum Foo { A }"),
            Ok(TypeDeclaration::Enum {
                name: "Foo".to_string(),
                variants: vec![("A".to_string(), HashMap::new())]
                    .into_iter()
                    .collect()
            })
        );

        assert_eq!(
            type_declaration().parse("enum Foo { A { foo: string } }"),
            Ok(TypeDeclaration::Enum {
                name: "Foo".to_string(),
                variants: vec![(
                    "A".to_string(),
                    vec![("foo".to_string(), TypeSig::String)]
                        .into_iter()
                        .collect()
                )]
                .into_iter()
                .collect()
            })
        );
    }
}
