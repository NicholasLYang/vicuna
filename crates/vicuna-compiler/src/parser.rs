use crate::ast::{
    BinaryOp, Expr, ExprBlock, Function, ImportType, PostFix, Program, Span, Stmt, TypeDeclaration,
    TypeSig, UnaryOp, Value,
};
use chumsky::prelude::*;
use miette::Diagnostic;
use serde::Serialize;
use std::ops::Range;
use thiserror::Error;

#[derive(Debug, Clone, Serialize, Error, Diagnostic)]
pub enum ParseError {
    #[diagnostic(code(parse_error::expected_found))]
    #[error("Expected {expected_chars:?} but found {received_char:?}")]
    ExpectedFound {
        #[label]
        span: Range<usize>,
        expected_chars: Vec<Option<char>>,
        received_char: Option<char>,
    },
}

impl chumsky::Error<char> for ParseError {
    type Span = Range<usize>;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<char>>>(
        span: Self::Span,
        expected_chars: Iter,
        received_char: Option<char>,
    ) -> Self {
        Self::ExpectedFound {
            span,
            expected_chars: expected_chars.into_iter().collect(),
            received_char,
        }
    }

    fn with_label(self, _: Self::Label) -> Self {
        self
    }

    fn merge(mut self, mut other: Self) -> Self {
        #[allow(irrefutable_let_patterns)]
        if let (
            Self::ExpectedFound { expected_chars, .. },
            Self::ExpectedFound {
                expected_chars: expected_other,
                ..
            },
        ) = (&mut self, &mut other)
        {
            expected_chars.append(expected_other)
        }

        self
    }
}

fn string() -> impl Parser<char, String, Error = ParseError> + Clone {
    let string_char = none_of('"').or(just('\\').ignore_then(any()));
    just('"')
        .ignore_then(string_char.repeated())
        .then_ignore(just('"'))
        .map(|chars| chars.into_iter().collect())
}

pub(crate) fn expr() -> impl Parser<char, Span<Expr>, Error = ParseError> + Clone {
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
            .map_with_span(Span)
            .then_ignore(just(':'))
            .then(expr.clone())
            .separated_by(just(','))
            .allow_trailing()
            .delimited_by(just('{'), just('}'));

        let enum_literal = ident
            .map_with_span(Span)
            .then_ignore(just("::"))
            .then(ident.map_with_span(Span))
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

        let struct_literal =
            ident
                .map_with_span(Span)
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
            .separated_by(just(','))
            .allow_trailing()
            .delimited_by(just('('), just(')'))
            .map_with_span(|args, span| Span(PostFix::Args(args), span));

        let field = just('.')
            .ignore_then(ident.map_with_span(Span))
            .map_with_span(|field, span| Span(PostFix::Field(field), span));

        let index = just('[')
            .ignore_then(expr.clone())
            .then_ignore(just(']'))
            .map_with_span(|index, span| Span(PostFix::Index(Box::new(index)), span));

        let call = atom
            .clone()
            .then(args.or(field).or(index).repeated())
            .foldl(|callee, post_fix| {
                let span = (callee.1.start())..(post_fix.1.end());
                Span(Expr::PostFix(Box::new(callee), post_fix), span)
            });

        let op = |c| just(c).padded();
        let op2 = |c| just(c).padded();

        let unary = op('-')
            .to(UnaryOp::Negate)
            .or(op('!').to(UnaryOp::Not))
            .map_with_span(Span)
            .repeated()
            .then(call.or(atom))
            .foldr(|op, rhs| {
                let span = (op.1.start())..(rhs.1.end());
                Span(Expr::Unary(op, Box::new(rhs)), span)
            });

        let product = unary
            .clone()
            .then(
                op('*')
                    .map_with_span(|_, span| Span(BinaryOp::Multiply, span))
                    .or(op('/').map_with_span(|_, span| Span(BinaryOp::Divide, span)))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let span = (lhs.1.start())..(rhs.1.end());
                Span(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), span)
            });

        let addition = product
            .clone()
            .then(
                op('+')
                    .to(BinaryOp::Add)
                    .or(op('-').to(BinaryOp::Subtract))
                    .map_with_span(Span)
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let span = (lhs.1.start())..(rhs.1.end());
                Span(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), span)
            });

        addition
            .clone()
            .then(
                op('>')
                    .to(BinaryOp::GreaterThan)
                    .or(op('<').to(BinaryOp::LessThan))
                    .or(op2(">=").to(BinaryOp::GreaterThanOrEqual))
                    .or(op2("<=").to(BinaryOp::LessThanOrEqual))
                    .or(op2("==").to(BinaryOp::Equal))
                    .or(op2("!=").to(BinaryOp::NotEqual))
                    .map_with_span(Span)
                    .then(addition)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let span = (lhs.1.start())..(rhs.1.end());
                Span(Expr::Binary(op, Box::new(lhs), Box::new(rhs)), span)
            })
    })
}

fn type_signature() -> impl Parser<char, Span<TypeSig>, Error = ParseError> + Clone {
    ident().map(|id| {
        let sig = match id.0.as_str() {
            "i32" => TypeSig::I32,
            "f32" => TypeSig::F32,
            "string" => TypeSig::String,
            "bool" => TypeSig::Bool,
            name => TypeSig::Named(name.to_string()),
        };

        Span(sig, id.1)
    })
}

fn ident() -> impl Parser<char, Span<String>, Error = ParseError> + Clone + Copy {
    text::ident().padded().map_with_span(Span)
}

fn if_expression() -> impl Parser<char, Span<Expr>, Error = ParseError> {
    text::keyword("if")
        .padded()
        .ignore_then(expr().map(Box::new))
        .then(expression_block().map_with_span(Span))
        .then_ignore(text::keyword("else"))
        .then(expression_block().map_with_span(Span))
        .map_with_span(|((condition, then_block), else_block), span| {
            Span(
                Expr::If {
                    condition,
                    then_block,
                    else_block,
                },
                span,
            )
        })
}

fn type_declaration() -> impl Parser<char, TypeDeclaration, Error = ParseError> {
    let ident = ident();

    let field = ident.then_ignore(just(':')).then(type_signature()).padded();

    let fields = field
        .separated_by(just(','))
        .allow_trailing()
        .padded()
        .delimited_by(just('{'), just('}'))
        .map(|fields| fields.into_iter().collect::<Vec<_>>());

    let struct_declaration = text::keyword("struct")
        .ignore_then(ident)
        .then(fields.clone())
        .map(|(name, fields)| TypeDeclaration::Struct { name, fields });

    let enum_variant = ident.then(fields.or(empty().to(Vec::new()))).padded();

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

fn expression_block() -> impl Parser<char, ExprBlock, Error = ParseError> {
    statement()
        .repeated()
        .then(expr().map(Box::new).map(Some).or(empty().to(None)))
        .delimited_by(just('{'), just('}'))
        .map(|(stmts, end_expr)| ExprBlock { stmts, end_expr })
        .padded()
}

fn statement() -> impl Parser<char, Span<Stmt>, Error = ParseError> {
    recursive(|stmt| {
        let ident = ident();

        let function_parameters = ident
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
            .ignore_then(ident)
            .then(function_parameters)
            .then(optional_return_type)
            .then(expression_block().map_with_span(Span))
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
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(if_expression().or(expr().padded()))
            .then_ignore(just(';'))
            .map_with_span(|(ident, expr), span| Span(Stmt::Let(ident, expr), span));

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
            .padded()
            .ignore_then(
                text::keyword("extern")
                    .padded()
                    .to(ImportType::External)
                    .or(empty().to(ImportType::Internal))
                    .map_with_span(Span),
            )
            .then(ident.map(Some).or(empty().to(None)))
            .then_ignore(just(',').padded().to(()).or(empty()))
            .then(
                ident
                    .separated_by(just(','))
                    .allow_trailing()
                    .delimited_by(just('{'), just('}'))
                    .or(empty().to(Vec::new())),
            )
            .then_ignore(text::keyword("from").padded())
            .then(string().map_with_span(Span))
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

fn parser() -> impl Parser<char, Vec<Span<Stmt>>, Error = ParseError> {
    statement().repeated().then_ignore(end())
}

pub fn parse(source: &str) -> (Option<Program>, Vec<ParseError>) {
    let (output, errors) = parser().parse_recovery(source);
    let program = output.map(|statements| Program { statements });

    (program, errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_bool() {
        insta::assert_yaml_snapshot!(expr().parse("true"));
        insta::assert_yaml_snapshot!(expr().parse("false"));
    }

    #[test]
    fn test_parse_int() {
        insta::assert_yaml_snapshot!(expr().parse("10"));
        insta::assert_yaml_snapshot!(expr().parse("2"));
    }

    #[test]
    fn test_parse_float() {
        insta::assert_yaml_snapshot!(expr().parse("10.5"));
        insta::assert_yaml_snapshot!(expr().parse("2.1"));
    }

    #[test]
    fn test_parse_variable() {
        insta::assert_yaml_snapshot!(expr().parse("abcd"));
        insta::assert_yaml_snapshot!(expr().parse("foo_bar"));
        insta::assert_yaml_snapshot!(expr().parse("fooBar"));
        insta::assert_yaml_snapshot!(expr().parse("_fooBar"));
        insta::assert_yaml_snapshot!(expr().parse("_"));
        insta::assert_yaml_snapshot!(expr().parse("a3"));
    }

    #[test]
    fn test_parse_struct_literal() {
        insta::assert_yaml_snapshot!(expr().parse("Foo { a: 10, b: 20 }"));
        insta::assert_yaml_snapshot!(expr().parse("Foo { a: 10 }"));

        insta::assert_yaml_snapshot!(expr().parse("Foo::Bar { a: 10, b: 20 }"));
    }

    #[test]
    fn test_parse_unary() {
        insta::assert_yaml_snapshot!(expr().parse("-10"));

        insta::assert_yaml_snapshot!(expr().parse("!bar"));

        insta::assert_yaml_snapshot!(expr().parse("-!10"));
    }

    #[test]
    fn test_parse_multiply() {
        insta::assert_yaml_snapshot!(expr().parse("10 * 11"));

        insta::assert_yaml_snapshot!(expr().parse("10 / 11"));

        insta::assert_yaml_snapshot!(expr().parse("10 / 11 / 12"));
    }

    #[test]
    fn test_parse_addition() {
        insta::assert_yaml_snapshot!(expr().parse("10 + 11"));

        insta::assert_yaml_snapshot!(expr().parse("10 - 11"));
    }

    #[test]
    fn test_parse_parens() {
        insta::assert_yaml_snapshot!(expr().parse("foo + (11 * 2)"));

        insta::assert_yaml_snapshot!(expr().parse("(10 - 11)"));
    }

    #[test]
    fn test_parse_call() {
        insta::assert_yaml_snapshot!(expr().parse("foo()"));

        insta::assert_yaml_snapshot!(expr().parse("foo(10)"));

        insta::assert_yaml_snapshot!(expr().parse("foo(10)(20)"));
    }

    #[test]
    fn test_parse_function() {
        insta::assert_yaml_snapshot!(stmt().parse("fn foo() { 20 }"));

        insta::assert_yaml_snapshot!(stmt().parse("fn foo() -> i32 { 20 }"));

        insta::assert_yaml_snapshot!(stmt().parse("fn foo() { let name = 10; }"));
    }

    #[test]
    fn test_parse_statement() {
        insta::assert_yaml_snapshot!(stmt().parse("let a = 10;"));

        insta::assert_yaml_snapshot!(stmt().parse("let a = if b { 10 } else { 20 }"));

        insta::assert_yaml_snapshot!(stmt().repeated().parse(
            "let a = if b { 10 } else { 20 }
            10 + 11;
            let h = foobar;"
        ))
    }

    #[test]
    fn test_parse_if_statement() {
        insta::assert_yaml_snapshot!(stmt().parse(
            "if b {
               let a = 10;
             } else {
               let b = 20;
             }"
        ));

        insta::assert_yaml_snapshot!(stmt().parse("if b { 10; }"));
    }

    #[test]
    fn test_parse_import_statement() {
        insta::assert_yaml_snapshot!(stmt().parse(r#"import foo from "./bar";"#));

        insta::assert_yaml_snapshot!(stmt().parse(r#"import { foo, bar } from "./baz";"#));

        insta::assert_yaml_snapshot!(stmt().parse(r#"import qux, { foo, bar } from "./baz";"#));

        insta::assert_yaml_snapshot!(
            stmt().parse(r#"import extern { foo, bar } from "https://example.com/baz";"#)
        );
    }

    #[test]
    fn test_parse_return_statement() {
        insta::assert_yaml_snapshot!(stmt().parse("return 10;"));

        insta::assert_yaml_snapshot!(stmt().parse("return;"));
    }

    #[test]
    fn test_parse_struct_declaration() {
        insta::assert_yaml_snapshot!(type_declaration().parse("struct Foo { a: i32, b: i32 }"));

        insta::assert_yaml_snapshot!(type_declaration().parse("struct Foo {}"));

        insta::assert_yaml_snapshot!(type_declaration().parse("struct Foo { a: i32 }"));

        insta::assert_yaml_snapshot!(type_declaration().parse("struct Foo { a: i32, }"));
    }

    #[test]
    fn test_parse_enum_declaration() {
        insta::assert_yaml_snapshot!(type_declaration().parse("enum Foo { A, B, C }"));

        insta::assert_yaml_snapshot!(type_declaration().parse("enum Foo { A }"));

        insta::assert_yaml_snapshot!(type_declaration().parse("enum Foo { A { foo: string } }"));
    }
}
