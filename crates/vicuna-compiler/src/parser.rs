use crate::ast::{
    BinaryOp, Expr, ExprBlock, ExprFields, Function, ImportType, MatchBindings, MatchCase, PostFix,
    Program, Span, Stmt, TypeDeclaration, TypeFields, TypeSig, UnaryOp, Value,
};
use chumsky::prelude::*;
use chumsky::primitive::OrderedContainer;
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

fn comment() -> impl Parser<char, (), Error = ParseError> + Clone {
    let single_line = just("//").then(take_until(text::newline())).ignored();

    let multi_line = just("/*").then(take_until(just("*/"))).ignored();

    single_line.or(multi_line)
}

fn optional_comment() -> impl Parser<char, (), Error = ParseError> + Clone {
    comment().or(empty())
}

fn string() -> impl Parser<char, String, Error = ParseError> + Clone {
    let string_char = none_of('"').or(just('\\').ignore_then(any()));
    just('"')
        .ignore_then(string_char.repeated())
        .then_ignore(just('"'))
        .padded_by(optional_comment())
        .map(|chars| chars.into_iter().collect())
}

pub(crate) fn expression() -> impl Parser<char, Span<Expr>, Error = ParseError> + Clone {
    let ident = text::ident().padded().padded_by(optional_comment());
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

        let bool = keyword("true")
            .padded_by(optional_comment())
            .to(true)
            .or(keyword("false").padded_by(optional_comment()).to(false))
            .map_with_span(|b, span| Span(Expr::Value(Value::Bool(b)), span));

        let string = string().map_with_span(|s, span| Span(Expr::Value(Value::String(s)), span));

        let named_fields = ident
            .clone()
            .map_with_span(Span)
            .then_ignore(just(':'))
            .then(expr.clone())
            .separated_by(just(','))
            .allow_trailing()
            .delimited_by(just('{'), just('}'))
            .map(|fields| ExprFields::Named(fields.into_iter().collect()));

        let tuple_fields = expr
            .clone()
            .separated_by(just(','))
            .allow_trailing()
            .delimited_by(just('('), just(')'))
            .map(ExprFields::Tuple);

        let enum_literal = ident
            .clone()
            .map_with_span(Span)
            .then_ignore(just("::"))
            .then(ident.clone().map_with_span(Span))
            .then(named_fields.clone().or(tuple_fields.clone()))
            .map_with_span(|((enum_name, variant_name), fields), span| {
                Span(
                    Expr::Enum {
                        enum_name,
                        variant_name,
                        fields,
                    },
                    span,
                )
            });

        let struct_literal = ident
            .clone()
            .map_with_span(Span)
            .then(named_fields)
            .map_with_span(|(name, fields), span| Span(Expr::Struct(name, fields), span));

        let atom = float
            .or(int)
            .or(expr.clone().delimited_by(just('('), just(')')))
            .or(bool)
            .or(string)
            .or(enum_literal)
            .or(struct_literal)
            .or(ident
                .clone()
                .map_with_span(|i, span| Span(Expr::Variable(i), span)))
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
    recursive(|type_sig| {
        just("i32")
            .padded()
            .to(TypeSig::I32)
            .or(just("f32").padded().to(TypeSig::F32))
            .or(just("string").padded().to(TypeSig::String))
            .or(just("bool").padded().to(TypeSig::Bool))
            .or(ident()
                .then(
                    type_sig
                        .separated_by(just(','))
                        .delimited_by(just('<'), just('>')),
                )
                .map(|(name, args)| TypeSig::Named(name, args)))
            .or(ident().map(|name| TypeSig::Named(name, vec![])))
            .map_with_span(Span)
    })
}

fn ident() -> impl Parser<char, Span<String>, Error = ParseError> + Clone {
    text::ident()
        .padded()
        .padded_by(optional_comment())
        .map_with_span(Span)
}

pub fn just_padded<C: OrderedContainer<char> + Clone>(
    inputs: C,
) -> impl Parser<char, C, Error = ParseError> + Clone {
    just(inputs).padded_by(optional_comment())
}

pub fn keyword(s: &'static str) -> impl Parser<char, (), Error = ParseError> + Clone {
    text::keyword(s).padded_by(optional_comment())
}

fn type_declaration() -> impl Parser<char, TypeDeclaration, Error = ParseError> {
    let ident = ident();

    let field = ident
        .clone()
        .then_ignore(just(':'))
        .then(type_signature())
        .padded();

    let named_fields = field
        .separated_by(just_padded(','))
        .allow_trailing()
        .padded()
        .delimited_by(just_padded('{'), just_padded('}'))
        .map(|fields| TypeFields::Named(fields.into_iter().collect()));

    let tuple_fields = type_signature()
        .separated_by(just_padded(','))
        .allow_trailing()
        .padded()
        .delimited_by(just_padded('('), just(')'))
        .map(TypeFields::Tuple);

    let empty_fields = empty().to(TypeFields::Empty);

    let type_parameters = ident
        .clone()
        .separated_by(just_padded(','))
        .allow_trailing()
        .delimited_by(just_padded('<'), just_padded('>'))
        .map_with_span(Span)
        .map(Some)
        .or(empty().to(None))
        .padded();

    let struct_declaration = keyword("struct")
        .ignore_then(ident.clone())
        .then(type_parameters.clone())
        .then(
            named_fields
                .clone()
                .or(tuple_fields.clone())
                .or(empty_fields.clone()),
        )
        .map(
            |((name, type_parameters), fields)| TypeDeclaration::Struct {
                name,
                type_parameters,
                fields,
            },
        );

    let enum_variant = ident
        .clone()
        .then(named_fields.or(tuple_fields).or(empty_fields))
        .padded();

    let enum_declaration = keyword("enum")
        .ignore_then(ident)
        .then(type_parameters)
        .then(
            enum_variant
                .separated_by(just_padded(','))
                .allow_trailing()
                .delimited_by(just_padded('{'), just_padded('}')),
        )
        .map(
            |((name, type_parameters), variants)| TypeDeclaration::Enum {
                name,
                type_parameters,
                variants: variants.into_iter().collect(),
            },
        );

    enum_declaration.or(struct_declaration)
}

fn statement() -> impl Parser<char, Span<Stmt>, Error = ParseError> {
    recursive(|stmt| {
        let ident = ident();

        let function_parameters = ident
            .clone()
            .then_ignore(just_padded(':'))
            .then(type_signature())
            .padded()
            .separated_by(just_padded(','))
            .delimited_by(just_padded('('), just_padded(')'));

        let return_type = just_padded("->")
            .padded()
            .ignore_then(type_signature())
            .map(Some)
            .padded();

        let optional_return_type = return_type.or(empty().to(None));
        let mut if_expression = Recursive::declare();
        let mut match_expression = Recursive::declare();
        let mut expression_block = Recursive::declare();

        if_expression.define(
            keyword("if")
                .padded()
                .ignore_then(expression().map(Box::new))
                .then(expression_block.clone().map_with_span(Span))
                .then_ignore(keyword("else"))
                .then(expression_block.clone().map_with_span(Span))
                .map_with_span(|((condition, then_block), else_block), span| {
                    Span(
                        Expr::If {
                            condition,
                            then_block,
                            else_block,
                        },
                        span,
                    )
                }),
        );

        let named_bindings = ident
            .clone()
            .then(
                empty()
                    .to(None)
                    .or(just_padded(':').ignore_then(ident.clone()).map(Some)),
            )
            .separated_by(just_padded(','))
            .allow_trailing()
            .delimited_by(just_padded('{'), just_padded('}'))
            .map(|bindings| MatchBindings::Named(bindings.into_iter().collect::<Vec<_>>()));

        let tuple_bindings = ident
            .clone()
            .separated_by(just_padded(','))
            .allow_trailing()
            .delimited_by(just_padded('('), just_padded(')'))
            .map(|bindings| MatchBindings::Tuple(bindings.into_iter().collect::<Vec<_>>()));

        let match_pattern = ident
            .clone()
            .then_ignore(just_padded("::"))
            .then(ident.clone())
            .then(
                named_bindings
                    .or(tuple_bindings)
                    .map(Some)
                    .or(empty().to(None)),
            )
            .padded()
            .map_with_span(|((enum_name, variant_name), fields), span| {
                Span(
                    MatchCase {
                        enum_name,
                        variant_name,
                        fields,
                    },
                    span,
                )
            });

        match_expression.define(
            keyword("match")
                .padded()
                .ignore_then(expression().map(Box::new))
                .then(
                    match_pattern
                        .then_ignore(just_padded("=>"))
                        .then(expression_block.clone().map_with_span(Span))
                        .padded()
                        .separated_by(just_padded(','))
                        .allow_trailing()
                        .padded()
                        .delimited_by(just_padded('{'), just_padded('}')),
                )
                .map_with_span(|(expr, cases), span| Span(Expr::Match { expr, cases }, span)),
        );

        expression_block.define(
            stmt.clone()
                .repeated()
                .then(
                    if_expression
                        .clone()
                        .or(match_expression.clone())
                        .or(expression())
                        .padded()
                        .map(Box::new)
                        .map(Some)
                        .or(empty().to(None)),
                )
                .delimited_by(just_padded('{'), just_padded('}'))
                .map(|(stmts, end_expr)| ExprBlock { stmts, end_expr })
                .padded(),
        );

        let function_decl = keyword("fn")
            .ignore_then(ident.clone())
            .then(function_parameters)
            .then(optional_return_type)
            .then(expression_block.map_with_span(Span))
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

        let let_decl = keyword("let")
            .ignore_then(ident.clone())
            .then_ignore(just_padded('='))
            .then(
                if_expression
                    .or(match_expression)
                    .or(expression().padded().then_ignore(just_padded(';'))),
            )
            .map_with_span(|(ident, expr), span| Span(Stmt::Let(ident, expr), span));

        let block = stmt
            .repeated()
            .delimited_by(just_padded('{'), just_padded('}'));

        let optional_else = keyword("else")
            .padded()
            .ignore_then(block.clone())
            .or(empty().to(Vec::new()));

        let if_stmt = keyword("if")
            .ignore_then(expression())
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

        let use_stmt = keyword("use")
            .ignore_then(ident.clone())
            .then_ignore(just_padded("::"))
            .then(
                ident
                    .clone()
                    .or(just_padded("*").map_with_span(|s, span| Span(s.to_string(), span))),
            )
            .then_ignore(just_padded(';'))
            .map_with_span(|(module, name), span| Span(Stmt::Use { module, name }, span));

        let import_stmt = keyword("import")
            .padded()
            .ignore_then(
                keyword("extern")
                    .padded()
                    .to(ImportType::External)
                    .or(empty().to(ImportType::Internal))
                    .map_with_span(Span),
            )
            .then(ident.clone().map(Some).or(empty().to(None)))
            .then_ignore(just_padded(',').padded().to(()).or(empty()))
            .then(
                ident
                    .separated_by(just_padded(','))
                    .allow_trailing()
                    .delimited_by(just_padded('{'), just_padded('}'))
                    .or(empty().to(Vec::new())),
            )
            .then_ignore(keyword("from").padded())
            .then(string().map_with_span(Span))
            .then_ignore(just_padded(';'))
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

        let return_stmt = keyword("return")
            .ignore_then(expression().padded().map(Some).or(empty().to(None)))
            .then_ignore(just_padded(';'))
            .map_with_span(|expr, span| Span(Stmt::Return(expr), span));

        function_decl
            .or(let_decl)
            .or(if_stmt)
            .or(return_stmt)
            .or(import_stmt)
            .or(type_declaration)
            .or(use_stmt)
            .or(expression()
                .then_ignore(just_padded(';'))
                .map_with_span(|expr, span| Span(Stmt::Expr(expr), span)))
            .padded()
            .padded_by(optional_comment())
    })
}

fn parser() -> impl Parser<char, Vec<Span<Stmt>>, Error = ParseError> {
    comment()
        .to(None)
        .or(statement().map(Some))
        .repeated()
        .flatten()
        .then_ignore(end())
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
        insta::assert_yaml_snapshot!(expression().parse("true"));
        insta::assert_yaml_snapshot!(expression().parse("false"));
    }

    #[test]
    fn test_parse_int() {
        insta::assert_yaml_snapshot!(expression().parse("10"));
        insta::assert_yaml_snapshot!(expression().parse("2"));
    }

    #[test]
    fn test_parse_float() {
        insta::assert_yaml_snapshot!(expression().parse("10.5"));
        insta::assert_yaml_snapshot!(expression().parse("2.1"));
    }

    #[test]
    fn test_parse_variable() {
        insta::assert_yaml_snapshot!(expression().parse("abcd"));
        insta::assert_yaml_snapshot!(expression().parse("foo_bar"));
        insta::assert_yaml_snapshot!(expression().parse("fooBar"));
        insta::assert_yaml_snapshot!(expression().parse("_fooBar"));
        insta::assert_yaml_snapshot!(expression().parse("_"));
        insta::assert_yaml_snapshot!(expression().parse("a3"));
    }

    #[test]
    fn test_parse_struct_literal() {
        insta::assert_yaml_snapshot!(expression().parse("Foo { a: 10, b: 20 }"));
        insta::assert_yaml_snapshot!(expression().parse("Foo { a: 10 }"));

        insta::assert_yaml_snapshot!(expression().parse("Foo::Bar { a: 10, b: 20 }"));
    }

    #[test]
    fn test_parse_unary() {
        insta::assert_yaml_snapshot!(expression().parse("-10"));

        insta::assert_yaml_snapshot!(expression().parse("!bar"));

        insta::assert_yaml_snapshot!(expression().parse("-!10"));
    }

    #[test]
    fn test_parse_multiply() {
        insta::assert_yaml_snapshot!(expression().parse("10 * 11"));

        insta::assert_yaml_snapshot!(expression().parse("10 / 11"));

        insta::assert_yaml_snapshot!(expression().parse("10 / 11 / 12"));
    }

    #[test]
    fn test_parse_addition() {
        insta::assert_yaml_snapshot!(expression().parse("10 + 11"));

        insta::assert_yaml_snapshot!(expression().parse("10 - 11"));
    }

    #[test]
    fn test_parse_parens() {
        insta::assert_yaml_snapshot!(expression().parse("foo + (11 * 2)"));

        insta::assert_yaml_snapshot!(expression().parse("(10 - 11)"));
    }

    #[test]
    fn test_parse_call() {
        insta::assert_yaml_snapshot!(expression().parse("foo()"));

        insta::assert_yaml_snapshot!(expression().parse("foo(10)"));

        insta::assert_yaml_snapshot!(expression().parse("foo(10)(20)"));
    }

    #[test]
    fn test_parse_function() {
        insta::assert_yaml_snapshot!(statement().parse("fn foo() { 20 }"));

        insta::assert_yaml_snapshot!(statement().parse("fn foo() -> i32 { 20 }"));

        insta::assert_yaml_snapshot!(statement().parse("fn foo() { let name = 10; }"));
    }

    #[test]
    fn test_parse_statement() {
        insta::assert_yaml_snapshot!(statement().parse("let a = 10;"));

        insta::assert_yaml_snapshot!(statement().parse("let a = if b { 10 } else { 20 }"));

        insta::assert_yaml_snapshot!(statement().repeated().parse(
            "let a = if b { 10 } else { 20 }
            10 + 11;
            let h = foobar;"
        ))
    }

    #[test]
    fn test_parse_if_statement() {
        insta::assert_yaml_snapshot!(statement().parse(
            "if b {
               let a = 10;
             } else {
               let b = 20;
             }"
        ));

        insta::assert_yaml_snapshot!(statement().parse("if b { 10; }"));
    }

    #[test]
    fn test_parse_import_statement() {
        insta::assert_yaml_snapshot!(statement().parse(r#"import foo from "./bar";"#));

        insta::assert_yaml_snapshot!(statement().parse(r#"import { foo, bar } from "./baz";"#));

        insta::assert_yaml_snapshot!(statement().parse(r#"import qux, { foo, bar } from "./baz";"#));

        insta::assert_yaml_snapshot!(
            statement().parse(r#"import extern { foo, bar } from "https://example.com/baz";"#)
        );
    }

    #[test]
    fn test_parse_return_statement() {
        insta::assert_yaml_snapshot!(statement().parse("return 10;"));

        insta::assert_yaml_snapshot!(statement().parse("return;"));
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

    #[test]
    fn test_parse_use_statement() {
        insta::assert_yaml_snapshot!(statement().parse("use Foo::Bar;"));

        insta::assert_yaml_snapshot!(statement().parse("use Foo::*;"));
    }
}
