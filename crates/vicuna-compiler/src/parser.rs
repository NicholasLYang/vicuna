use crate::ast::{BinaryOp, Expr, ExprBlock, Function, PostFix, Stmt, TypeSig, UnaryOp, Value};
use anyhow::{anyhow, Result};
use chumsky::prelude::*;
use tree_sitter_c2rust::{Language, Tree};

extern "C" {
    fn tree_sitter_vicuna() -> Language;
}

pub fn parse(source: &str) -> Result<Tree> {
    let mut parser = tree_sitter_c2rust::Parser::new();
    parser.set_language(unsafe { tree_sitter_vicuna() })?;

    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| anyhow!("Unable to parse code"))?;

    Ok(tree)
}

pub(crate) fn expr() -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
    let ident = text::ident().padded();
    recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Value(Value::I32(s.parse().unwrap())))
            .padded();

        let float = text::int(10)
            .then_ignore(just("."))
            .then(text::int(10))
            .map(|(l, r)| Expr::Value(Value::F32(format!("{}.{}", l, r).parse().unwrap())));

        let bool = text::keyword("true")
            .to(true)
            .or(text::keyword("false").to(false))
            .map(|b| Expr::Value(Value::Bool(b)));

        let atom = float
            .or(int)
            .or(expr.clone().delimited_by(just('('), just(')')))
            .or(bool)
            .or(ident.map(Expr::Variable))
            .padded();

        let args = expr
            .clone()
            .separated_by(just(','))
            .allow_trailing()
            .delimited_by(just('('), just(')'))
            .map(PostFix::Args);

        let field = just('.').ignore_then(ident.clone()).map(PostFix::Field);

        let index = just('[')
            .ignore_then(expr.clone())
            .then_ignore(just(']'))
            .map(Box::new)
            .map(PostFix::Index);

        let call = atom
            .clone()
            .then(args.or(field).or(index).repeated())
            .foldl(|callee, post_fix| Expr::PostFix(Box::new(callee), post_fix));

        let op = |c| just(c).padded();

        let unary = op('-')
            .to(UnaryOp::Negate)
            .or(op('!').to(UnaryOp::Not))
            .repeated()
            .then(call.or(atom))
            .foldr(|op, rhs| Expr::Unary(op, Box::new(rhs)));

        let div: fn(_, _) -> _ = |lhs, rhs| Expr::Binary(BinaryOp::Divide, lhs, rhs);
        let mul: fn(_, _) -> _ = |lhs, rhs| Expr::Binary(BinaryOp::Multiply, lhs, rhs);

        let product = unary
            .clone()
            .then(op('*').to(mul).or(op('/').to(div)).then(unary).repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let add: fn(_, _) -> _ = |lhs, rhs| Expr::Binary(BinaryOp::Add, lhs, rhs);
        let sub: fn(_, _) -> _ = |lhs, rhs| Expr::Binary(BinaryOp::Subtract, lhs, rhs);

        let sum = product
            .clone()
            .then(op('+').to(add).or(op('-').to(sub)).then(product).repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        sum
    })
}

fn type_signature() -> impl Parser<char, TypeSig, Error = Simple<char>> {
    ident().map(|id| match id.as_str() {
        "i32" => TypeSig::I32,
        "f32" => TypeSig::F32,
        "string" => TypeSig::String,
        "bool" => TypeSig::Bool,
        _ => TypeSig::Named(id),
    })
}

fn ident() -> impl Parser<char, String, Error = Simple<char>> + Clone + Copy {
    text::ident().padded()
}

fn stmt() -> impl Parser<char, Stmt, Error = Simple<char>> {
    recursive(|stmt| {
        let ident = ident();

        let expr_block = stmt
            .repeated()
            .then(expr().map(Some).or(empty().to(None)))
            .delimited_by(just('{'), just('}'))
            .map(|(stmts, end_expr)| ExprBlock { stmts, end_expr })
            .padded();

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
            .then(expr_block.clone())
            .map(|(((name, params), return_type), body)| {
                Stmt::Function(Function {
                    name,
                    params,
                    return_type: return_type,
                    body,
                })
            });

        let let_decl = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr().padded())
            .then_ignore(just(';'))
            .map(|(ident, expr)| Stmt::Let(ident, expr));

        let let_if_decl = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then_ignore(text::keyword("if").padded())
            .then(expr())
            .then(expr_block.clone())
            .then_ignore(text::keyword("else"))
            .then(expr_block)
            .map(
                |(((name, condition), then_block), else_block)| Stmt::LetIf {
                    name,
                    condition,
                    then_block,
                    else_block,
                },
            );

        function_decl
            .or(let_if_decl)
            .or(let_decl)
            .or(expr().then_ignore(just(';')).map(Stmt::Expr))
            .padded()
    })
}

pub fn parser() -> impl Parser<char, Vec<Stmt>, Error = Simple<char>> {
    stmt().repeated().then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

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
            parser().parse("fn foo() { 20 }"),
            Ok(vec![Stmt::Function(Function {
                name: "foo".to_string(),
                params: vec![],
                return_type: None,
                body: ExprBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Value(Value::I32(20))),
                },
            })])
        );

        assert_eq!(
            parser().parse("fn foo() -> i32 { 20 }"),
            Ok(vec![Stmt::Function(Function {
                name: "foo".to_string(),
                params: vec![],
                return_type: Some(TypeSig::I32),
                body: ExprBlock {
                    stmts: vec![],
                    end_expr: Some(Expr::Value(Value::I32(20))),
                },
            })])
        );

        assert_eq!(
            parser().parse("fn foo() { let name = 10; }"),
            Ok(vec![Stmt::Function(Function {
                name: "foo".to_string(),
                params: vec![],
                return_type: None,
                body: ExprBlock {
                    stmts: vec![Stmt::Let("name".to_string(), Expr::Value(Value::I32(10)))],
                    end_expr: None,
                },
            })])
        );
    }

    #[test]
    fn test_parse_statement() {
        assert_eq!(
            parser().parse("let a = 10;"),
            Ok(vec![Stmt::Let(
                "a".to_string(),
                Expr::Value(Value::I32(10))
            )])
        );

        assert_eq!(
            parser().parse("let a = if b { 10 } else { 20 }"),
            Ok(vec![Stmt::LetIf {
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
            }])
        );

        assert_eq!(
            parser().parse(
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
}
