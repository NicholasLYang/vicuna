use crate::ast::{BinaryOp, Expr, ExprBlock, Stmt, UnaryOp, Value};
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

fn expr_parser() -> impl Parser<char, Expr, Error = Simple<char>> + Clone {
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
            .or(expr.delimited_by(just('('), just(')')))
            .or(bool)
            .or(ident.map(Expr::Variable))
            .padded();

        let op = |c| just(c).padded();

        let unary = op('-')
            .to(UnaryOp::Negate)
            .or(op('!').to(UnaryOp::Not))
            .repeated()
            .then(atom)
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

fn stmt() -> impl Parser<char, Stmt, Error = Simple<char>> {
    recursive(|stmt| {
        let ident = text::ident().padded();

        let expr_block = stmt
            .repeated()
            .then(expr_parser())
            .delimited_by(just('{'), just('}'))
            .map(|(stmts, end_expr)| ExprBlock {
                stmts,
                end_expr: Some(end_expr),
            })
            .padded();

        let let_decl = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr_parser().padded())
            .then_ignore(just(';'))
            .map(|(ident, expr)| Stmt::Let(ident, expr));

        let let_if_decl = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then_ignore(text::keyword("if").padded())
            .then(expr_parser())
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

        let_if_decl.or(let_decl).or(expr_parser()
            .then_ignore(just(';').padded())
            .map(Stmt::Expr))
    })
}

pub fn parser() -> impl Parser<char, Vec<Stmt>, Error = Simple<char>> {
    stmt().repeated().then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_bool() {
        assert_eq!(
            expr_parser().parse("true"),
            Ok(Expr::Value(Value::Bool(true)))
        );
        assert_eq!(
            expr_parser().parse("false"),
            Ok(Expr::Value(Value::Bool(false)))
        );
    }

    #[test]
    fn test_parse_int() {
        assert_eq!(expr_parser().parse("10"), Ok(Expr::Value(Value::I32(10))));
        assert_eq!(expr_parser().parse("2"), Ok(Expr::Value(Value::I32(2))));
    }

    #[test]
    fn test_parse_float() {
        assert_eq!(
            expr_parser().parse("10.5"),
            Ok(Expr::Value(Value::F32(10.5)))
        );
        assert_eq!(expr_parser().parse("2.1"), Ok(Expr::Value(Value::F32(2.1))));
    }

    #[test]
    fn test_parse_variable() {
        assert_eq!(
            expr_parser().parse("abcd"),
            Ok(Expr::Variable("abcd".to_string()))
        );
        assert_eq!(
            expr_parser().parse("foo_bar"),
            Ok(Expr::Variable("foo_bar".to_string()))
        );
        assert_eq!(
            expr_parser().parse("fooBar"),
            Ok(Expr::Variable("fooBar".to_string()))
        );
        assert_eq!(
            expr_parser().parse("_fooBar"),
            Ok(Expr::Variable("_fooBar".to_string()))
        );
        assert_eq!(
            expr_parser().parse("_"),
            Ok(Expr::Variable("_".to_string()))
        );
        assert_eq!(
            expr_parser().parse("a3"),
            Ok(Expr::Variable("a3".to_string()))
        );
    }

    #[test]
    fn test_parse_unary() {
        assert_eq!(
            expr_parser().parse("-10"),
            Ok(Expr::Unary(
                UnaryOp::Negate,
                Box::new(Expr::Value(Value::I32(10)))
            ))
        );

        assert_eq!(
            expr_parser().parse("!bar"),
            Ok(Expr::Unary(
                UnaryOp::Not,
                Box::new(Expr::Variable("bar".to_string()))
            ))
        );

        assert_eq!(
            expr_parser().parse("-!10"),
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
            expr_parser().parse("10 * 11"),
            Ok(Expr::Binary(
                BinaryOp::Multiply,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)))
            ))
        );

        assert_eq!(
            expr_parser().parse("10 / 11"),
            Ok(Expr::Binary(
                BinaryOp::Divide,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)))
            ))
        );

        assert_eq!(
            expr_parser().parse("10 / 11 / 12"),
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
            expr_parser().parse("10 + 11"),
            Ok(Expr::Binary(
                BinaryOp::Add,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)),)
            ))
        );

        assert_eq!(
            expr_parser().parse("10 - 11"),
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
            expr_parser().parse("foo + (11 * 2)"),
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
            expr_parser().parse("(10 - 11)"),
            Ok(Expr::Binary(
                BinaryOp::Subtract,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)),)
            ))
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
