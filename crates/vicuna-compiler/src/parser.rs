use crate::ast::{BinaryOp, Expr, UnaryOp, Value};
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

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Value(Value::I32(s.parse().unwrap())))
            .padded();

        let atom = int.or(expr.delimited_by(just('('), just(')'))).padded();

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
    .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_int() {
        assert_eq!(parser().parse("10"), Ok(Expr::Value(Value::I32(10))));
        assert_eq!(parser().parse("2"), Ok(Expr::Value(Value::I32(2))));
    }

    #[test]
    fn test_parse_unary() {
        assert_eq!(
            parser().parse("-10"),
            Ok(Expr::Unary(
                UnaryOp::Negate,
                Box::new(Expr::Value(Value::I32(10)))
            ))
        );

        assert_eq!(
            parser().parse("!10"),
            Ok(Expr::Unary(
                UnaryOp::Not,
                Box::new(Expr::Value(Value::I32(10)))
            ))
        );

        assert_eq!(
            parser().parse("-!10"),
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
            parser().parse("10 * 11"),
            Ok(Expr::Binary(
                BinaryOp::Multiply,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)))
            ))
        );

        assert_eq!(
            parser().parse("10 / 11"),
            Ok(Expr::Binary(
                BinaryOp::Divide,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)))
            ))
        );

        assert_eq!(
            parser().parse("10 / 11 / 12"),
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
            parser().parse("10 + 11"),
            Ok(Expr::Binary(
                BinaryOp::Add,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)),)
            ))
        );

        assert_eq!(
            parser().parse("10 - 11"),
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
            parser().parse("10 + (11 * 2)"),
            Ok(Expr::Binary(
                BinaryOp::Add,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Binary(
                    BinaryOp::Multiply,
                    Box::new(Expr::Value(Value::I32(11))),
                    Box::new(Expr::Value(Value::I32(2)))
                ))
            ))
        );

        assert_eq!(
            parser().parse("(10 - 11)"),
            Ok(Expr::Binary(
                BinaryOp::Subtract,
                Box::new(Expr::Value(Value::I32(10))),
                Box::new(Expr::Value(Value::I32(11)),)
            ))
        );
    }
}
