use std::ops::Range;
use super::{
    expr::{Expr, Primary},
    error::Error,
    token::op::UnaryOp,
    Associativity,
    Parse,
    Parser,
};

/// Attempt to parse a unary operator with the correct associativity. Returns a non-fatal error if
/// the operator is not of the correct associativity.
fn try_parse_unary_op(input: &mut Parser, associativity: Associativity) -> Result<UnaryOp, Error> {
    input.try_parse_with::<UnaryOp, _>(|op, input| {
        if op.associativity() == associativity {
            Ok(())
        } else {
            Err(input.non_fatal())
        }
    })
}

/// A unary expression, such as `2!`. Unary expressions can include nested expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    /// The operand of the unary expression (left or right, depending on the associativity).
    pub operand: Expr,

    /// The operator of the unary expression.
    pub op: UnaryOp,

    /// The region of the source code that this unary expression was parsed from.
    pub span: Range<usize>,
}

impl Unary {
    /// Parses a unary expression with respect to the given associativity.
    pub fn parse_with_associativity(input: &mut Parser, associativity: Associativity) -> Result<Self, Error> {
        let start = input.cursor;

        match associativity {
            Associativity::Left => {
                // to avoid infinite recursion, parse a terminal expression first
                let operand = input.try_parse::<Primary>()?.into();

                // one operator must be present
                let op = try_parse_unary_op(input, associativity)?;
                let mut result = Self {
                    operand,
                    op,
                    span: start..input.prev_token().unwrap().span.end
                };

                // iteratively find any other left-associative operators
                loop {
                    if let Ok(next_op) = try_parse_unary_op(input, associativity) {
                        result = Self {
                            operand: Expr::Unary(Box::new(result)),
                            op: next_op,
                            span: start..input.cursor,
                        };
                    } else {
                        break;
                    }
                }

                Ok(result)
            },
            Associativity::Right => {
                let op = try_parse_unary_op(input, associativity)?;
                Ok(Self {
                    operand: input.try_parse()?,
                    op,
                    span: start..input.cursor,
                })
            },
        }
    }
}

impl Parse for Unary {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        Self::parse_with_associativity(input, Associativity::Right)
            .or_else(|_| Self::parse_with_associativity(input, Associativity::Left))
    }
}
