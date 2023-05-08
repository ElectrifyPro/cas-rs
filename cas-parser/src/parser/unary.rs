use std::ops::Range;
use super::{
    binary::Binary,
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
    input.try_parse_then::<UnaryOp, _>(|op, input| {
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
    pub operand: Box<Expr>,

    /// The operator of the unary expression.
    pub op: UnaryOp,

    /// The region of the source code that this unary expression was parsed from.
    pub span: Range<usize>,
}

impl Unary {
    /// Returns the span of the unary expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    /// Parses a unary expression with respect to the given associativity.
    pub fn parse_with_associativity(input: &mut Parser, associativity: Associativity) -> Result<Self, Error> {
        match associativity {
            Associativity::Left => {
                // to avoid infinite recursion, parse a terminal expression first
                let operand: Expr = input.try_parse::<Primary>()?.into();
                let start_span = operand.span().start;

                // one operator must be present
                let op = try_parse_unary_op(input, associativity)?;
                let mut result = Self {
                    operand: Box::new(operand),
                    op,
                    span: start_span..input.prev_token().unwrap().span.end,
                };

                // iteratively find any other left-associative operators
                loop {
                    if let Ok(next_op) = try_parse_unary_op(input, associativity) {
                        result = Self {
                            operand: Box::new(Expr::Unary(result)),
                            op: next_op,
                            span: start_span..input.prev_token().unwrap().span.end,
                        };
                    } else {
                        break;
                    }
                }

                Ok(result)
            },
            Associativity::Right => {
                let op = try_parse_unary_op(input, associativity)?;
                let op_precedence = op.precedence();
                let start_span = input.prev_token().unwrap().span.start;
                let operand = input.try_parse_with_fn::<Expr, _>(|input| {
                    let lhs = input.try_parse_with_fn(Unary::parse_or_lower)?;
                    Binary::parse_expr(input, lhs, op_precedence)
                })?;
                let end_span = operand.span().end;
                Ok(Self {
                    operand: Box::new(operand),
                    op,
                    span: start_span..end_span,
                })
            },
        }
    }

    /// Parses a unary expression, or lower precedence expressions.
    pub fn parse_or_lower(input: &mut Parser) -> Result<Expr, Error> {
        input
            .try_parse_with_fn(|input| {
                Self::parse_with_associativity(input, Associativity::Right)
                    .or_else(|_| Self::parse_with_associativity(input, Associativity::Left))
                    .map(|unary| Expr::Unary(unary))
            })
            .or_else(|_| Primary::parse(input).map(Into::into))
    }
}

impl Parse for Unary {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        Self::parse_with_associativity(input, Associativity::Right)
            .or_else(|_| Self::parse_with_associativity(input, Associativity::Left))
    }
}
