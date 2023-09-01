use std::ops::Range;
use crate::{
    parser::{
        binary::Binary,
        expr::{Expr, Primary},
        error::{kind, Error},
        token::op::{Associativity, UnaryOp},
        Parse,
        Parser,
        ParseResult,
    },
    return_if_ok,
};

/// Attempt to parse a unary operator with the correct associativity. Returns a non-fatal error if
/// the operator is not of the correct associativity.
fn try_parse_unary_op(input: &mut Parser, associativity: Associativity) -> Result<UnaryOp, Vec<Error>> {
    input.try_parse_then::<UnaryOp, _>(|op, input| {
        if op.associativity() == associativity {
            ParseResult::Ok(())
        } else {
            ParseResult::Unrecoverable(vec![input.error(kind::NonFatal)])
        }
    }).forward_errors(&mut Vec::new())
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
    pub fn parse_with_associativity(
        input: &mut Parser,
        associativity: Associativity,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        match associativity {
            Associativity::Left => {
                // to avoid infinite recursion, parse a terminal expression first
                let operand: Expr = input.try_parse::<Primary>().forward_errors(recoverable_errors)?.into();
                let start_span = operand.span().start;

                // one operator must be present
                let op = try_parse_unary_op(input, associativity)?;
                let mut result = Self {
                    operand: Box::new(operand),
                    op,
                    span: start_span..input.prev_token().unwrap().span.end,
                };

                // iteratively find any other left-associative operators
                while let Ok(next_op) = try_parse_unary_op(input, associativity) {
                    result = Self {
                        operand: Box::new(Expr::Unary(result)),
                        op: next_op,
                        span: start_span..input.prev_token().unwrap().span.end,
                    };
                }

                Ok(result)
            },
            Associativity::Right => {
                let op = try_parse_unary_op(input, associativity)?;
                let op_precedence = op.precedence();
                let start_span = input.prev_token().unwrap().span.start;
                let operand = {
                    let lhs = Unary::parse_or_lower(input, recoverable_errors)?;
                    Binary::parse_expr(input, recoverable_errors, lhs, op_precedence)?
                };
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
    pub fn parse_or_lower(
        input: &mut Parser,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Expr, Vec<Error>> {
        let _ = return_if_ok!(
            input.try_parse_with_fn(|input| {
                Self::parse(input).map(Expr::Unary)
            }).forward_errors(recoverable_errors)
        );
        Primary::parse(input)
            .map(Into::into)
            .forward_errors(recoverable_errors)
    }
}

impl<'source> Parse<'source> for Unary {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let _ = return_if_ok!(
            Self::parse_with_associativity(input, Associativity::Right, recoverable_errors)
        );
        Self::parse_with_associativity(input, Associativity::Left, recoverable_errors)
    }
}
