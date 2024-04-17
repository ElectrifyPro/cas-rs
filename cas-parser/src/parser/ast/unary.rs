use crate::{
    parser::{
        ast::{binary::Binary, expr::{Expr, Primary}},
        error::{kind, Error},
        fmt::Latex,
        token::op::{Associativity, UnaryOp},
        Parser,
        ParseResult,
    },
    return_if_ok,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

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
///
/// Unary expressions do not directly implement [`Parse`] due to performance implications involving
/// parsing left-associative unary expressions (see [`Unary::parse_left_or_operand`]). Instead, the
/// a combination of [`Unary::parse_right`] and [`Unary::parse_left_or_operand`] can be used to
/// parse unary expressions.
///
/// [`Parse`]: crate::parser::Parse
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

    /// Parse a unary expression with right-associativity.
    pub fn parse_right(input: &mut Parser, recoverable_errors: &mut Vec<Error>) -> Result<Self, Vec<Error>> {
        let op = try_parse_unary_op(input, Associativity::Right)?;
        let op_precedence = op.precedence();
        let start_span = op.span.start;
        let operand = {
            let lhs = Unary::parse_or_lower(input, recoverable_errors)?;
            Binary::parse_expr(input, recoverable_errors, lhs, op_precedence)?.0
        };
        let end_span = operand.span().end;
        Ok(Self {
            operand: Box::new(operand),
            op,
            span: start_span..end_span,
        })
    }

    /// Parse a unary expression with left-associativity.
    ///
    /// By the nature of left-associative operators, we must parse the operand first. This can
    /// result in enormous backtracking if the operator is not present. To avoid this, this
    /// function returns the parsed operand as an [`Primary`] if it does determine that there
    /// is no operator present.
    pub fn parse_left_or_operand(input: &mut Parser, recoverable_errors: &mut Vec<Error>) -> Result<Expr, Vec<Error>> {
        let operand = input.try_parse::<Primary>().forward_errors(recoverable_errors)?;
        let start_span = operand.span().start;

        // one operator must be present
        let op = match try_parse_unary_op(input, Associativity::Left) {
            Ok(op) => op,
            Err(_) => return Ok(operand.into()),
        };
        let mut result = Self {
            operand: Box::new(operand.into()),
            op,
            span: start_span..input.prev_token().unwrap().span.end,
        };

        // iteratively find any other left-associative operators
        while let Ok(next_op) = try_parse_unary_op(input, Associativity::Left) {
            result = Self {
                operand: Box::new(Expr::Unary(result)),
                op: next_op,
                span: start_span..input.prev_token().unwrap().span.end,
            };
        }

        Ok(Expr::Unary(result))
    }

    /// Parses a unary expression, or lower precedence expressions.
    pub fn parse_or_lower(
        input: &mut Parser,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Expr, Vec<Error>> {
        let _ = return_if_ok!(Self::parse_right(input, recoverable_errors).map(Expr::Unary));
        Self::parse_left_or_operand(input, recoverable_errors)
    }
}

impl std::fmt::Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.op.associativity() {
            Associativity::Left => {
                self.operand.fmt(f)?;
                self.op.fmt(f)
            },
            Associativity::Right => {
                self.op.fmt(f)?;
                self.operand.fmt(f)
            },
        }
    }
}

impl Latex for Unary {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.op.associativity() {
            Associativity::Left => {
                self.operand.fmt_latex(f)?;
                self.op.fmt_latex(f)
            },
            Associativity::Right => {
                self.op.fmt_latex(f)?;
                self.operand.fmt_latex(f)
            },
        }
    }
}
