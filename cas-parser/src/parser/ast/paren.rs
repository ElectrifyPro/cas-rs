use crate::parser::{
    ast::expr::Expr,
    error::{kind, Error},
    fmt::Latex,
    garbage::Garbage,
    token::{CloseParen, OpenParen},
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A parenthesized expression. A [`Paren`] can only contain a single expression.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Paren {
    /// The inner expression.
    pub expr: Box<Expr>,

    /// The region of the source code that this [`Paren`] was parsed from.
    pub span: Range<usize>,
}

impl Paren {
    /// Returns the span of the parenthesized expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    /// Returns the innermost expression in the parenthesized expression.
    pub fn innermost(&self) -> &Expr {
        let mut inner = &self.expr;
        while let Expr::Paren(paren) = inner.as_ref() {
            inner = &paren.expr;
        }
        inner
    }

    /// Returns the innermost expression in the parenthesized expression, consuming the [`Paren`].
    pub fn into_innermost(self) -> Expr {
        let mut inner = self.expr;
        while let Expr::Paren(paren) = *inner {
            inner = paren.expr;
        }
        *inner
    }
}

impl<'source> Parse<'source> for Paren {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let open_paren = input.try_parse::<OpenParen>().forward_errors(recoverable_errors)?;
        let expr = input.try_parse().forward_errors(recoverable_errors)?;
        let close_paren = input.try_parse::<CloseParen>()
            .forward_errors(recoverable_errors)
            .unwrap_or_else(|_| {
                recoverable_errors.push(Error::new(
                    vec![open_paren.span.clone()],
                    kind::UnclosedParenthesis { opening: true },
                ));

                // fake a close paren for recovery purposes
                Garbage::garbage()
            });
        Ok(Self {
            expr: Box::new(expr),
            span: open_paren.span.start..close_paren.span.end,
        })
    }
}

impl std::fmt::Display for Paren {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(")?;
        self.expr.fmt(f)?;
        write!(f, ")")
    }
}

impl Latex for Paren {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\left(")?;
        self.expr.fmt_latex(f)?;
        write!(f, "\\right)")
    }
}
