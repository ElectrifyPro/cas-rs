use std::{fmt, ops::Range};
use super::{
    error::{kind, Error},
    expr::Expr,
    fmt::Latex,
    garbage::Garbage,
    token::{CloseParen, OpenParen},
    Parse,
    Parser,
};

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
}

impl<'source> Parse<'source> for Paren {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let open_paren = input.try_parse::<OpenParen>().forward_errors(recoverable_errors)?;
        let expr = match input.try_parse::<Expr>().forward_errors(recoverable_errors) {
            Ok(expr) => Ok(expr),
            Err(errs) => {
                if let Ok(close_paren) = input.try_parse::<CloseParen>().forward_errors(recoverable_errors) {
                    recoverable_errors.push(Error::new(
                        vec![open_paren.span.start..close_paren.span.end],
                        kind::EmptyParenthesis,
                    ));

                    // return a fake expression for recovery purposes, and also so that we don't
                    // try to parse the close paren again below (which would add an extraneous
                    // error to the error list)
                    return Garbage::garbage();
                } else {
                    Err(errs)
                }
            },
        }?;
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

impl Latex for Paren {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\left(")?;
        self.expr.fmt_latex(f)?;
        write!(f, "\\right)")
    }
}
