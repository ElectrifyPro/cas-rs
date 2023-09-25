use crate::parser::{
    ast::expr::Expr,
    error::Error,
    fmt::Latex,
    token::Semicolon,
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Represents a statement in CalcScript.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Stmt {
    /// The expression of a statement.
    pub expr: Expr,

    /// The span of the semicolon that terminates the statement, if any.
    ///
    /// When this is [`None`], the statement is an expression, and will return the value of the
    /// expression. Otherwise, the expression is evaluated for side effects, and the statement
    /// returns [`Value::Unit`].
    pub semicolon: Option<Range<usize>>,

    /// The region of the source code that this statement was parsed from.
    pub span: Range<usize>,
}

impl Stmt {
    /// Returns the span of the statement.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Stmt {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let expr = input.try_parse::<Expr>().forward_errors(recoverable_errors)?;
        let semicolon = if let Ok(semi) = input.try_parse::<Semicolon>().forward_errors(recoverable_errors) {
            Some(semi.span.clone())
        } else {
            None
        };
        let stmt_span = if let Some(semicolon) = &semicolon {
            expr.span().start..semicolon.end
        } else {
            expr.span()
        };

        Ok(Stmt {
            expr,
            semicolon,
            span: stmt_span,
        })
    }
}

impl Latex for Stmt {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.expr.fmt_latex(f)?;
        if self.semicolon.is_some() {
            write!(f, ";")?;
        }
        Ok(())
    }
}
