use crate::parser::{
    ast::expr::Expr,
    error::Error,
    fmt::Latex,
    keyword::While as WhileToken,
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A `while` loop expression, such as `while x < 10 then x += 1`. The loop body is executed
/// repeatedly as long as the outer condition is true.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct While {
    /// The condition that must be true for the loop body to be executed.
    pub condition: Box<Expr>,

    /// The body of the loop.
    pub body: Box<Expr>,

    /// The region of the source code that this expression was parsed from.
    pub span: Range<usize>,

    /// The span of the `while` keyword.
    pub while_span: Range<usize>,
}

impl While {
    /// Returns the span of the `while` loop expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for While {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let while_token = input.try_parse::<WhileToken>().forward_errors(recoverable_errors)?;
        let condition = input.try_parse::<Expr>().forward_errors(recoverable_errors)?;
        let then_body = input.try_parse_with_state::<_, Expr>(|state| {
            state.allow_then = true;
            state.allow_loop_control = true;
        }).forward_errors(recoverable_errors)?;
        let span = while_token.span.start..then_body.span().end;

        Ok(Self {
            condition: Box::new(condition),
            body: Box::new(then_body),
            span,
            while_span: while_token.span,
        })
    }
}

impl std::fmt::Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "while {} {}", self.condition, self.body)
    }
}

impl Latex for While {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{while }}")?;
        self.condition.fmt_latex(f)?;
        self.body.fmt_latex(f)?;
        Ok(())
    }
}
