use cas_error::Error;
use crate::parser::{
    ast::{
        expr::Expr,
        literal::LitSym,
        range::Range as RangeExpr,
    },
    fmt::Latex,
    keyword::{In as InToken, For as ForToken},
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A `for` loop expression, such as `for i in 0..10 then print(i)`. The loop body is executed for
/// each value in the specified range, with the variable taking on the current value.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct For {
    /// The variable name representing each value in the range.
    pub variable: LitSym,

    /// The range of values that the variable will take on.
    pub range: RangeExpr,

    /// The body of the summation.
    pub body: Box<Expr>,

    /// The region of the source code that this `for` expression was parsed from.
    pub span: Range<usize>,
}

impl For {
    /// Returns the span of the `for` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for For {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let for_token = input.try_parse::<ForToken>().forward_errors(recoverable_errors)?;
        let variable = input.try_parse::<LitSym>().forward_errors(recoverable_errors)?;
        input.try_parse::<InToken>().forward_errors(recoverable_errors)?;
        let range = input.try_parse::<RangeExpr>().forward_errors(recoverable_errors)?;
        let body = input.try_parse_with_state::<_, Expr>(|state| {
            state.allow_then = true;
            state.allow_loop_control = true;
        }).forward_errors(recoverable_errors)?;
        let span = for_token.span.start..body.span().end;

        Ok(Self {
            variable,
            range,
            body: Box::new(body),
            span,
        })
    }
}

impl std::fmt::Display for For {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "for {} in {} {}", self.variable, self.range, self.body)
    }
}

impl Latex for For {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\\forall {} \\in {} \\text{{ do }} {}",
            self.variable,
            self.range,
            self.body
        )
    }
}
