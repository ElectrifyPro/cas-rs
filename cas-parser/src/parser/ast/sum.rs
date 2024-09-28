use cas_error::Error;
use crate::parser::{
    ast::{
        expr::Expr,
        literal::LitSym,
        range::Range as RangeExpr,
    },
    fmt::Latex,
    keyword::{In as InToken, Sum as SumToken},
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

/// A sum expression, such as `sum n in 1..10 of n`.
///
/// A sum expression is a shortcut for a loop that represents a summation. The final expression is
/// summed over the specified range, with a specific variable name taking on each value in the
/// range. The above example is equivalent to the following code:
///
/// ```calcscript
/// out = 0
/// for n in 1..10 {
///     out += n
/// }
/// out
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Sum {
    /// The variable name representing each value in the range.
    pub variable: LitSym,

    /// The range of values that the variable will take on.
    pub range: RangeExpr,

    /// The body of the summation.
    pub body: Box<Expr>,

    /// The region of the source code that this `sum` expression was parsed from.
    pub span: Range<usize>,
}

impl Sum {
    /// Returns the span of the `sum` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Sum {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let sum_token = input.try_parse::<SumToken>().forward_errors(recoverable_errors)?;
        let variable = input.try_parse::<LitSym>().forward_errors(recoverable_errors)?;
        input.try_parse::<InToken>().forward_errors(recoverable_errors)?;
        let range = input.try_parse::<RangeExpr>().forward_errors(recoverable_errors)?;
        let body = input.try_parse_with_state::<_, Expr>(|state| {
            state.allow_of = true;
        }).forward_errors(recoverable_errors)?;
        let span = sum_token.span.start..body.span().end;

        Ok(Self {
            variable,
            range,
            body: Box::new(body),
            span,
        })
    }
}

impl std::fmt::Display for Sum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "sum {} in {} {}", self.variable, self.range, self.body)
    }
}

impl Latex for Sum {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\\sum_{{{}={}}}^{{{}}} {}",
            self.variable,
            self.range.start,
            self.range.end,
            self.body,
        )
    }
}
