use std::ops::Range;
use super::{
    error::{kind::TooManyDerivatives, Error},
    expr::Expr,
    literal::LitSym,
    token::Quote,
    ParenDelimited,
    Parse,
    Parser,
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A function call, such as `func(x, -40)`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Call {
    /// The name of the function to call.
    pub name: LitSym,

    /// The number of derivatives to take before calling the function.
    pub derivatives: u8,

    /// The arguments to the function.
    pub args: Vec<Expr>,

    /// The region of the source code that this function call was parsed from.
    pub span: Range<usize>,

    /// The span of the parentheses that surround the arguments.
    pub paren_span: Range<usize>,
}

impl Call {
    /// Returns the span of the function call.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    /// Returns a set of two spans, where the first is the span of the function name (with the
    /// opening parenthesis) and the second is the span of the closing parenthesis.
    pub fn outer_span(&self) -> [Range<usize>; 2] {
        [
            self.name.span.start..self.paren_span.start + 1,
            self.paren_span.end - 1..self.paren_span.end,
        ]
    }
}

impl<'source> Parse<'source> for Call {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let name = input.try_parse::<LitSym>().forward_errors(recoverable_errors)?;
        let mut derivatives = 0usize;
        let mut quote_span: Option<Range<_>> = None;
        let mut too_many_derivatives = false;

        while let Ok(quote) = input.try_parse::<Quote>().forward_errors(recoverable_errors) {
            if derivatives == u8::MAX.into() {
                too_many_derivatives = true;
            }

            derivatives += 1;
            quote_span = quote_span
                .or_else(|| Some(quote.span.clone()))
                .map(|span| span.start..quote.span.end);
        }

        if too_many_derivatives {
            recoverable_errors.push(Error::new(
                vec![quote_span.unwrap()],
                TooManyDerivatives { derivatives }
            ));
        }

        let surrounded = input.try_parse::<ParenDelimited<_>>().forward_errors(recoverable_errors)?;

        // use `name` here before it is moved into the struct
        let span = name.span.start..surrounded.end.span.end;
        Ok(Self {
            name,
            derivatives: derivatives as u8,
            args: surrounded.value.values,
            span,
            paren_span: surrounded.start.span.start..surrounded.end.span.end,
        })
    }
}
