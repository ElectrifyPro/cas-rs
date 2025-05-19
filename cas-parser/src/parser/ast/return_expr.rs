use cas_error::Error;
use crate::parser::{
    ast::expr::Expr,
    error::ReturnOutsideFunction,
    fmt::Latex,
    keyword::Return as ReturnToken,
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A `return` expression, used to return a value from a function.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Return {
    /// The value to return from the function.
    pub value: Option<Box<Expr>>,

    /// The region of the source code that this expression was parsed from.
    pub span: Range<usize>,

    /// The span of the `return` keyword.
    pub return_span: Range<usize>,
}

impl Return {
    /// Returns the span of the `return` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Return {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let return_token = input.try_parse::<ReturnToken>().forward_errors(recoverable_errors)?;
        let value = input.try_parse_with_state::<_, Expr>(|state| {
            state.expr_end_at_eol = true;
        }).forward_errors(recoverable_errors).ok();
        let span = if let Some(value) = &value {
            return_token.span.start..value.span().end
        } else {
            return_token.span.clone()
        };

        // `return` expressions can only be used inside functions
        if !input.state.allow_return {
            recoverable_errors.push(Error::new(
                vec![return_token.span.clone()],
                ReturnOutsideFunction,
            ));
        }

        Ok(Self {
            value: value.map(Box::new),
            span,
            return_span: return_token.span,
        })
    }
}

impl std::fmt::Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "return")?;
        if let Some(value) = &self.value {
            write!(f, " {}", value)?;
        }
        Ok(())
    }
}

impl Latex for Return {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{return }}")?;
        if let Some(value) = &self.value {
            value.fmt_latex(f)?;
        }
        Ok(())
    }
}
