use crate::parser::{
    ast::expr::Expr,
    error::Error,
    fmt::Latex,
    keyword::{Break as BreakToken, Continue as ContinueToken, Loop as LoopToken},
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A `loop` expression, as in `loop { ... }`. The code inside the braces is
/// evaluated repeatedly until a `break` expression is encountered.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Loop {
    /// The body of the loop.
    pub body: Box<Expr>,

    /// The region of the source code that this expression was parsed from.
    pub span: Range<usize>,

    /// The span of the `loop` keyword.
    pub loop_span: Range<usize>,
}

impl Loop {
    /// Returns the span of the `loop` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Loop {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let loop_token = input.try_parse::<LoopToken>().forward_errors(recoverable_errors)?;
        let body = input.try_parse::<Expr>().forward_errors(recoverable_errors)?;
        let span = loop_token.span.start..body.span().end;

        Ok(Self {
            body: Box::new(body),
            span,
            loop_span: loop_token.span,
        })
    }
}

impl std::fmt::Display for Loop {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "loop {}", self.body)
    }
}

impl Latex for Loop {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{loop }}")?;
        self.body.fmt_latex(f)?;
        Ok(())
    }
}

/// A `break` expression, used to exit a loop, optionally with a value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Break {
    /// The value to return from the loop.
    pub value: Option<Box<Expr>>,

    /// The region of the source code that this expression was parsed from.
    pub span: Range<usize>,

    /// The span of the `break` keyword.
    pub break_span: Range<usize>,
}

impl Break {
    /// Returns the span of the `break` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Break {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let break_token = input.try_parse::<BreakToken>().forward_errors(recoverable_errors)?;
        let value = if let Ok(value) = input.try_parse::<Expr>().forward_errors(recoverable_errors) {
            Some(value)
        } else {
            None
        };
        let span = if let Some(value) = &value {
            break_token.span.start..value.span().end
        } else {
            break_token.span.clone()
        };

        Ok(Self {
            value: value.map(Box::new),
            span,
            break_span: break_token.span,
        })
    }
}

impl std::fmt::Display for Break {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "break")?;
        if let Some(value) = &self.value {
            write!(f, " {}", value)?;
        }
        Ok(())
    }
}

impl Latex for Break {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{break }}")?;
        if let Some(value) = &self.value {
            value.fmt_latex(f)?;
        }
        Ok(())
    }
}

/// A `continue` expression, used to skip the rest of a loop iteration.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Continue {
    /// The region of the source code that this expression was parsed from.
    pub span: Range<usize>,
}

impl Continue {
    /// Returns the span of the `continue` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Continue {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        input.try_parse::<ContinueToken>()
            .forward_errors(recoverable_errors)
            .map(|continue_token| Self {
                span: continue_token.span,
            })
    }
}

impl std::fmt::Display for Continue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "continue")
    }
}

impl Latex for Continue {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{continue}}")
    }
}
