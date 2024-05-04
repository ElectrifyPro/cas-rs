use cas_error::Error;
use crate::parser::{
    ast::{expr::{Expr, Primary}, helper::Square},
    fmt::Latex,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// List indexing, such as `list[0]`.
///
/// [`Parse`]: crate::parser::Parse
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Index {
    /// The expression being indexed.
    pub target: Box<Expr>,

    /// The amount to index by.
    pub index: Box<Expr>,

    /// The region of the source code that this function call was parsed from.
    pub span: Range<usize>,

    /// The span of the brackets that surround the index expression.
    pub bracket_span: Range<usize>,
}

impl Index {
    /// Returns the span of the index expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    /// Returns a set of two spans, where the first is the span of the expression being indexed
    /// (with the opening bracket) and the second is the span of the closing bracket.
    pub fn outer_span(&self) -> [Range<usize>; 2] {
        [
            self.target.span().start..self.bracket_span.start + 1,
            self.bracket_span.end - 1..self.bracket_span.end,
        ]
    }

    /// Attempts to parse an [`Index`], where the initial target has already been parsed.
    pub fn parse_or_lower(
        input: &mut Parser,
        recoverable_errors: &mut Vec<Error>,
        mut target: Primary,
    ) -> Primary {
        // iteratively search for nested index expressions
        while let Ok(surrounded) = input.try_parse::<Square<_>>().forward_errors(recoverable_errors) {
            let span = target.span().start..surrounded.close.span.end;
            target = Primary::Index(Self {
                target: Box::new(target.into()),
                index: Box::new(surrounded.value),
                span,
                bracket_span: surrounded.open.span.start..surrounded.close.span.end,
            });
        }

        target
    }
}

impl std::fmt::Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}[{}]", self.target, self.index)
    }
}

impl Latex for Index {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}_{{{}}}", self.target, self.index)
    }
}
