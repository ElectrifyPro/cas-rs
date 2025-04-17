use crate::parser::{ast::expr::Expr, fmt::Latex};
use std::fmt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Whether a range is inclusive or exclusive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum RangeKind {
    /// A half-open range, `[start, end)`.
    HalfOpen,

    /// A closed range, `[start, end]`.
    Closed,
}

/// A range expression, written as `start..end` for a half-open range (`[start, end)`), or
/// `start..=end` for a closed range (`[start, end]`).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Range {
    /// The start of the range.
    pub start: Box<Expr>,

    /// The end of the range.
    pub end: Box<Expr>,

    /// Whether the range is inclusive or exclusive.
    pub kind: RangeKind,

    /// The region of the source code that this literal was parsed from.
    pub span: std::ops::Range<usize>,
}

impl Range {
    /// Returns the span of the `range` expression.
    pub fn span(&self) -> std::ops::Range<usize> {
        self.span.clone()
    }
}

impl std::fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            RangeKind::HalfOpen => write!(f, "{} .. {}", self.start, self.end),
            RangeKind::Closed => write!(f, "{} ..= {}", self.start, self.end),
        }
    }
}

impl Latex for Range {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            RangeKind::HalfOpen => write!(f, "\\left[{}, {}\\right)", self.start, self.end),
            RangeKind::Closed => write!(f, "\\left[{}, {}\\right]", self.start, self.end),
        }
    }
}
