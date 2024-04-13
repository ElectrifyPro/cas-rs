use crate::{
    parser::{
        ast::{expr::{Expr, Primary}, helper::Square},
        error::Error,
        fmt::Latex,
        Parser,
    },
    return_if_ok,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// List indexing, such as `list[0]`.
///
/// Indexing expressions do not directly implement [`Parse`] due to performance impliciations
/// involving parsing left-recursive expressions. Instead, the [`Index::parse_or_lower`] method can
/// be used to parse indexing expressions, falling back to other primary expressions if no index
/// expression is found.
///
/// [`Parse`]: crate::parser::Parse
#[derive(Debug, Clone, PartialEq)]
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

    /// [`Primary::std_parse`] without [`Index`].
    pub fn parse_primary(input: &mut Parser, recoverable_errors: &mut Vec<Error>) -> Result<Primary, Vec<Error>> {
        let _ = return_if_ok!(input.try_parse().map(Primary::If).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Primary::Loop).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Primary::While).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Primary::Break).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Primary::Continue).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Primary::Return).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Primary::Call).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Primary::Literal).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Primary::Paren).forward_errors(recoverable_errors));
        input.try_parse().map(Primary::Block).forward_errors(recoverable_errors)
    }

    /// Parses an [`Index`], or any other primary expression.
    pub fn parse_or_lower(
        input: &mut Parser,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Primary, Vec<Error>> {
        let target = Index::parse_primary(input, recoverable_errors)?;
        let surrounded = match input.try_parse::<Square<_>>().forward_errors(recoverable_errors) {
            Ok(surrounded) => surrounded,
            Err(_) => return Ok(target),
        };

        let span = target.span().start..surrounded.close.span.end;
        let mut result = Self {
            target: Box::new(target.into()),
            index: Box::new(surrounded.value),
            span,
            bracket_span: surrounded.open.span.start..surrounded.close.span.end,
        };

        // iteratively search for nested index expressions
        while let Ok(surrounded) = input.try_parse::<Square<_>>().forward_errors(recoverable_errors) {
            let span = result.span.start..surrounded.close.span.end;
            result = Self {
                target: Box::new(Expr::Index(result)),
                index: Box::new(surrounded.value),
                span,
                bracket_span: surrounded.open.span.start..surrounded.close.span.end,
            };
        }

        Ok(Primary::Index(result))
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
