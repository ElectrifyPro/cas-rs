use ariadne::{Color, Fmt, Label, Report, ReportKind};
use crate::tokenizer::TokenKind;
use std::ops::Range;

const EXPR: Color = Color::RGB(52, 235, 152);

/// The kind of error that occurred.
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    /// The end of the source code was reached unexpectedly.
    UnexpectedEof,

    /// The end of the source code was expected, but something else was found.
    ExpectedEof,

    /// An unexpected token was encountered.
    UnexpectedToken {
        /// The token(s) that were expected.
        expected: &'static [TokenKind],

        /// The token that was found.
        found: TokenKind,
    },

    /// The base used in radix notation was out of the allowed range.
    ///
    /// If the included boolean is true, the given base is too large. If false, the given base is
    /// too small.
    InvalidRadixBase(bool),

    /// An invalid digit was used in a radix literal.
    InvalidRadixDigit {
        /// The radix that was expected.
        radix: u8,

        /// The set of allowed digits for this radix.
        allowed: &'static [char],

        /// The invalid digit that was used.
        digit: char,
    },

    /// A non-fatal error. If this error is encountered, the parser should try parsing other
    /// branches.
    NonFatal,
}

/// A general parsing error.
#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    /// The region of the source code that this error originated from.
    pub span: Range<usize>,

    /// The kind of error that occurred.
    pub kind: ErrorKind,
}

impl Error {
    /// Creates a new error with the given span and kind.
    pub fn new(span: Range<usize>, kind: ErrorKind) -> Self {
        Self { span, kind }
    }

    /// Build a report from this error. Calling this function on a non-fatal error will return
    /// [`None`].
    pub fn build_report(&self) -> Option<Report<(&'static str, Range<usize>)>> {
        let id = "input";

        match self.kind {
            ErrorKind::UnexpectedEof => Some(Report::build(ReportKind::Error, id, self.span.start)
                .with_message("unexpected end of file")
                .with_labels(vec![
                    Label::new((id, self.span.clone()))
                        .with_message(format!("you might need to add another {} here", "expression".fg(EXPR)))
                        .with_color(EXPR),
                ])
                .finish()),
            ErrorKind::ExpectedEof => Some(Report::build(ReportKind::Error, id, self.span.start)
                .with_message("expected end of file")
                .with_labels(vec![
                    Label::new((id, self.span.clone()))
                        .with_message(format!("I could not understand the remaining {} here", "expression".fg(EXPR)))
                        .with_color(EXPR),
                ])
                .finish()),
            ErrorKind::UnexpectedToken { expected, found } => {
                let expected = expected
                    .iter()
                    .map(|t| format!("{:?}", t))
                    .collect::<Vec<_>>()
                    .join(", ");
                Some(Report::build(ReportKind::Error, id, self.span.start)
                    .with_message(format!("expected one of: {}", expected))
                    .with_labels(vec![
                        Label::new((id, self.span.clone()))
                            .with_message(format!("found {:?}", found))
                            .with_color(Color::Red),
                    ])
                    .finish())
            },
            ErrorKind::InvalidRadixBase(too_big) => Some(Report::build(ReportKind::Error, id, self.span.start)
                .with_message("invalid base in radix notation")
                .with_labels(vec![
                    Label::new((id, self.span.clone()))
                        .with_message(if too_big { "this value is too large" } else { "this value is too small" })
                        .with_color(EXPR),
                ])
                .with_help(format!("the base must be {}", "between 2 and 64, inclusive".fg(EXPR)))
                .finish()
            ),
            ErrorKind::InvalidRadixDigit { radix, allowed, digit } => Some(Report::build(ReportKind::Error, id, self.span.start)
                .with_message(format!("invalid digit in radix notation: `{}`", digit))
                .with_labels(vec![
                    Label::new((id, self.span.clone())).with_color(Color::Red),
                ])
                .with_help(format!("base {} uses these digits (from lowest to highest value): {}", radix, allowed.iter().collect::<String>().fg(EXPR)))
                .finish()
            ),
            ErrorKind::NonFatal => None,
        }
    }
}
