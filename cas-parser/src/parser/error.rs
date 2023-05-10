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
                        .with_message(format!("you might need to remove this {} here", "expression".fg(EXPR)))
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
            ErrorKind::NonFatal => None,
        }
    }
}
