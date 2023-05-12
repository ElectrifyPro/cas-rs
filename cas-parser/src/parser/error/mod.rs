pub mod kind;

use ariadne::Report;
use kind::ErrorKind;
use std::ops::Range;

/// A general parsing error.
#[derive(Debug)]
pub struct Error {
    /// The region of the source code that this error originated from.
    pub span: Range<usize>,

    /// The kind of error that occurred.
    pub kind: Box<dyn ErrorKind>,

    /// Whether this error is fatal. Fatal errors will cause the parser to stop parsing and return
    /// the error. Non-fatal errors will generally be ignored.
    ///
    /// If it is unknown whether an error is fatal or not, it is recommended to assume that it is
    /// not, as non-fatal errors can be upgraded to fatal errors later.
    pub fatal: bool,
}

impl Error {
    /// Creates a new non-fatal error with the given span and kind.
    pub fn new(span: Range<usize>, kind: impl ErrorKind + 'static) -> Self {
        Self { span, kind: Box::new(kind), fatal: false }
    }

    /// Creates a new fatal error with the given span and kind.
    pub fn new_fatal(span: Range<usize>, kind: impl ErrorKind + 'static) -> Self {
        Self { span, kind: Box::new(kind), fatal: true }
    }

    /// Build a report from this error kind.
    pub fn build_report(&self) -> Report<(&'static str, Range<usize>)> {
        self.kind.build_report("input", self.span.clone())
    }
}
