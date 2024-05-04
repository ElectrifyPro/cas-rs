//! Contains the common [`ErrorKind`] trait used by all errors to display user-facing error
//! messages.

use ariadne::{Color, Report};
use std::{fmt::Debug, ops::Range};

/// The color to use to highlight expressions.
pub const EXPR: Color = Color::RGB(52, 235, 152);

/// Represents any kind of error that can occur during some operation.
pub trait ErrorKind: Debug + Send {
    /// Builds the report for this error.
    fn build_report<'a>(
        &self,
        src_id: &'a str,
        spans: &[Range<usize>],
    ) -> Report<(&'a str, Range<usize>)>;
}

/// An error associated with regions of source code that can be highlighted.
#[derive(Debug)]
pub struct Error {
    /// The regions of the source code that this error originated from.
    pub spans: Vec<Range<usize>>,

    /// The kind of error that occurred.
    pub kind: Box<dyn ErrorKind>,
}

impl Error {
    /// Creates a new error with the given spans and kind.
    pub fn new(spans: Vec<Range<usize>>, kind: impl ErrorKind + 'static) -> Self {
        Self { spans, kind: Box::new(kind) }
    }

    /// Build a report from this error kind.
    pub fn build_report<'a>(&self, src_id: &'a str) -> Report<(&'a str, Range<usize>)> {
        self.kind.build_report(src_id, &self.spans)
    }
}
