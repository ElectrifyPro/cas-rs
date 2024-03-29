pub mod kind;

use ariadne::Report;
use cas_error::ErrorKind;
use std::ops::Range;

/// A general evaluation error.
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
    pub fn build_report(&self) -> Report<(&'static str, Range<usize>)> {
        self.kind.build_report("input", &self.spans)
    }
}
