//! Contains the common [`ErrorKind`] trait used by all parsing and evaluation errors to display
//! user-facing error messages.

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
