//! Contains the common [`ErrorKind`] trait used by all parsing and evaluation errors to display
//! user-facing error messages.

use ariadne::{Color, Report};
use std::{any::Any, fmt::Debug, ops::Range};

/// The color to use to highlight expressions.
pub const EXPR: Color = Color::RGB(52, 235, 152);

/// Represents any kind of error that can occur during some operation.
pub trait ErrorKind: Debug {
    fn as_any(&self) -> &dyn Any;

    /// Builds the report for this error.
    fn build_report(
        &self,
        src_id: &'static str,
        spans: &[Range<usize>],
    ) -> Report<(&'static str, Range<usize>)>;
}
