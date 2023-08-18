//! Contains the common [`ErrorKind`] trait used by all parsing and evaluation errors to display
//! user-facing error messages.

use ariadne::{Color, Label, Report, ReportKind};
use std::{any::Any, fmt::Debug, ops::Range};

/// The color to use to highlight expressions.
pub const EXPR: Color = Color::RGB(52, 235, 152);

/// Represents any kind of error that can occur during some operation.
pub trait ErrorKind: Debug {
    fn as_any(&self) -> &dyn Any;

    /// The message to show the user when this error occurs.
    fn message(&self) -> String;

    /// The label message for the span of the error.
    fn label(&self) -> String;

    /// The optional help message for the error.
    fn help(&self) -> Option<String> {
        None
    }

    /// Builds the report for this error.
    fn build_report(&self, src_id: &'static str, span: Range<usize>) -> Report<(&'static str, Range<usize>)> {
        let mut builder = Report::build(ReportKind::Error, src_id, span.start)
            .with_message(self.message())
            .with_labels(vec![
                Label::new((src_id, span.clone()))
                    .with_message(self.label())
                    .with_color(EXPR),
            ]);

        if let Some(help) = self.help() {
            builder.set_help(help);
        }

        builder.finish()
    }
}
