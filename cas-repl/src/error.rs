use ariadne::Source;
use cas_error::Error;

/// Utility enum to package one or multiple errors.
pub enum ReplError {
    /// Multiple errors that can occur during parsing.
    Many(Vec<Error>),

    /// Single error that can occur during compilation or evaluation.
    One(Error),
}

impl ReplError {
    /// Report the errors in this [`ReplError`] to stderr.
    ///
    /// The `ariadne` crate's [`Report`] type actually does not have a `Display` implementation, so
    /// we can only use its `eprint` method to print to stderr.
    ///
    /// [`Report`]: ariadne::Report
    pub fn report_to_stderr(&self, src_id: &str, input: &str) {
        let do_one = |err: &Error| {
            let report = err.build_report(src_id);
            report.eprint((src_id, Source::from(input))).unwrap();
        };
        match self {
            Self::Many(errs) => errs.iter().for_each(do_one),
            Self::One(err) => do_one(err),
        }
    }
}

impl From<Vec<Error>> for ReplError {
    fn from(errs: Vec<Error>) -> Self {
        Self::Many(errs)
    }
}

impl From<Error> for ReplError {
    fn from(err: Error) -> Self {
        Self::One(err)
    }
}
