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
    /// [`Report`]: https://docs.rs/ariadne/latest/ariadne/struct.Report.html
    pub fn report_to_stderr(&self, src_id: &str, input: &str) {
        match self {
            Self::Many(errs) => errs.iter().for_each(|err| err.report_to_stderr(src_id, input).unwrap()),
            Self::One(err) => err.report_to_stderr(src_id, input).unwrap(),
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
