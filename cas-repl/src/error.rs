use ariadne::Source;
use cas_compute::numerical::error::Error as EvalError;
use cas_parser::parser::error::Error as ParseError;

/// Utility enum to package errors that can occur while parsing / evaluating.
pub enum Error {
    /// Errors that occurred while parsing.
    ParseError(Vec<ParseError>),

    /// An error that occurred while evaluating.
    EvalError(EvalError),
}

impl Error {
    /// Report the errors in this [`Error`] to stderr.
    ///
    /// The `ariadne` crate's [`Report`] type actually does not have a `Display` implementation, so
    /// we can only use its `eprint` method to print to stderr.
    pub fn report_to_stderr(&self, input: &str) {
        match self {
            Self::ParseError(errs) => errs.iter().for_each(|err| {
                let report = err.build_report();
                report.eprint(("input", Source::from(input))).unwrap();
            }),
            Self::EvalError(err) => std::iter::once(err).for_each(|err| {
                let report = err.build_report();
                report.eprint(("input", Source::from(input))).unwrap();
            }),
        }
    }
}

impl From<Vec<ParseError>> for Error {
    fn from(errs: Vec<ParseError>) -> Self {
        Self::ParseError(errs)
    }
}

impl From<EvalError> for Error {
    fn from(err: EvalError) -> Self {
        Self::EvalError(err)
    }
}
