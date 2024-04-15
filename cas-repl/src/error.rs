use ariadne::Source;
use cas_compiler::error::Error as CompileError;
use cas_parser::parser::error::Error as ParseError;
use cas_vm::{error::Error as EvalError, ReplVmError};

/// Utility enum to package errors that can occur while parsing / evaluating.
pub enum Error {
    /// Errors that occurred while parsing.
    ParseError(Vec<ParseError>),

    /// Error that occurred during compilation.
    CompileError(CompileError),

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
            Self::CompileError(err) => {
                let report = err.build_report();
                report.eprint(("input", Source::from(input))).unwrap();
            },
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

impl From<CompileError> for Error {
    fn from(err: CompileError) -> Self {
        Self::CompileError(err)
    }
}

impl From<EvalError> for Error {
    fn from(err: EvalError) -> Self {
        Self::EvalError(err)
    }
}

impl From<ReplVmError> for Error {
    fn from(err: ReplVmError) -> Self {
        match err {
            ReplVmError::Compile(err) => Self::CompileError(err),
            ReplVmError::Eval(err) => Self::EvalError(err),
        }
    }
}
