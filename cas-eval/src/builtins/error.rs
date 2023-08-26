use cas_attrs::ErrorKind;
use cas_error::ErrorKind;
use cas_parser::parser::call::Call;
use crate::error::{kind::{MissingArgument, TooManyArguments, TypeMismatch}, Error};
use std::ops::Range;

/// Represents an error that can occur while evaluating a builtin function.
#[derive(Debug)]
pub enum BuiltinError {
    /// The function was called with too many arguments.
    TooManyArguments(TooManyArguments),

    /// An argument to the function call is missing.
    MissingArgument(MissingArgument),

    /// The function was called with a mismatched argument type.
    TypeMismatch(TypeMismatch),

    /// A function specific error.
    FunctionSpecific(FunctionSpecific),
}

impl BuiltinError {
    /// Convert the [`BuiltinError`] into an [`Error`], using the given function call to provide
    /// spans.
    pub fn into_error(self, call: &Call) -> Error {
        match self {
            BuiltinError::TooManyArguments(e) => Error {
                spans: call.outer_span().to_vec(),
                kind: Box::new(e) as Box<dyn ErrorKind>,
            },
            BuiltinError::MissingArgument(e) => Error {
                spans: call.outer_span().to_vec(),
                kind: Box::new(e) as Box<dyn ErrorKind>,
            },
            BuiltinError::TypeMismatch(e) => {
                let mut this_function_call = call.outer_span().to_vec();
                this_function_call.extend([call.args[e.index].span()]);
                Error {
                    spans: this_function_call,
                    kind: Box::new(e) as Box<dyn ErrorKind>,
                }
            },
            BuiltinError::FunctionSpecific(e) => Error {
                spans: e.spans(call),
                kind: e.into_kind(),
            },
        }
    }
}

/// Represents an error specific to a builtin function.
#[derive(Debug)]
pub enum FunctionSpecific {
    /// Errors for the `ncr` builtin function.
    Ncr(NcrError),
}

impl FunctionSpecific {
    /// Get the spans for the error.
    fn spans(&self, call: &Call) -> Vec<Range<usize>> {
        match self {
            FunctionSpecific::Ncr(e) => e.spans(call),
        }
    }

    /// Convert the [`FunctionSpecific`] into an [`ErrorKind`].
    fn into_kind(self) -> Box<dyn ErrorKind> {
        match self {
            FunctionSpecific::Ncr(e) => Box::new(e),
        }
    }
}

/// Errors for the `ncr` builtin function.
#[derive(Debug, Clone, Copy, ErrorKind, PartialEq, Eq)]
#[error(
    message = "incorrect arguments for the `ncr` function",
    labels = if *self == NcrError::NLessThanK {
        (&[
            "this function call",
            "",
            "(1) argument `n`...",
            "(2) ...must be less than or equal to argument `k`",
        ]).into_iter()
    } else {
        (&[
            "this function call",
            "",
            "argument `k` must be positive",
        ]).into_iter()
    }
)]
pub enum NcrError {
    /// The first argument is less than the second.
    NLessThanK,

    /// One or both of the arguments is negative.
    NegativeArgs,
}

impl NcrError {
    fn spans(&self, call: &Call) -> Vec<Range<usize>> {
        let mut this_function_call = call.outer_span().to_vec();
        match self {
            NcrError::NLessThanK => {
                this_function_call.extend(call.args.iter().map(|arg| arg.span()));
            },
            NcrError::NegativeArgs => {
                this_function_call.push(call.args[1].span());
            },
        };
        this_function_call
    }
}

impl From<NcrError> for BuiltinError {
    fn from(e: NcrError) -> Self {
        BuiltinError::FunctionSpecific(FunctionSpecific::Ncr(e))
    }
}
