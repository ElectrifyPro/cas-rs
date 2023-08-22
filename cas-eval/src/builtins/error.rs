use cas_error::ErrorKind;
use cas_parser::parser::call::Call;
use crate::error::{kind::{MissingArgument, TooManyArguments, TypeMismatch}, Error};

/// Represents an error that can occur while evaluating a builtin function.
#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinError {
    /// The function was called with too many arguments.
    TooManyArguments(TooManyArguments),

    /// An argument to the function call is missing.
    MissingArgument(MissingArgument),

    /// The function was called with a mismatched argument type.
    TypeMismatch(TypeMismatch),
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
        }
    }
}
