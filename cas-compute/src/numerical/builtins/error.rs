use cas_error::ErrorKind;
use cas_parser::parser::ast::call::Call;
use crate::numerical::{
    builtins::func_specific::FunctionSpecific,
    error::{kind::{MissingArgument, TooManyArguments, TypeMismatch}, Error},
};

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
