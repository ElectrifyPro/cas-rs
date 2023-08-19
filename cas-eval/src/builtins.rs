//! Built-in functions for CalcScript.

use cas_attrs::args;
use cas_error::ErrorKind;
use cas_parser::parser::call::Call;
use super::{
    error::{kind::{MissingArgument, TooManyArguments, TypeMismatch}, Error},
    value::Value::{self, *},
};

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
                spans: vec![call.span.clone()],
                kind: Box::new(e) as Box<dyn ErrorKind>,
            },
            BuiltinError::MissingArgument(e) => Error {
                spans: vec![call.span.clone()],
                kind: Box::new(e) as Box<dyn ErrorKind>,
            },
            BuiltinError::TypeMismatch(e) => Error {
                spans: vec![call.span.clone(), call.args[e.index].span()],
                kind: Box::new(e) as Box<dyn ErrorKind>,
            },
        }
    }
}

/// Returns the absolute value of a value.
#[args(Number(n))]
pub fn abs(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n.abs()))
}

/// Returns the builtin function with the given name.
pub fn get_builtin(name: &str) -> Option<fn(&[Value]) -> Result<Value, BuiltinError>> {
    match name {
        "abs" => Some(abs),
        _ => None,
    }
}
