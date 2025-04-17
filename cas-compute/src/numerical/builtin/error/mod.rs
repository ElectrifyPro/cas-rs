pub mod check;
pub mod func_specific;

pub use check::{MissingArgument, TooManyArguments, TypeMismatch};
pub use func_specific::FunctionSpecific;

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
