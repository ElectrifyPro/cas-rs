//! Built-in functions for CalcScript.

mod error;

use cas_attrs::args;
use error::BuiltinError;
use super::{
    error::kind::{MissingArgument, TooManyArguments, TypeMismatch},
    value::Value::{self, *},
};

/// Generates builtin implementations for simple one-argument functions that take a number.
macro_rules! generate_number_builtin {
    ($($name:ident)+) => {
        $(
            #[args(Number(n))]
            pub fn $name(args: &[Value]) -> Result<Value, BuiltinError> {
                Ok(Number(n.$name()))
            }
        )*
    };
}

generate_number_builtin!(
    // trigonometric functions
    sin cos tan asin acos atan

    abs
);

#[args(Number(y), Number(x))]
pub fn atan2(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(y.atan2(*x)))
}

#[args(Number(n))]
pub fn csc(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(1.0 / n.sin()))
}

#[args(Number(n))]
pub fn sec(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(1.0 / n.cos()))
}

#[args(Number(n))]
pub fn cot(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(1.0 / n.tan()))
}

#[args(Number(n))]
pub fn acsc(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number((1.0 / n).asin()))
}

#[args(Number(n))]
pub fn asec(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number((1.0 / n).acos()))
}

#[args(Number(n))]
pub fn acot(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number((1.0 / n).atan()))
}

/// Returns the builtin function with the given name.
pub fn get_builtin(name: &str) -> Option<fn(&[Value]) -> Result<Value, BuiltinError>> {
    macro_rules! match_builtin {
        ($($name:ident)+) => {
            match name {
                $(
                    stringify!($name) => Some($name),
                )*
                _ => None,
            }
        };
    }

    match_builtin!(
        // trigonometric functions
        sin cos tan asin acos atan atan2
        csc sec cot acsc asec acot

        abs
    )
}
