//! Built-in functions for CalcScript.

pub mod error;

use cas_attrs::args;
use error::BuiltinError;
use rug::ops::Pow;
use super::{
    consts::{ONE, PHI, PI, TEN, float, int_from_float},
    error::kind::{MissingArgument, TooManyArguments, TypeMismatch},
    funcs::factorial as rs_factorial,
    value::Value::{self, *},
};

/// Generates builtin implementations for simple one-argument functions that take a number.
macro_rules! generate_number_builtin {
    ($($name:ident)+) => {
        $(
            #[args(n: Number)]
            pub fn $name(args: &[Value]) -> Result<Value, BuiltinError> {
                Ok(Number(n.$name()))
            }
        )*
    };
}

generate_number_builtin!(
    // trigonometric functions
    sin cos tan csc sec cot
    asin acos atan
    sinh cosh tanh csch sech coth
    asinh acosh atanh

    // exponential and logarithmic functions
    exp ln

    // root / power functions
    sqrt cbrt

    abs
);

// trigonometric functions

#[args(y: Number, x: Number)]
pub fn atan2(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(y.atan2(&x)))
}

// TODO: acsch, asech, acoth

#[args(n: Number)]
pub fn dtr(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n * &*PI / 180.0))
}

#[args(n: Number)]
pub fn rtd(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n * 180.0 / &*PI))
}

// TODO: circle

// exponential and logarithmic functions

#[args(a: Number, b: Number)]
pub fn scientific(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(a * float(&*TEN).pow(b)))
}

#[args(x: Number, y: Number = float(10.0))]
pub fn log(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(x.ln() / y.ln()))
}

// root / power functions

#[args(a: Number, b: Number)]
pub fn hypot(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(a.hypot(&b)))
}

#[args(n: Number, i: Number)]
pub fn root(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n.pow(1.0 / i)))
}

#[args(n: Number, p: Number)]
pub fn pow(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(n.pow(&p)))
}

// sequences

/// Returns the `n`th term of the Fibonacci sequence, using Binet's formula.
#[args(n: Number)]
pub fn fib(args: &[Value]) -> Result<Value, BuiltinError> {
    let result_negative = if n.is_sign_negative() {
        // TODO
        n.to_integer().unwrap().is_even()
    } else {
        false
    };

    let one_minus_phi = float(&*ONE - &*PHI);
    let five_sqrt = float(5.0).sqrt();
    let raw = ((float((&*PHI).pow(&n)) - one_minus_phi.pow(&n)) / five_sqrt).round();

    Ok(Number(if result_negative { -raw } else { raw }))
}

// miscellaneous functions

#[args(n: Number)]
pub fn factorial(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(float(rs_factorial(int_from_float(n)))))
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
        sin cos tan csc sec cot
        asin acos atan atan2
        sinh cosh tanh csch sech coth
        asinh acosh atanh

        // conversion functions
        dtr rtd

        // exponential and logarithmic functions
        exp scientific log ln

        // root / power functions
        hypot sqrt cbrt root pow

        // sequences
        fib

        // miscellaneous functions
        factorial

        abs
    )
}
