//! Built-in functions for CalcScript.

pub mod error;
pub mod func_specific;

use cas_attrs::args;
use error::BuiltinError;
use rug::ops::Pow;
use super::{
    builtins::func_specific::{NcprError, NcprErrorKind},
    consts::{ONE, PHI, PI, TEN, complex, float, int, int_from_float},
    error::kind::{MissingArgument, TooManyArguments, TypeMismatch},
    funcs::{factorial as rs_factorial, partial_factorial},
    value::Value::{self, *},
};

/// Generates builtin implementations for simple one-argument functions that take a complex number.
macro_rules! generate_complex_builtin {
    ($($name:ident)+) => {
        $(
            #[args(n: Complex)]
            pub fn $name(args: &[Value]) -> Result<Value, BuiltinError> {
                Ok(Complex(n.$name()))
            }
        )*
    };
}

generate_complex_builtin!(
    // trigonometric functions
    sin cos tan // csc sec cot
    asin acos atan // acsc asec acot
    sinh cosh tanh // csch sech coth
    asinh acosh atanh // acsch asech acoth

    // exponential and logarithmic functions
    exp ln

    // root / power functions
    sqrt // cbrt

    abs
);

// trigonometric functions

#[args(n: Complex)]
pub fn csc(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.sin().recip()))
}

#[args(n: Complex)]
pub fn sec(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.cos().recip()))
}

#[args(n: Complex)]
pub fn cot(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.tan().recip()))
}

#[args(n: Complex)]
pub fn acsc(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().asin()))
}

#[args(n: Complex)]
pub fn asec(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().acos()))
}

#[args(n: Complex)]
pub fn acot(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().atan()))
}

#[args(y: Number, x: Number)]
pub fn atan2(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(y.atan2(&x)))
}

#[args(n: Complex)]
pub fn csch(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.sinh().recip()))
}

#[args(n: Complex)]
pub fn sech(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.cosh().recip()))
}

#[args(n: Complex)]
pub fn coth(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.tanh().recip()))
}

#[args(n: Complex)]
pub fn acsch(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().asinh()))
}

#[args(n: Complex)]
pub fn asech(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().acosh()))
}

#[args(n: Complex)]
pub fn acoth(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.recip().atanh()))
}

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

#[args(a: Complex, b: Complex)]
pub fn scientific(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(a * complex(&*TEN).pow(b)))
}

#[args(x: Complex, y: Complex = complex(&*TEN))]
pub fn log(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(x.ln() / y.ln()))
}

// root / power functions

#[args(a: Number, b: Number)]
pub fn hypot(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(a.hypot(&b)))
}

#[args(n: Complex)]
pub fn cbrt(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.pow(1.0 / 3.0)))
}

#[args(n: Complex, i: Complex)]
pub fn root(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.pow(&i.recip())))
}

#[args(n: Complex, p: Complex)]
pub fn pow(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(n.pow(&p)))
}

// complex numbers

#[args(z: Complex)]
pub fn re(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(z.into_real_imag().0))
}

#[args(z: Complex)]
pub fn im(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(z.into_real_imag().1))
}

#[args(z: Complex)]
pub fn arg(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Number(z.arg().into_real_imag().0))
}

#[args(z: Complex)]
pub fn conj(args: &[Value]) -> Result<Value, BuiltinError> {
    Ok(Complex(z.conj()))
}

// statistics

#[args(n: Number, k: Number)]
pub fn ncr(args: &[Value]) -> Result<Value, BuiltinError> {
    if &n < &k {
        return Err(NcprError::new("ncr", NcprErrorKind::NLessThanK).into());
    } else if k.is_sign_negative() {
        // if k is positive, then n is also positive
        return Err(NcprError::new("ncr", NcprErrorKind::NegativeArgs).into());
    }

    let (n, k) = (n.to_integer().unwrap(), k.to_integer().unwrap());
    let sub = int(&n - &k);

    if &k > &sub {
        Ok(Number(float(partial_factorial(n, k) / rs_factorial(sub))))
    } else {
        Ok(Number(float(partial_factorial(n, sub) / rs_factorial(k))))
    }
}

#[args(n: Number, k: Number)]
pub fn npr(args: &[Value]) -> Result<Value, BuiltinError> {
    if &n < &k {
        return Err(NcprError::new("npr", NcprErrorKind::NLessThanK).into());
    } else if k.is_sign_negative() {
        // if k is positive, then n is also positive
        return Err(NcprError::new("npr", NcprErrorKind::NegativeArgs).into());
    }

    let (n, k) = (n.to_integer().unwrap(), k.to_integer().unwrap());
    let sub = int(&n - &k);
    Ok(Number(float(partial_factorial(n, sub))))
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
        asin acos atan atan2 acsc asec acot
        sinh cosh tanh csch sech coth
        asinh acosh atanh acsch asech acoth

        // conversion functions
        dtr rtd

        // exponential and logarithmic functions
        exp scientific log ln

        // root / power functions
        hypot sqrt cbrt root pow

        // complex numbers
        re im arg conj

        // sequences
        fib

        // statistics
        ncr npr

        // miscellaneous functions
        factorial

        abs
    )
}
