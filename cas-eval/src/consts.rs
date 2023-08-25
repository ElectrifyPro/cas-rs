//! Additional constants used in the library. This module consists of static constants that return
//! [`Float`]s with the given value.

use once_cell::sync::Lazy;
use rug::{Assign, Complex, Float, Integer};

/// The number of digits of precision to use when computing values.
pub const PRECISION: u32 = 256;

/// Creates an [`Integer`] with the given value.
pub fn int<T>(n: T) -> Integer
where
    Integer: From<T>,
{
    Integer::from(n)
}

/// Creates an [`Integer`] from a [`Float`] by truncating the fractional part.
pub fn int_from_float(f: Float) -> Integer {
    // TODO: can panic if NaN
    f.trunc().to_integer().unwrap()
}

/// Creates a [`Float`] with the given value.
pub fn float<T>(n: T) -> Float
where
    Float: Assign<T>,
{
    Float::with_val(PRECISION, n)
}

/// Creates a [`Float`] from a string slice.
pub fn float_from_str(s: &str) -> Float {
    Float::with_val(PRECISION, Float::parse(s).unwrap())
}

/// Creates a [`Complex`] with the given value.
pub fn complex<T>(n: T) -> Complex
where
    Complex: Assign<T>,
{
    Complex::with_val(PRECISION, n)
}

pub static ZERO: Lazy<Float> = Lazy::new(|| float(0));

pub static ONE: Lazy<Float> = Lazy::new(|| float(1));

pub static TEN: Lazy<Float> = Lazy::new(|| float(10));

/// The imaginary unit.
pub static I: Lazy<Complex> = Lazy::new(|| complex((0, 1)));

/// Euler's number.
pub static E: Lazy<Float> = Lazy::new(|| {
    float_from_str(
        "2.71828182845904523536028747135266249775724709369995957496696762772407663035354",
    )
});

/// The golden ratio.
pub static PHI: Lazy<Float> = Lazy::new(|| (float(1) + float(5).sqrt()) / float(2));

pub static PI: Lazy<Float> = Lazy::new(|| {
    float_from_str(
        "3.141592653589793238462643383279502884197169399375105820974944592307816406286",
    )
});

pub static TAU: Lazy<Float> = Lazy::new(|| float(2) * &*PI);
