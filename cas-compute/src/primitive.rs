//! Functions to construct [`Integer`]s, [`Float`]s, and [`Complex`] numbers from various types.

use cas_parser::parser::ast::literal::DIGITS;
use rug::{ops::Pow, Assign, Complex, Float, Integer};

/// The number of digits of precision to use when computing values.
pub const PRECISION: u32 = 1 << 9;

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

/// Creates an [`Integer`] from a string slice.
pub fn int_from_str(s: &str) -> Integer {
    Integer::from_str_radix(s, 10).unwrap()
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

/// Parses a number from a string, with the given radix. The radix must be between 2 and 64,
/// inclusive.
///
/// TODO: Replace panics with errors
pub fn from_str_radix(s: &str, radix: u8) -> Integer {
    let mut result = int(0);
    let allowed_digits = &DIGITS[..radix as usize];

    let radix = int(radix);
    for (i, c) in s.chars().rev().enumerate() {
        let digit = int(allowed_digits.iter().position(|&d| d == c).unwrap());
        result += digit * int((&radix).pow(i as u32));
    }

    result
}

/// Creates a [`Complex`] with the given value.
pub fn complex<T>(n: T) -> Complex
where
    Complex: Assign<T>,
{
    Complex::with_val(PRECISION, n)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn radix_eval() {
        let expected = 1072.0;
        let numbers = [
            (2, "10000110000"),
            (8, "2060"),
            (25, "1hm"),
            (32, "11g"),
            (47, "mC"),
        ];

        for (radix, number) in numbers.iter() {
            assert_eq!(from_str_radix(number, *radix), expected);
        }
    }
}
