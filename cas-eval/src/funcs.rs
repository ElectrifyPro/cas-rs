use cas_parser::parser::literal::DIGITS;
use rug::{ops::Pow, Integer};
use super::consts::int;

/// Computes the factorial of a number.
pub fn factorial(n: Integer) -> Integer {
    partial_factorial(n, int(1))
}

/// Computes a partial factorial of a number from `n` to `k`, where `k` is exclusive (i.e. `n * (n
/// - 1) * ... * (k + 1)`).
pub fn partial_factorial(mut n: Integer, k: Integer) -> Integer {
    let one = int(1);
    let mut result = int(1);
    while n > k {
        result *= &n;
        n -= &one;
    }
    result
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
