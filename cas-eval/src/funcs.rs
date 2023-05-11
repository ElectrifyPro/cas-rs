use cas_parser::parser::literal::DIGITS;

/// Computes the factorial of a number.
pub fn factorial(n: f64) -> f64 {
    (2..=(n as u64)).product::<u64>() as f64
}

/// Parses a number from a string, with the given radix. The radix must be between 2 and 64,
/// inclusive.
///
/// TODO: Replace panics with errors
pub fn from_str_radix(s: &str, radix: u8) -> f64 {
    let mut result = 0;
    let allowed_digits = &DIGITS[..radix as usize];

    let radix = radix as u64;
    for (i, c) in s.chars().rev().enumerate() {
        let digit = allowed_digits.iter().position(|&d| d == c).unwrap() as u64;
        result += digit * radix.pow(i as u32);
    }

    result as f64
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
