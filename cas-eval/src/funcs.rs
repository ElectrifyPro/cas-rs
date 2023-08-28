use cas_parser::parser::literal::DIGITS;
use once_cell::sync::Lazy;
use rug::{ops::Pow, Complex, Float, Integer};
use super::consts::{ONE, ONE_HALF, TAU, complex, float, float_from_str, int, int_from_float};

/// Computes the factorial of a number.
pub fn factorial(n: Float) -> Float {
    if !n.is_integer() || n.is_sign_negative() {
        (n + &*ONE).gamma()
    } else {
        float(partial_factorial(int_from_float(n), int(1)))
    }
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

static GAMMA_P: Lazy<[Float; 9]> = Lazy::new(|| [
    float_from_str("0.99999999999980993"),
    float_from_str("676.5203681218851"),
    float_from_str("-1259.1392167224028"),
    float_from_str("771.32342877765313"),
    float_from_str("-176.61502916214059"),
    float_from_str("12.507343278686905"),
    float_from_str("-0.13857109526572012"),
    float_from_str("9.9843695780195716e-6"),
    float_from_str("1.5056327351493116e-7"),
]);

static GAMMA_G: Lazy<Float> = Lazy::new(|| float(7));

/// Computes the gamma function of a complex number.
pub fn gamma(mut z: Complex) -> Complex {
    // implementation from https://en.wikipedia.org/wiki/Lanczos_approximation#Simple_implementation
    if z.imag().is_zero() {
        return complex(z.into_real_imag().0.gamma());
    }

    z -= &*ONE;

    let mut x = complex(&GAMMA_P[0]);
    for (i, p) in GAMMA_P.iter().enumerate().skip(1) {
        x += p / complex(&z + i);
    }

    let t = complex(complex(&z + &*GAMMA_G) + float(&*ONE_HALF));

    let tau_sqrt = float(&*TAU).sqrt();
    let t_pow = (&t).pow(complex(&z + &*ONE_HALF));
    let exp_t = complex(t.as_neg().exp_ref());

    x * t_pow * exp_t * tau_sqrt
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
