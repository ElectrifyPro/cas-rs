use cas_parser::parser::literal::DIGITS;
use once_cell::sync::Lazy;
use rand::{Fill, Rng};
use rug::{float::Special, ops::Pow, Complex, Float, Integer};
use super::consts::{ONE, ONE_HALF, PI, TAU, TWO, complex, float, float_from_str, int, int_from_float};

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

/// Computes the number of ways to choose `k` elements from a set of `n` elements.
pub fn choose(n: Integer, k: Integer) -> Integer {
    // TODO: what if k > n?
    let sub = int(&n - &k);
    if k > sub {
        partial_factorial(n, k) / partial_factorial(sub, int(1))
    } else {
        partial_factorial(n, sub) / partial_factorial(k, int(1))
    }
}

/// Binomial probability function.
///
/// Returns the probability of exactly `x` successes occurring in `n` trials, where the probability
/// of success on a single trial is `p`.
pub fn binompdf(n: Integer, p: Float, x: Integer) -> Float {
    let q = float(&*ONE - &p);
    choose(n.clone(), x.clone()) * p.pow(&x) * q.pow(n - x)
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

/// Precomputed values for the inverse error function.
static INV_ERF_A: Lazy<[Float; 4]> = Lazy::new(|| [
    float_from_str("0.886226899"),
    float_from_str("-1.645349621"),
    float_from_str("0.914624893"),
    float_from_str("-0.140543331"),
]);

static INV_ERF_B: Lazy<[Float; 5]> = Lazy::new(|| [
    float(&*ONE),
    float_from_str("-2.118377725"),
    float_from_str("1.442710462"),
    float_from_str("-0.329097515"),
    float_from_str("0.012229801"),
]);

static INV_ERF_C: Lazy<[Float; 4]> = Lazy::new(|| [
    float_from_str("-1.970840454"),
    float_from_str("-1.62490649"),
    float_from_str("3.429567803"),
    float_from_str("1.641345311"),
]);

static INV_ERF_D: Lazy<[Float; 3]> = Lazy::new(|| [
    float(&*ONE),
    float_from_str("3.543889200"),
    float_from_str("1.637067800"),
]);

/// Computes the inverse error function of a number.
pub fn inverse_error(x: Float) -> Float {
    if x <= float(-1) {
        return float(Special::NegInfinity);
    } else if x >= float(1) {
        return float(Special::Infinity);
    }

    // implementation from http://www.mimirgames.com/articles/programming/approximations-of-the-inverse-error-function/
    let mut z = float(x.abs_ref());

    let fold_arr = |arr: &[Float], factor: &Float| {
        (1..arr.len())
            .rev()
            .fold(float(&arr[arr.len() - 1]), |acc, i| {
                acc * factor + float(&arr[i - 1])
            })
    };

    let mut r = if z <= 0.7 {
        let x2 = float(z.square_ref());
        let num = &z * fold_arr(&*INV_ERF_A, &x2);
        let den = fold_arr(&*INV_ERF_B, &x2);
        num / den
    } else {
        let y = {
            let inner_log = (&*ONE - float(&z)) / &*TWO;
            (-inner_log.ln()).sqrt()
        };
        let num = fold_arr(&*INV_ERF_C, &y);
        let den = fold_arr(&*INV_ERF_D, &y);
        num / den
    };

    r = if x.is_sign_negative() { -r } else { r };
    z = if x.is_sign_negative() { -z } else { z };

    // double newton's method
    let f = 2 / float(&*PI).sqrt();
    r -= (float(r.erf_ref()) - &z) / (&f * float((-float(r.square_ref())).exp_ref()));
    r -= (float(r.erf_ref()) - &z) / (&f * float((-float(r.square_ref())).exp_ref()));

    r
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

/// Fill the given slice with random bytes.
pub fn fill_random<T: Fill>(bytes: &mut T) {
    rand::thread_rng().fill(bytes);
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
