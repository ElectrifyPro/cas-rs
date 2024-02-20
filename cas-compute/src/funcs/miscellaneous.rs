//! Uncategorized functions.

use cas_attrs::builtin;
use crate::consts::TAU;
use crate::numerical::value::Value;
use crate::primitive::{complex, float_from_str, float, int};
use once_cell::sync::Lazy;
use rand::Rng;
use rug::{integer::Order, ops::Pow, rand::RandState, Complex, Float, Integer};

/// Returns the absolute value.
#[derive(Debug)]
pub struct Abs;

#[cfg_attr(feature = "numerical", builtin)]
impl Abs {
    pub fn eval_static(v: Complex) -> Float {
        v.abs().into_real_imag().0
    }
}

/// Returns `true` if the given value is "truthy".
///
/// For each type, the following values are considered "truthy":
///
/// - `Float`: any value except `0.0` and `NaN`
/// - `Integer`: any value except `0`
/// - `Complex`: any value except `0.0 + 0.0i` and `NaN + NaNi`
/// - `Bool`: `true`
/// - `Unit`: never
#[derive(Debug)]
pub struct Bool;

#[cfg_attr(feature = "numerical", builtin)]
impl Bool {
    pub fn eval_static(v: Value) -> bool {
        v.is_truthy()
    }
}

/// Returns a random floating-point number between `0.0` and `1.0`.
#[derive(Debug)]
pub struct Rand;

#[cfg_attr(feature = "numerical", builtin)]
impl Rand {
    pub fn eval_static() -> Float {
        let mut seed = Integer::new();
        let mut digits = [0u128; 2]; // 256 bits
        rand::thread_rng().fill(&mut digits);
        seed.assign_digits(&digits, Order::Lsf);

        let mut rand_state = RandState::new();
        rand_state.seed(&seed);
        float(Float::random_bits(&mut rand_state))
    }
}

/// Computes a partial factorial of an integer from `n` to `k`, where `k` is exclusive (i.e. `n * (n
/// - 1) * ... * (k + 1)`).
pub fn partial_factorial(mut n: Integer, k: Integer) -> Integer {
    let mut result = int(1);
    while n > k {
        result *= &n;
        n -= 1;
    }
    result
}

/// The factorial function, extended to support floating-point values as well as integers.
#[derive(Debug)]
pub struct Factorial;

#[cfg_attr(feature = "numerical", builtin)]
impl Factorial {
    pub fn eval_static(n: Float) -> Value {
        if !n.is_integer() || n.is_sign_negative() {
            Value::Float((n + 1u8).gamma())
        } else {
            // returning an integer in this case is more efficient and provides better precision
            let n = n.to_integer().unwrap();
            Value::Integer(partial_factorial(n, int(1)))
        }
    }
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

/// The gamma function.
#[derive(Debug)]
pub struct Gamma;

#[cfg_attr(feature = "numerical", builtin)]
impl Gamma {
    pub fn eval_static(mut z: Complex) -> Complex {
        // implementation from https://en.wikipedia.org/wiki/Lanczos_approximation#Simple_implementation
        if z.imag().is_zero() {
            return complex(z.into_real_imag().0.gamma());
        }

        z -= 1;

        let mut x = complex(&GAMMA_P[0]);
        for (i, p) in GAMMA_P.iter().enumerate().skip(1) {
            x += p / complex(&z + i);
        }

        let t = complex(complex(&z + &*GAMMA_G) + 0.5);

        let tau_sqrt = float(&*TAU).sqrt();
        let t_pow = (&t).pow(z + 0.5);
        let exp_t = complex(t.as_neg().exp_ref());

        x * t_pow * exp_t * tau_sqrt
    }
}

/// Linearly interpolates between two values by a constant amount.
#[derive(Debug)]
pub struct Lerp;

#[cfg_attr(feature = "numerical", builtin)]
impl Lerp {
    pub fn eval_static(v1: Complex, v2: Complex, t: Float) -> Complex {
        &v1 + (v2 - &v1) * t
    }
}

/// Computes the linear parameter that produces the interpolant `v` given the values `v1` and `v2`.
#[derive(Debug)]
pub struct Invlerp;

#[cfg_attr(feature = "numerical", builtin)]
impl Invlerp {
    pub fn eval_static(v1: Float, v2: Float, v: Float) -> Float {
        (v - &v1) / (v2 - &v1)
    }
}

/// Returns the minimum of two values.
#[derive(Debug)]
pub struct Min;

#[cfg_attr(feature = "numerical", builtin)]
impl Min {
    pub fn eval_static(v1: Float, v2: Float) -> Float {
        v1.min(&v2)
    }
}

/// Returns the maximum of two values.
#[derive(Debug)]
pub struct Max;

#[cfg_attr(feature = "numerical", builtin)]
impl Max {
    pub fn eval_static(v1: Float, v2: Float) -> Float {
        v1.max(&v2)
    }
}

/// Clamps a value between two bounds.
#[derive(Debug)]
pub struct Clamp;

#[cfg_attr(feature = "numerical", builtin)]
impl Clamp {
    pub fn eval_static(v: Float, min: Float, max: Float) -> Float {
        v.clamp(&min, &max)
    }
}

/// Returns the greatest common factor of two integers.
#[derive(Debug)]
pub struct Gcf;

#[cfg_attr(feature = "numerical", builtin)]
impl Gcf {
    pub fn eval_static(a: Integer, b: Integer) -> Integer {
        a.gcd(&b)
    }
}

/// Returns the least common multiple of two integers.
#[derive(Debug)]
pub struct Lcm;

#[cfg_attr(feature = "numerical", builtin)]
impl Lcm {
    pub fn eval_static(a: Integer, b: Integer) -> Integer {
        a.lcm(&b)
    }
}

/// Returns the sign of a value. Returns zero if the value is zero.
#[derive(Debug)]
pub struct Sign;

#[cfg_attr(feature = "numerical", builtin)]
impl Sign {
    pub fn eval_static(v: Float) -> Float {
        if v.is_zero() {
            v
        } else {
            v.signum()
        }
    }
}

/// Returns the number of significant bits in the binary representation of an integer.
#[derive(Debug)]
pub struct Size;

#[cfg_attr(feature = "numerical", builtin)]
impl Size {
    pub fn eval_static(v: Integer) -> Integer {
        v.significant_bits().into()
    }
}
