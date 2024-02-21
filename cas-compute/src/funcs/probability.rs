//! Probability density and distribution functions.

use cas_attrs::builtin;
use crate::consts::{E, ONE, PI, TAU, TWO, ZERO};
use crate::primitive::{float, float_from_str, int};
use once_cell::sync::Lazy;
use rug::{float::Special, ops::Pow, Float, Integer};
use super::miscellaneous::partial_factorial;

/// The error function, `erf(x)`.
#[derive(Debug)]
pub struct Erf;

#[cfg_attr(feature = "numerical", builtin)]
impl Erf {
    pub fn eval_static(x: Float) -> Float {
        x.erf()
    }
}

/// The complementary error function, `erfc(x)`.
#[derive(Debug)]
pub struct Erfc;

#[cfg_attr(feature = "numerical", builtin)]
impl Erfc {
    pub fn eval_static(x: Float) -> Float {
        x.erfc()
    }
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

/// The inverse error function, `inverf(x)`.
#[derive(Debug)]
pub struct Inverf;

#[cfg_attr(feature = "numerical", builtin)]
impl Inverf {
    pub fn eval_static(x: Float) -> Float {
        if x <= -1 {
            return float(Special::NegInfinity);
        } else if x >= 1 {
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
}

/// Normal probability density function.
///
/// This function does not actually represent any probability. To get the probability of a normally
/// distributed random variable being between a value `a` and `b`, use [`Normcdf`].
#[derive(Debug)]
pub struct Normpdf;

#[cfg_attr(feature = "numerical", builtin)]
impl Normpdf {
    pub fn eval_static(x: Float, m: Option<Float>, s: Option<Float>) -> Float {
        let m = m.unwrap_or_else(|| float(&*ZERO));
        let s = s.unwrap_or_else(|| float(&*ONE));
        let exp_arg = Float::exp(float(x - m).square() / (-2 * float(s.square_ref())));
        let bot = s * float(TAU.sqrt_ref());
        exp_arg / bot
    }
}

/// Normal cumulative distribution function.
///
/// Returns the probability that a random variable, chosen from a normal distribution with mean `m`
/// and standard deviation `s`, will be between `a` and `b`, inclusive.
#[derive(Debug)]
pub struct Normcdf;

#[cfg_attr(feature = "numerical", builtin)]
impl Normcdf {
    pub fn eval_static(a: Float, b: Float, m: Option<Float>, s: Option<Float>) -> Float {
        let m = m.unwrap_or_else(|| float(&*ZERO));
        let s = s.unwrap_or_else(|| float(&*ONE));
        let sqrt_two = float(TWO.sqrt_ref());
        let z_a = (a - &m) / float(&s * &sqrt_two);
        let z_b = (b - &m) / float(&s * &sqrt_two);
        (z_b.erf() - z_a.erf()) / &*TWO
    }
}

/// Inverse normal cumulative distribution function.
///
/// Returns the value `x` such that `normcdf(-∞, x, m, s) = p`, i.e., the value `x` such that the
/// probability of a random variable, chosen from a normal distribution with mean `m` and standard
/// deviation `s`, being between `-∞` and `x` is `p`.
#[derive(Debug)]
pub struct Invnorm;

#[cfg_attr(feature = "numerical", builtin)]
impl Invnorm {
    pub fn eval_static(p: Float, m: Option<Float>, s: Option<Float>) -> Float {
        let m = m.unwrap_or_else(|| float(&*ZERO));
        let s = s.unwrap_or_else(|| float(&*ONE));
        let sqrt_two = float(TWO.sqrt_ref());
        let z = sqrt_two * Inverf::eval_static(2 * p - 1);
        m + s * z
    }
}

/// Geometric probability function.
///
/// Returns the probability of the first success of an event occurring on the `n`th trial, where the
/// probability of success on a single trial is `p`.
#[derive(Debug)]
pub struct Geompdf;

#[cfg_attr(feature = "numerical", builtin)]
impl Geompdf {
    pub fn eval_static(p: Float, n: Integer) -> Float {
        if n <= *ZERO {
            return float(&*ZERO);
        }

        let q = float(&*ONE - &p);
        p * q.pow(n - 1)
    }
}

/// Cummulative geometric probability function.
///
/// Returns the probability of the first success of an event occurring on or before the `n`th
/// trial, where the probability of success on a single trial is `p`.
#[derive(Debug)]
pub struct Geomcdf;

#[cfg_attr(feature = "numerical", builtin)]
impl Geomcdf {
    pub fn eval_static(p: Float, n: Integer) -> Float {
        if n <= *ZERO {
            return float(&*ZERO);
        }

        let q = float(&*ONE - &p);
        &*ONE - q.pow(n)
    }
}

/// Binomial probability function.
///
/// Returns the probability of exactly `x` successes occurring in `n` trials, where the probability
/// of success on a single trial is `p`.
#[derive(Debug)]
pub struct Binompdf;

#[cfg_attr(feature = "numerical", builtin)]
impl Binompdf {
    pub fn eval_static(n: Integer, p: Float, x: Integer) -> Float {
        if x < *ZERO || x > n {
            return float(&*ZERO);
        }

        let q = float(&*ONE - &p);

        let c = q.pow(n.clone() - &x);
        let b = p.pow(&x);
        let a = super::combinatoric::Ncr::eval_static(n, x);
        a * b * c
    }
}

/// Cummulative binomial probability function.
///
/// Returns the probability of `x` or fewer successes occurring in `n` trials, where the
/// probability of success on a single trial is `p`.
#[derive(Debug)]
pub struct Binomcdf;

#[cfg_attr(feature = "numerical", builtin)]
impl Binomcdf {
    pub fn eval_static(n: Integer, p: Float, mut x: Integer) -> Float {
        if x < *ZERO {
            return float(&*ZERO);
        } else if x >= n {
            return float(&*ONE);
        }

        let mut sum = float(&*ZERO);
        while x >= *ZERO {
            sum += Binompdf::eval_static(n.clone(), p.clone(), x.clone());
            x -= 1;
        }
        sum
    }
}

/// Poisson probability function.
///
/// Returns the probability of exactly `k` events occurring in an arbitrary time interval, where
/// the mean number of occurrences of the event in the time interval is `l`.
#[derive(Debug)]
pub struct Poisspdf;

#[cfg_attr(feature = "numerical", builtin)]
impl Poisspdf {
    pub fn eval_static(k: Integer, l: Float) -> Float {
        if k < *ZERO {
            return float(&*ZERO);
        }

        let b = float(&*E).pow(&*l.as_neg());
        let a = l.pow(&k);
        let c = partial_factorial(k, int(1));
        a * b / c
    }
}

/// Cummulative poisson probability function.
///
/// Returns the probability of `k` or fewer events occurring in an arbitrary time interval, where
/// the mean number of occurrences of the event in the time interval is `l`.
#[derive(Debug)]
pub struct Poisscdf;

#[cfg_attr(feature = "numerical", builtin)]
impl Poisscdf {
    pub fn eval_static(k: Integer, l: Float) -> Float {
        if k < *ZERO {
            return float(&*ZERO);
        }

        fn reg_gamma(s: Float, x: Float) -> Float {
            float(s.gamma_inc_ref(&x)) / s.gamma()
        }

        reg_gamma(float(k + 1), l)
    }
}
