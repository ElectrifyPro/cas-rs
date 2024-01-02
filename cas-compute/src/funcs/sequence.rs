//! Functions to determine the specified term of particular sequences.

use cas_attrs::builtin;
use crate::consts::PHI;
use crate::primitive::float;
use rug::{ops::Pow, Float};

/// Returns the `n`th term of the Fibonacci sequence.
///
/// The implementation considers `fib(1) = fib(2) = 1`.
#[derive(Debug)]
pub struct Fib;

#[cfg_attr(feature = "numerical", builtin)]
impl Fib {
    pub fn eval_static(n: Float) -> Float {
        let result_negative = if n.is_sign_negative() {
            // TODO
            n.to_integer().unwrap().is_even()
        } else {
            false
        };

        let a = float(&*PHI).pow(&*n.as_abs());
        let b = float(1.0 - &*PHI).pow(&*n.as_abs());
        let five_sqrt = float(5.0).sqrt();
        let raw = ((a - b) / five_sqrt).round();

        if result_negative { -raw } else { raw }
    }
}
