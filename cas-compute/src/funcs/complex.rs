//! Useful functions for complex numbers.

use cas_attrs::builtin;
use rug::{Complex, Float};

/// Returns the real part of the given complex number.
#[derive(Debug)]
pub struct Re;

#[cfg_attr(feature = "numerical", builtin)]
impl Re {
    pub fn eval_static(n: Complex) -> Float {
        n.into_real_imag().0
    }
}

/// Returns the imaginary part of the given complex number.
#[derive(Debug)]
pub struct Im;

#[cfg_attr(feature = "numerical", builtin)]
impl Im {
    pub fn eval_static(n: Complex) -> Float {
        n.into_real_imag().1
    }
}

/// Returns the argument of the given complex number, in radians.
#[derive(Debug)]
pub struct Arg;

#[cfg_attr(feature = "numerical", builtin(radian = output))]
impl Arg {
    pub fn eval_static(n: Complex) -> Float {
        n.arg().into_real_imag().0 // `n`.arg() returns Complex interestingly
    }
}

/// Returns the complex conjugate of the given complex number.
#[derive(Debug)]
pub struct Conj;

#[cfg_attr(feature = "numerical", builtin)]
impl Conj {
    pub fn eval_static(n: Complex) -> Complex {
        n.conj()
    }
}
