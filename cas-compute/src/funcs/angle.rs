//! Utilities for converting between degrees and radians.

use cas_attrs::builtin;
use crate::consts::{PI, TAU};
use rug::Float;

/// Converts the given value from degrees to radians.
#[derive(Debug)]
pub struct Dtr;

#[cfg_attr(feature = "numerical", builtin)]
impl Dtr {
    pub fn eval_static(n: Float) -> Float {
        n * &*PI / 180.0
    }
}

/// Converts the given value from radians to degrees.
#[derive(Debug)]
pub struct Rtd;

#[cfg_attr(feature = "numerical", builtin)]
impl Rtd {
    pub fn eval_static(n: Float) -> Float {
        n * 180.0 / &*PI
    }
}

/// Computes the amount of angle needed to traverse a specified fraction of a circle.
///
/// For example, `circle(0.25)` returns `PI / 2`, since a rotation of `PI / 2` radians is needed to
/// traverse a quarter of a circle.
#[derive(Debug)]
pub struct Circle;

#[cfg_attr(feature = "numerical", builtin)]
impl Circle {
    pub fn eval_static(n: Float) -> Float {
        n * &*TAU
    }
}
