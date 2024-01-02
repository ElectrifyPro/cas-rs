//! Additional constants used in the library. This module consists of static constants that return
//! [`Float`]s with the given value.

use once_cell::sync::Lazy;
use rug::{Complex, Float};
use super::primitive::{complex, float, float_from_str};

pub static ZERO: Lazy<Float> = Lazy::new(|| float(0));

pub static ONE: Lazy<Float> = Lazy::new(|| float(1));

pub static ONE_HALF: Lazy<Float> = Lazy::new(|| float(1) / float(2));

pub static TWO: Lazy<Float> = Lazy::new(|| float(2));

pub static TEN: Lazy<Float> = Lazy::new(|| float(10));

/// The imaginary unit.
pub static I: Lazy<Complex> = Lazy::new(|| complex((0, 1)));

/// Euler's number.
pub static E: Lazy<Float> = Lazy::new(|| {
    float_from_str(
        "2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563",
    )
});

/// The golden ratio.
pub static PHI: Lazy<Float> = Lazy::new(|| (float(1) + float(5).sqrt()) / float(2));

pub static PI: Lazy<Float> = Lazy::new(|| float(-1).acos());

pub static TAU: Lazy<Float> = Lazy::new(|| float(2) * &*PI);
