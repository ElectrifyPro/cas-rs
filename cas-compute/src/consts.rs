//! Additional constants used in the library. This module consists of static constants that return
//! [`Float`]s with the given value.

use once_cell::sync::Lazy;
use rug::{Complex, Float};
use std::{collections::HashSet, sync::OnceLock};
use super::primitive::{complex, float};

pub static ZERO: Lazy<Float> = Lazy::new(|| float(0));

pub static ONE: Lazy<Float> = Lazy::new(|| float(1));

pub static ONE_HALF: Lazy<Float> = Lazy::new(|| float(1) / float(2));

pub static TWO: Lazy<Float> = Lazy::new(|| float(2));

pub static TEN: Lazy<Float> = Lazy::new(|| float(10));

/// The imaginary unit.
pub static I: Lazy<Complex> = Lazy::new(|| complex((0, 1)));

/// Euler's number.
pub static E: Lazy<Float> = Lazy::new(|| float(1).exp());

/// The golden ratio.
pub static PHI: Lazy<Float> = Lazy::new(|| (float(1) + float(5).sqrt()) / float(2));

pub static PI: Lazy<Float> = Lazy::new(|| float(-1).acos());

pub static TAU: Lazy<Float> = Lazy::new(|| float(2) * &*PI);

/// Returns a set of all constants provided in a `cas-rs` script.
#[cfg(feature = "numerical")]
pub fn all() -> &'static HashSet<&'static str> {
    // NOTE: this is a set and not a map; as a map, we would have to store a `Value` in order to
    // support both `Float` and `Complex` constants
    // however, at the moment, `Value` is not `Sync` (because of `Value::List`), so we cannot store
    // it in a `OnceLock`. this will probably change in the future
    static CONSTS: OnceLock<HashSet<&'static str>> = OnceLock::new();
    CONSTS.get_or_init(|| {
        HashSet::from(["i", "e", "phi", "pi", "tau"])
    })
}
