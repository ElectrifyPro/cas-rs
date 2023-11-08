use crate::algebra::expr::Expr;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use super::consts::{input::*, output::*, ONE, ONE_HALF, ZERO};

/// The output of the trigonometric functions for certain angles, and whether to negate the output.
#[derive(PartialEq, Eq, Hash)]
pub struct TrigOut {
    /// The output of the trigonometric function.
    pub output: &'static Expr,

    /// Whether to negate the output.
    pub neg: bool,
}

impl From<&'static Expr> for TrigOut {
    fn from(output: &'static Expr) -> Self {
        Self {
            output,
            neg: false,
        }
    }
}

pub static SIN_TABLE: Lazy<HashMap<&Expr, TrigOut>> = Lazy::new(|| HashMap::from([
    // sin(0) = 0
    (&*ZERO, TrigOut::from(&*ZERO)),

    // sin(1/12 = pi/6 rad = 30 deg) = 1/2
    (&*ONE_TWELFTH, TrigOut::from(&*ONE_HALF)),

    // sin(1/8 = pi/4 rad = 45 deg) = 1/sqrt(2) = sqrt(2)/2
    (&*ONE_EIGHTH, TrigOut::from(&*SQRT_TWO_HALF)),

    // sin(1/6 = pi/3 rad = 60 deg) = sqrt(3)/2
    (&*ONE_SIXTH, TrigOut::from(&*SQRT_THREE_HALF)),

    // sin(1/4 = pi/2 rad = 90 deg) = 1
    (&*ONE_FOURTH, TrigOut::from(&*ONE)),

    // sin(1/3 = 2pi/3 rad = 120 deg) = sqrt(3)/2
    (&*ONE_THIRD, TrigOut::from(&*SQRT_THREE_HALF)),

    // sin(3/8 = 3pi/4 rad = 135 deg) = 1/sqrt(2) = sqrt(2)/2
    (&*THREE_EIGHTS, TrigOut::from(&*SQRT_TWO_HALF)),

    // sin(5/12 = 5pi/6 rad = 150 deg) = 1/2
    (&*FIVE_TWELFTHS, TrigOut::from(&*ONE_HALF)),

    // sin(1/2 = pi rad = 180 deg) = 0
    (&*ONE_HALF, TrigOut::from(&*ZERO)),

    // sin(7/12 = ...) = -1/2
    (&*SEVEN_TWELFTHS, TrigOut { output: &ONE_HALF, neg: true }),

    // sin(5/8 = ...) = -1/sqrt(2) = -sqrt(2)/2
    (&*FIVE_EIGHTHS, TrigOut { output: &SQRT_TWO_HALF, neg: true }),

    // sin(2/3 = ...) = -sqrt(3)/2
    (&*TWO_THIRDS, TrigOut { output: &SQRT_THREE_HALF, neg: true }),

    // sin(3/4 = ...) = -1
    (&*THREE_FOURTHS, TrigOut { output: &ONE, neg: true }),

    // sin(5/6 = ...) = -sqrt(3)/2
    (&*FIVE_SIXTHS, TrigOut { output: &SQRT_THREE_HALF, neg: true }),

    // sin(7/8 = ...) = -1/sqrt(2) = -sqrt(2)/2
    (&*SEVEN_EIGHTHS, TrigOut { output: &SQRT_TWO_HALF, neg: true }),

    // sin(11/12 = ...) = -1/2
    (&*ELEVEN_TWELFTHS, TrigOut { output: &ONE_HALF, neg: true }),

    // sin(1 = ...) = 0
    (&*ONE, TrigOut::from(&*ZERO)),
]));

pub static COS_TABLE: Lazy<HashMap<&Expr, TrigOut>> = Lazy::new(|| HashMap::from([
    // cos(0) = 1
    (&*ZERO, TrigOut::from(&*ONE)),

    // cos(1/12 = pi/6 rad = 30 deg) = sqrt(3)/2
    (&*ONE_TWELFTH, TrigOut::from(&*SQRT_THREE_HALF)),

    // cos(1/8 = pi/4 rad = 45 deg) = 1/sqrt(2) = sqrt(2)/2
    (&*ONE_EIGHTH, TrigOut::from(&*SQRT_TWO_HALF)),

    // cos(1/6 = pi/3 rad = 60 deg) = 1/2
    (&*ONE_SIXTH, TrigOut::from(&*ONE_HALF)),

    // cos(1/4 = pi/2 rad = 90 deg) = 0
    (&*ONE_FOURTH, TrigOut::from(&*ZERO)),

    // cos(1/3 = 2pi/3 rad = 120 deg) = -1/2
    (&*ONE_THIRD, TrigOut { output: &ONE_HALF, neg: true }),

    // cos(3/8 = 3pi/4 rad = 135 deg) = -1/sqrt(2) = -sqrt(2)/2
    (&*THREE_EIGHTS, TrigOut { output: &SQRT_TWO_HALF, neg: true }),

    // cos(5/12 = 5pi/6 rad = 150 deg) = -sqrt(3)/2
    (&*FIVE_TWELFTHS, TrigOut { output: &SQRT_THREE_HALF, neg: true }),

    // cos(1/2 = pi rad = 180 deg) = -1
    (&*ONE_HALF, TrigOut { output: &ONE, neg: true }),

    // cos(7/12 = ...) = -sqrt(3)/2
    (&*SEVEN_TWELFTHS, TrigOut { output: &SQRT_THREE_HALF, neg: true }),

    // cos(5/8 = ...) = -1/sqrt(2) = -sqrt(2)/2
    (&*FIVE_EIGHTHS, TrigOut { output: &SQRT_TWO_HALF, neg: true }),

    // cos(2/3 = ...) = -1/2
    (&*TWO_THIRDS, TrigOut { output: &ONE_HALF, neg: true }),

    // cos(3/4 = ...) = 0
    (&*THREE_FOURTHS, TrigOut::from(&*ZERO)),

    // cos(5/6 = ...) = 1/2
    (&*FIVE_SIXTHS, TrigOut::from(&*ONE_HALF)),

    // cos(7/8 = ...) = 1/sqrt(2) = sqrt(2)/2
    (&*SEVEN_EIGHTHS, TrigOut::from(&*SQRT_TWO_HALF)),

    // cos(11/12 = ...) = sqrt(3)/2
    (&*ELEVEN_TWELFTHS, TrigOut::from(&*SQRT_THREE_HALF)),

    // cos(1 = ...) = 1
    (&*ONE, TrigOut::from(&*ONE)),
]));

pub static TAN_TABLE: Lazy<HashMap<&Expr, TrigOut>> = Lazy::new(|| HashMap::from([
    // tan(0) = 0
    (&*ZERO, TrigOut::from(&*ZERO)),

    // tan(1/12 = pi/6 rad = 30 deg) = sqrt(3)/3
    (&*ONE_TWELFTH, TrigOut::from(&*SQRT_THREE_THIRD)),

    // tan(1/8 = pi/4 rad = 45 deg) = 1
    (&*ONE_EIGHTH, TrigOut::from(&*ONE)),

    // tan(1/6 = pi/3 rad = 60 deg) = sqrt(3)
    (&*ONE_SIXTH, TrigOut::from(&*SQRT_THREE)),

    // tan(1/4 = pi/2 rad = 90 deg) = undefined
    // (&*ONE_FOURTH, TrigOut::from(&*UNDEFINED)),

    // tan(1/3 = 2pi/3 rad = 120 deg) = -sqrt(3)
    (&*ONE_THIRD, TrigOut { output: &SQRT_THREE, neg: true }),

    // tan(3/8 = 3pi/4 rad = 135 deg) = -1
    (&*THREE_EIGHTS, TrigOut { output: &ONE, neg: true }),

    // tan(5/12 = 5pi/6 rad = 150 deg) = -sqrt(3)/3
    (&*FIVE_TWELFTHS, TrigOut { output: &SQRT_THREE_THIRD, neg: true }),

    // tan(1/2 = pi rad = 180 deg) = 0
    (&*ONE_HALF, TrigOut::from(&*ZERO)),

    // tan(7/12 = ...) = sqrt(3)/3
    (&*SEVEN_TWELFTHS, TrigOut::from(&*SQRT_THREE_THIRD)),

    // tan(5/8 = ...) = 1
    (&*FIVE_EIGHTHS, TrigOut::from(&*ONE)),

    // tan(2/3 = ...) = sqrt(3)
    (&*TWO_THIRDS, TrigOut::from(&*SQRT_THREE)),

    // tan(3/4 = ...) = undefined
    // (&*THREE_FOURTHS, TrigOut::from(&*UNDEFINED)),

    // tan(5/6 = ...) = -sqrt(3)
    (&*FIVE_SIXTHS, TrigOut { output: &SQRT_THREE, neg: true }),

    // tan(7/8 = ...) = -1
    (&*SEVEN_EIGHTHS, TrigOut { output: &ONE, neg: true }),

    // tan(11/12 = ...) = -sqrt(3)/3
    (&*ELEVEN_TWELFTHS, TrigOut { output: &SQRT_THREE_THIRD, neg: true }),

    // tan(1 = ...) = 0
    (&*ONE, TrigOut::from(&*ZERO)),
]));
