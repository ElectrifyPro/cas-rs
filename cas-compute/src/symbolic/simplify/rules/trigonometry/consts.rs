//! Relevant constants used as input and output to a trigonometric function.

use crate::numerical::consts::int;
use crate::symbolic::{
    expr::{Expr, Primary},
    simplify::fraction::make_fraction,
};
use once_cell::sync::Lazy;

/// The number one, wrapped in an [`Expr`].
pub static ONE: Lazy<Expr> = Lazy::new(|| Expr::Primary(Primary::Integer(int(1))));

/// The number 1/2, wrapped in an [`Expr`].
pub static ONE_HALF: Lazy<Expr> = Lazy::new(|| make_fraction(
    Expr::Primary(Primary::Integer(int(1))),
    Expr::Primary(Primary::Integer(int(2))),
));

/// The number zero, wrapped in an [`Expr`].
pub static ZERO: Lazy<Expr> = Lazy::new(|| Expr::Primary(Primary::Integer(int(0))));

/// Relevant fractions of a unit circle.
///
/// The trigonometric functions for certain angles are well-known, and can be represented exactly
/// in terms of square roots. These exact values are called "constructible" values. We can take
/// advantage of this by creating a lookup table of these input angles to the outputs of the
/// respective trigonometric functions.
///
/// Here, we simplify things a step further by pre-dividing the set of useful input angles by
/// `2pi` (or 360 degrees). The result is a set of numerical fractions that represent a normalized
/// angle from 0 to 1. Normalization allows us to use the same lookup table for all input angles,
/// irregardless of the current trigonometric mode set by the user (e.g. `radians` or `degrees`).
///
/// The angles `0 rad` (`0 deg`), `pi rad` (`180 deg`) and `2pi rad` (`360 deg`) are **not**
/// included in this module. The constants [`ONE`], [`ONE_HALF`], and [`ZERO`] can be used instead.
pub mod input {
    use super::*;

    /// 1/12 = pi/6 rad = 30 deg
    pub static ONE_TWELFTH: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(1))),
        Expr::Primary(Primary::Integer(int(12))),
    ));

    /// 1/8 = pi/4 rad = 45 deg
    pub static ONE_EIGHTH: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(1))),
        Expr::Primary(Primary::Integer(int(8))),
    ));

    /// 1/6 = pi/3 rad = 60 deg
    pub static ONE_SIXTH: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(1))),
        Expr::Primary(Primary::Integer(int(6))),
    ));

    /// 1/4 = pi/2 rad = 90 deg
    pub static ONE_FOURTH: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(1))),
        Expr::Primary(Primary::Integer(int(4))),
    ));

    /// 1/3 = 2pi/3 rad = 120 deg
    pub static ONE_THIRD: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(1))),
        Expr::Primary(Primary::Integer(int(3))),
    ));

    /// 3/8 = 3pi/4 rad = 135 deg
    pub static THREE_EIGHTS: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(3))),
        Expr::Primary(Primary::Integer(int(8))),
    ));

    /// 5/12 = 5pi/6 rad = 150 deg
    pub static FIVE_TWELFTHS: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(5))),
        Expr::Primary(Primary::Integer(int(12))),
    ));

    // lower half
    /// 7/12 = 7pi/6 rad = 210 deg
    pub static SEVEN_TWELFTHS: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(7))),
        Expr::Primary(Primary::Integer(int(12))),
    ));

    /// 5/8 = 5pi/4 rad = 225 deg
    pub static FIVE_EIGHTHS: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(5))),
        Expr::Primary(Primary::Integer(int(8))),
    ));

    /// 2/3 = 4pi/3 rad = 240 deg
    pub static TWO_THIRDS: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(2))),
        Expr::Primary(Primary::Integer(int(3))),
    ));

    /// 3/4 = 3pi/2 rad = 270 deg
    pub static THREE_FOURTHS: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(3))),
        Expr::Primary(Primary::Integer(int(4))),
    ));

    /// 5/6 = 5pi/3 rad = 300 deg
    pub static FIVE_SIXTHS: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(5))),
        Expr::Primary(Primary::Integer(int(6))),
    ));

    /// 7/8 = 7pi/4 rad = 315 deg
    pub static SEVEN_EIGHTHS: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(7))),
        Expr::Primary(Primary::Integer(int(8))),
    ));

    /// 11/12 = 11pi/6 rad = 330 deg
    pub static ELEVEN_TWELFTHS: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(11))),
        Expr::Primary(Primary::Integer(int(12))),
    ));
}

/// Common outputs of trigonometric functions.
///
/// The outputs `0`, `1/2`, and `1` are not included in this module. The constants [`ONE`],
/// [`ONE_HALF`], and [`ZERO`] can be used instead.
pub mod output {
    use super::*;

    /// sqrt(2)/2
    pub static SQRT_TWO_HALF: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(2))).sqrt(),
        Expr::Primary(Primary::Integer(int(2))),
    ));

    /// sqrt(3)/2
    pub static SQRT_THREE_HALF: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(3))).sqrt(),
        Expr::Primary(Primary::Integer(int(2))),
    ));

    /// sqrt(3)/3
    pub static SQRT_THREE_THIRD: Lazy<Expr> = Lazy::new(|| make_fraction(
        Expr::Primary(Primary::Integer(int(3))).sqrt(),
        Expr::Primary(Primary::Integer(int(3))),
    ));

    /// sqrt(3)
    pub static SQRT_THREE: Lazy<Expr> = Lazy::new(|| Expr::Primary(Primary::Integer(int(3))).sqrt());
}
