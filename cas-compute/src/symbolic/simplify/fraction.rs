//! Tools to help manipulate fractions.

use crate::{approx::approximate_rational, numerical::consts::int};
use crate::symbolic::expr::{Expr, Primary};
use rug::Integer;

/// Create an [`Expr`] representing a fraction with the given numerator and denominator.
///
/// The representation is an [`Expr::Mul`] containing two factors. The first factor is the
/// numerator, and the second factor is the denominator raised to the power of -1.
pub(crate) fn make_fraction(numerator: Expr, denominator: Expr) -> Expr {
    numerator *
        Expr::Exp(
            Box::new(denominator),
            Box::new(Expr::Primary(Primary::Integer(int(-1)))),
        )
}

/// Extracts a numerical fraction from the factors of an [`Expr::Mul`].
///
/// All [`Expr`]s in this library are represented in some canonical form. Fractions are represented
/// as an [`Expr::Mul`] containing a [`Primary::Integer`], and an [`Expr::Exp`], where the base is a
/// [`Primary::Integer`] and the exponent is `-1`.
///
/// This function finds two integer factors that match this pattern, removes them, and returns the
/// numerator and denominator as floats. This is a very specific definition of a fraction; this
/// function **will not** return the fraction 1/1 if it does not find any of those patterns, unless
/// the `numerator_optional` and / or `denominator_optional` arguments are set to `true`.
///
/// For example, when `numerator_optional` is `true`, the function will return an implied 1 as the
/// numerator if it does not find a [`Primary::Integer`] in the factors. When
/// `denominator_optional` is `true`, the function will return an implied 1 as the denominator if
/// it does not find a valid [`Expr::Exp`] in the factors.
///
/// To also support extracting fractions represented as a [`Primary::Float`], use the
/// [`extract_fractional`] function.
pub(crate) fn extract_integer_fraction(
    factors: &mut Vec<Expr>,
    numerator_optional: bool,
    denominator_optional: bool,
) -> Option<(Integer, Integer)> {
    let mut idx = 0;
    let mut numerator = None;
    let mut denominator = None;
    while idx < factors.len() {
        if numerator.is_none() && factors[idx].is_integer() {
            numerator = Some(factors.swap_remove(idx).into_integer().unwrap());
            continue;
        }

        if denominator.is_none() && factors[idx].is_integer_recip() {
            denominator = Some(factors.swap_remove(idx).into_integer_recip().unwrap());
            continue;
        }

        if numerator.is_some() && denominator.is_some() {
            break;
        }

        idx += 1;
    }

    match (numerator, denominator) {
        (Some(numerator), Some(denominator)) => Some((numerator, denominator)),
        (Some(numerator), None) if denominator_optional => Some((numerator, int(1))),
        (None, Some(denominator)) if numerator_optional => Some((int(1), denominator)),
        (None, None) if numerator_optional && denominator_optional => Some((int(1), int(1))),
        _ => None,
    }
}

/// Extracts an expression from the factors of an [`Expr::Mul`] that represents a fraction. This is
/// like [`extract_integer_fraction`], but the result of the function is an [`Expr`], and not the
/// extracted numerator and denominator.
///
/// Accordingly, this function also extracts [`Primary::Float`]s, simply returning them as-is.
pub(crate) fn extract_fractional(factors: &mut Vec<Expr>) -> Option<Expr> {
    let mut idx = 0;
    let mut numerator_idx = None;
    let mut denominator_idx = None;
    while idx < factors.len() {
        if factors[idx].is_float() {
            return Some(factors.swap_remove(idx));
        }

        if numerator_idx.is_none() && factors[idx].is_integer() {
            numerator_idx = Some(idx);
            continue;
        }

        if denominator_idx.is_none() && factors[idx].is_integer_recip() {
            denominator_idx = Some(idx);
            continue;
        }

        if numerator_idx.is_some() && denominator_idx.is_some() {
            break;
        }

        idx += 1;
    }

    match (numerator_idx, denominator_idx) {
        (Some(numerator_idx), Some(denominator_idx)) => {
            // remove larger index first, so that the smaller index is still valid
            if numerator_idx > denominator_idx {
                Some(factors.swap_remove(numerator_idx)
                    * factors.swap_remove(denominator_idx))
            } else {
                Some(factors.swap_remove(denominator_idx)
                    * factors.swap_remove(numerator_idx))
            }
        },
        (Some(numerator_idx), None) => Some(factors.swap_remove(numerator_idx)),
        (None, Some(denominator_idx)) => Some(factors.swap_remove(denominator_idx)),
        (None, None) => None,
    }
}

/// A more aggressive version of [`extract_explicit_frac`] that extracts numerical fractions from
/// any kind of expression, replacing the original expression with a [`Primary::Integer`]
/// containing the number 1.
///
/// [`Primary::Float`]s are also extracted with a rational approximation of the float.
///
/// Fractions are extracted as follows:
///
/// - [`Expr::Primary(Primary::Integer(int))`] -> `int / 1`
/// - [`Expr::Primary(Primary::Float(float))`] -> rational approximation of `float`
/// - [`Expr::Mul(factors)`] -> `numerator / denominator`
///   * `numerator` is the first [`Expr::Primary(Primary::Integer(num))`] found in `factors`
///   * `denominator` is the first [`Expr::Exp`] found in `factors`, where the base is a
///   [`Primary::Integer`] and the exponent is `-1`; if no such expression is found, `denominator`
///   is `1`
/// - [`Expr::Exp(lhs, rhs)`] -> `1 / lhs`
///   * `lhs` must be a [`Primary::Integer`]
pub(crate) fn extract_explicit_frac(expr: &mut Expr) -> Option<(Integer, Integer)> {
    match expr {
        Expr::Primary(Primary::Integer(num)) => {
            Some((std::mem::replace(num, int(1)), int(1)))
        },
        Expr::Primary(Primary::Float(num)) => {
            let rational = approximate_rational(num);
            *expr = Expr::Primary(Primary::Integer(int(1)));
            Some(rational.into_numer_denom())
        },
        Expr::Mul(factors) => extract_integer_fraction(factors, false, true),
        Expr::Exp(..) => {
            if expr.is_integer_recip() {
                let denominator = std::mem::replace(expr, Expr::Primary(Primary::Integer(int(1))))
                    .into_integer_recip()
                    .unwrap();
                Some((int(1), denominator))
            } else {
                None
            }
        },
        _ => None,
    }
}
