//! Tools to help manipulate fractions.

use crate::approx::approximate_rational;
use crate::primitive::int;
use crate::symbolic::expr::{SymExpr, Primary};
use rug::Integer;

/// Create a [`SymExpr`] representing a fraction with the given numerator and denominator.
///
/// The representation is a [`SymExpr::Mul`] containing two factors. The first factor is the
/// numerator, and the second factor is the denominator raised to the power of -1.
pub(crate) fn make_fraction(numerator: SymExpr, denominator: SymExpr) -> SymExpr {
    numerator *
        SymExpr::Exp(
            Box::new(denominator),
            Box::new(SymExpr::Primary(Primary::Integer(int(-1)))),
        )
}

/// Extracts a numerical fraction from the factors of a [`SymExpr::Mul`].
///
/// All [`SymExpr`]s in this library are represented in some canonical form. Fractions are
/// represented as a [`SymExpr::Mul`] containing a [`Primary::Integer`], and a [`SymExpr::Exp`],
/// where the base is a [`Primary::Integer`] and the exponent is `-1`.
///
/// This function finds two integer factors that match this pattern, removes them, and returns the
/// numerator and denominator as floats. This is a very specific definition of a fraction; this
/// function **will not** return the fraction 1/1 if it does not find any of those patterns, unless
/// the `numerator_optional` and / or `denominator_optional` arguments are set to `true`.
///
/// For example, when `numerator_optional` is `true`, the function will return an implied 1 as the
/// numerator if it does not find a [`Primary::Integer`] in the factors. When
/// `denominator_optional` is `true`, the function will return an implied 1 as the denominator if
/// it does not find a valid [`SymExpr::Exp`] in the factors.
///
/// To also support extracting fractions represented as a [`Primary::Float`], use the
/// [`extract_fractional`] function.
pub(crate) fn extract_integer_fraction(
    factors: &mut Vec<SymExpr>,
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

/// Extracts an expression from the factors of a [`SymExpr::Mul`] that represents a fraction. This
/// is like [`extract_integer_fraction`], but the result of the function is a [`SymExpr`], and not
/// the extracted numerator and denominator.
///
/// Accordingly, this function also extracts [`Primary::Float`]s, simply returning them as-is.
pub(crate) fn extract_fractional(factors: &mut Vec<SymExpr>) -> Option<SymExpr> {
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
/// - [`SymExpr::Primary(Primary::Integer(int))`] -> `int / 1`
/// - [`SymExpr::Primary(Primary::Float(float))`] -> rational approximation of `float`
/// - [`SymExpr::Mul(factors)`] -> `numerator / denominator`
///   * `numerator` is the first [`SymExpr::Primary(Primary::Integer(num))`] found in `factors`
///   * `denominator` is the first [`SymExpr::Exp`] found in `factors`, where the base is a
///   [`Primary::Integer`] and the exponent is `-1`; if no such expression is found, `denominator`
///   is `1`
/// - [`SymExpr::Exp(lhs, rhs)`] -> `1 / lhs`
///   * `lhs` must be a [`Primary::Integer`]
pub(crate) fn extract_explicit_frac(expr: &mut SymExpr) -> Option<(Integer, Integer)> {
    match expr {
        SymExpr::Primary(Primary::Integer(num)) => {
            Some((std::mem::replace(num, int(1)), int(1)))
        },
        SymExpr::Primary(Primary::Float(num)) => {
            let rational = approximate_rational(num);
            *expr = SymExpr::Primary(Primary::Integer(int(1)));
            Some(rational.into_numer_denom())
        },
        SymExpr::Mul(factors) => extract_integer_fraction(factors, false, true),
        SymExpr::Exp(..) => {
            if expr.is_integer_recip() {
                let denominator = std::mem::replace(expr, SymExpr::Primary(Primary::Integer(int(1))))
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
