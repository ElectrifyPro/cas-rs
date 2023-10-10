//! Tools to help manipulate fractions.

use cas_eval::consts::float;
use crate::algebra::expr::{Expr, Primary};
use rug::Float;

/// Create an [`Expr`] representing a fraction with the given numerator and denominator.
///
/// The representation is an [`Expr::Mul`] containing two factors. The first factor is the
/// numerator, and the second factor is the denominator raised to the power of -1.
pub(crate) fn make_fraction(numerator: Expr, denominator: Expr) -> Expr {
    numerator *
        Expr::Exp(
            Box::new(denominator),
            Box::new(Expr::Primary(Primary::Number(float(-1)))),
        )
}

/// Extracts a numerical fraction from the factors of an [`Expr::Mul`].
///
/// Because all expressions are in a canonical form, a fraction is represented as an [`Expr::Mul`]
/// containing a [`Primary::Number`], and an [`Expr::Exp`], where the base is a [`Primary::Number`]
/// and the exponent is `-1`.
///
/// This function finds two factors that match this pattern, removes them, and returns the
/// numerator and denominator as floats. This is a very specific definition of a fraction; this
/// function **will not** return a single [`Primary::Number`] factor as a fraction, unless if the
/// `denominator_optional` parameter is `true`.
pub(crate) fn extract_numerical_fraction(
    factors: &mut Vec<Expr>,
    denominator_optional: bool,
) -> Option<(Float, Float)> {
    let mut idx = 0;
    let mut numerator = None;
    let mut denominator = None;
    while idx < factors.len() {
        if numerator.is_none() && factors[idx].is_number() {
            numerator = Some(factors.swap_remove(idx).into_number().unwrap());
            continue;
        }

        if denominator.is_none() && factors[idx].is_number_recip() {
            let denominator_expr = factors.swap_remove(idx);
            if let Expr::Exp(lhs, _) = denominator_expr {
                if let Expr::Primary(Primary::Number(num)) = *lhs {
                    denominator = Some(num);
                    continue;
                }
            }
        }

        if numerator.is_some() && denominator.is_some() {
            break;
        }

        idx += 1;
    }

    if let Some(numerator) = numerator {
        match denominator {
            Some(denominator) => Some((numerator, denominator)),
            None if denominator_optional => Some((numerator, float(1))),
            _ => None,
        }
    } else {
        None
    }
}

/// A more aggressive version of [`extract_numerical_fraction`] that extracts numerical fractions
/// from any kind of expression, replacing the expression with a [`Primary::Number`] containing the
/// number 1.
///
/// Fractions are extracted as follows:
///
/// - [`Expr::Primary(Primary::Number(num))`] -> `num / 1`
/// - [`Expr::Mul(factors)`] -> `numerator / denominator`
///   * `numerator` is the first [`Expr::Primary(Primary::Number(num))`] found in `factors`
///   * `denominator` is the first [`Expr::Exp`] found in `factors`, where the base is a
///   [`Primary::Number`] and the exponent is `-1`; if no such expression is found, `denominator`
///   is `1`
/// - [`Expr::Exp(lhs, rhs)`] -> `1 / lhs`
///   * `lhs` must be a [`Primary::Number`]
pub(crate) fn extract_explicit_frac(expr: &mut Expr) -> Option<(Float, Float)> {
    match expr {
        Expr::Primary(Primary::Number(num)) => {
            let clone = num.clone();
            *num = float(1);
            Some((clone, float(1)))
        },
        Expr::Mul(factors) => extract_numerical_fraction(factors, true),
        Expr::Exp(..) => {
            if let Some(num) = expr.as_number_recip().cloned() {
                *expr = Expr::Primary(Primary::Number(float(1)));
                Some((float(1), num))
            } else {
                None
            }
        },
        _ => None,
    }
}
