//! Simplification rules for expressions involving multiplication, including combining like
//! factors.

use cas_eval::consts::{ZERO, ONE, float, int_from_float};
use crate::{
    algebra::{expr::{Expr, Primary}, simplify::{rules::do_multiply, step::Step}},
    step::StepCollector,
};
use rug::Float;

/// Extracts a numerical fraction from the factors of a [`Expr::Mul`].
///
/// Because all expressions are in a canonical form, a fraction is represented as a [`Expr::Mul`]
/// containing a [`Primary::Number`], and a [`Expr::Exp`], where the base is a [`Primary::Number`]
/// and the exponent is `-1`.
///
/// This function finds two factors that match this pattern, removes them, and returns the
/// numerator and denominator as floats.
fn extract_numerical_fraction(factors: &mut Vec<Expr>) -> Option<(Float, Float)> {
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

    if let (Some(numerator), Some(denominator)) = (numerator, denominator) {
        Some((numerator, denominator))
    } else {
        None
    }
}

/// `0*a = 0`
/// `a*0 = 0`
pub fn multiply_zero(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_multiply(expr, |factors| {
        if factors.iter().any(|factor| factor.as_number() == Some(&ZERO)) {
            Some(Expr::Primary(Primary::Number(ZERO.clone())))
        } else {
            None
        }
    })?;

    // keep the step collection logic outside of the closure to make it implement `Fn`
    step_collector.push(Step::MultiplyZero);
    Some(opt)
}

/// `1*a = a`
/// `a*1 = a`
pub fn multiply_one(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_multiply(expr, |factors| {
        let mut new_factors = factors.iter()
            .filter(|factor| {
                // keep all non-one factors
                factor.as_number()
                    .map(|n| n != &1)
                    .unwrap_or(true)
            })
            .cloned()
            .collect::<Vec<_>>();

        if new_factors.len() == factors.len() {
            None
        } else if new_factors.is_empty() {
            Some(Expr::Primary(Primary::Number(ONE.clone())))
        } else if new_factors.len() == 1 {
            Some(new_factors.remove(0))
        } else {
            Some(Expr::Mul(new_factors))
        }
    })?;

    step_collector.push(Step::MultiplyOne);
    Some(opt)
}

/// Simplifies numerical fractions.
///
/// `3/12 = 1/4`
/// `12/3 = 4`
pub fn reduce_numerical_fraction(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_multiply(expr, |factors| {
        let mut new_factors = factors.to_vec();

        // extract a fraction, a Number and a Number^-1
        let (numerator, denominator) = extract_numerical_fraction(&mut new_factors)?;

        // reduce the fraction
        let gcd = int_from_float(numerator.clone())
            .gcd(&int_from_float(denominator.clone()));
        if gcd == 1 {
            return None;
        }

        let numerator = Expr::Primary(Primary::Number(numerator / &gcd));
        let denominator = Expr::Exp(
            Box::new(Expr::Primary(Primary::Number(denominator / &gcd))),
            Box::new(Expr::Primary(Primary::Number(float(-1)))),
        );

        // insert the reduced fraction back into the factors
        new_factors.extend([numerator, denominator]);
        Some(Expr::Mul(new_factors))
    })?;

    step_collector.push(Step::ReduceFraction);
    Some(opt)
}

/// Combines like factors.
///
/// `a^b*a^c = a^(b+c)`
/// `a^c*b^c = (a*b)^c`
/// etc.
pub fn combine_like_factors(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_multiply(expr, |factors| {
        let mut new_factors = factors.to_vec();
        let mut current_factor_idx = 0;

        /// Utility function to extract the base and exponent of an expression. If the expression
        /// is not [`Expr::Exp`], the exponent is `1`.
        ///
        /// - `a^b` -> `(a, b)`
        /// - `a` -> `(a, 1)`
        fn get_exp(expr: &Expr) -> (Expr, Expr) {
            match expr {
                Expr::Exp(lhs, rhs) => (*lhs.clone(), *rhs.clone()),
                expr => (expr.clone(), Expr::Primary(Primary::Number(ONE.clone()))),
            }
        }

        // this is O(n^2) worst case, due to scanning the whole vec for each factor
        // TODO: optimize with hashing?
        while current_factor_idx < new_factors.len() {
            let (mut current_factor, mut current_factor_exp) = get_exp(&new_factors[current_factor_idx]);

            // look at every factor after `current_factor`
            let mut next_factor_idx = current_factor_idx + 1;
            while next_factor_idx < new_factors.len() {
                let (next_factor, next_factor_exp) = get_exp(&new_factors[next_factor_idx]);

                if current_factor == next_factor {
                    // bases must be strictly equal
                    // if they are, apply a^b*a^c = a^(b+c)
                    current_factor_exp += next_factor_exp;
                    new_factors.swap_remove(next_factor_idx);
                } else if current_factor_exp == next_factor_exp
                    && current_factor.is_number() && next_factor.is_number() {
                    // degrees must be strictly equal
                    // if they are, apply a^c*b^c = (a*b)^c
                    current_factor *= next_factor;
                    new_factors.swap_remove(next_factor_idx);
                } else {
                    next_factor_idx += 1;
                }
            }

            // after all combining, update the current factor
            if current_factor_exp.as_number() == Some(&ONE) {
                new_factors[current_factor_idx] = current_factor;
            } else {
                new_factors[current_factor_idx] = Expr::Exp(
                    Box::new(current_factor),
                    Box::new(current_factor_exp),
                );
            }

            current_factor_idx += 1;
        }

        if new_factors.len() == factors.len() {
            None
        } else if new_factors.len() == 1 {
            Some(new_factors.remove(0))
        } else {
            Some(Expr::Mul(new_factors))
        }
    })?;

    step_collector.push(Step::CombineLikeFactors);
    Some(opt)
}

/// Applies all multiplication rules.
///
/// All multiplication rules will reduce the complexity of the expression.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    multiply_zero(expr, step_collector)
        .or_else(|| multiply_one(expr, step_collector))
        .or_else(|| reduce_numerical_fraction(expr, step_collector))
        .or_else(|| combine_like_factors(expr, step_collector))
}
