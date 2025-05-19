//! Simplification rules for expressions involving multiplication, including combining like
//! factors.

use crate::primitive::int;
use crate::symbolic::{
    expr::{SymExpr, Primary},
    simplify::{fraction::{extract_integer_fraction, make_fraction}, rules::do_multiply, step::Step},
    step_collector::StepCollector,
};

/// `0*a = 0`
/// `a*0 = 0`
pub fn multiply_zero(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_multiply(expr, |factors| {
        if factors.iter().any(|factor| factor.as_integer().map(|n| n.is_zero()).unwrap_or(false)) {
            Some(SymExpr::Primary(Primary::Integer(int(0))))
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
pub fn multiply_one(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_multiply(expr, |factors| {
        let new_factors = factors.iter()
            .filter(|factor| {
                // keep all non-one factors
                factor.as_integer()
                    .map(|n| n != &1)
                    .unwrap_or(true)
            })
            .cloned()
            .collect::<Vec<_>>();

        if new_factors.len() == factors.len() {
            None
        } else {
            Some(SymExpr::Mul(new_factors).downgrade())
        }
    })?;

    step_collector.push(Step::MultiplyOne);
    Some(opt)
}

/// Simplifies numerical fractions.
///
/// `3/12 = 1/4`
/// `12/3 = 4`
pub fn reduce_numerical_fraction(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_multiply(expr, |factors| {
        let mut new_factors = factors.to_vec();

        // extract a fraction, a Integer and a Integer^-1
        let (numerator, denominator) = extract_integer_fraction(&mut new_factors, false, false)?;

        // reduce the fraction
        let gcd = numerator.clone().gcd(&denominator.clone());
        if gcd == 1 {
            return None;
        }

        // insert the reduced fraction back into the factors
        Some(SymExpr::Mul(new_factors) * make_fraction(
            SymExpr::Primary(Primary::Integer(numerator / &gcd)),
            SymExpr::Primary(Primary::Integer(denominator / &gcd)),
        ))
    })?;

    step_collector.push(Step::ReduceFraction);
    Some(opt)
}

/// Combines like factors.
///
/// `a^b*a^c = a^(b+c)`
/// `a^c*b^c = (a*b)^c`
/// etc.
pub fn combine_like_factors(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_multiply(expr, |factors| {
        let mut new_factors = factors.to_vec();
        let mut current_factor_idx = 0;

        /// Utility function to extract the base and exponent of an expression. If the expression
        /// is not [`Expr::Exp`], the exponent is `1`.
        ///
        /// - `a^b` -> `(a, b)`
        /// - `a` -> `(a, 1)`
        fn get_exp(expr: &SymExpr) -> (SymExpr, SymExpr) {
            match expr {
                SymExpr::Exp(lhs, rhs) => (*lhs.clone(), *rhs.clone()),
                expr => (expr.clone(), SymExpr::Primary(Primary::Integer(int(1)))),
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

                if current_factor_exp == next_factor_exp
                    && (current_factor.is_integer() && next_factor.is_integer()
                        || current_factor.is_float() && next_factor.is_float()) {
                    // degrees must be strictly equal
                    // if they are, apply a^c*b^c = (a*b)^c
                    current_factor *= next_factor;
                    new_factors.swap_remove(next_factor_idx);
                } else if current_factor == next_factor {
                    // bases must be strictly equal
                    // if they are, apply a^b*a^c = a^(b+c)
                    current_factor_exp += next_factor_exp;
                    new_factors.swap_remove(next_factor_idx);
                } else {
                    next_factor_idx += 1;
                }
            }

            // after all combining, update the current factor
            if current_factor_exp.as_integer().map(|n| n == &1).unwrap_or(false) {
                new_factors[current_factor_idx] = current_factor;
            } else {
                new_factors[current_factor_idx] = SymExpr::Exp(
                    Box::new(current_factor),
                    Box::new(current_factor_exp),
                );
            }

            current_factor_idx += 1;
        }

        if new_factors.len() == factors.len() {
            None
        } else {
            Some(SymExpr::Mul(new_factors).downgrade())
        }
    })?;

    step_collector.push(Step::CombineLikeFactors);
    Some(opt)
}

/// Applies all multiplication rules.
///
/// All multiplication rules will reduce the complexity of the expression.
pub fn all(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    multiply_zero(expr, step_collector)
        .or_else(|| multiply_one(expr, step_collector))
        .or_else(|| reduce_numerical_fraction(expr, step_collector))
        .or_else(|| combine_like_factors(expr, step_collector))
}
