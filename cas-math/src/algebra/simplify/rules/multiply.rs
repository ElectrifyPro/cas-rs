//! Simplification rules for expressions involving multiplication, including combining like
//! factors.

use cas_eval::consts::{ZERO, ONE};
use crate::{
    algebra::{expr::{Expr, Primary}, simplify::{rules::do_multiply, step::Step}},
    step::StepCollector,
};

/// `0*a = 0`
/// `a*0 = 0`
pub fn multiply_zero(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_multiply(expr, |factors| {
        if factors.iter().any(|factor| factor.as_number() == Some(&ZERO)) {
            return Some(Expr::Primary(Primary::Number(ZERO.clone())));
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

/// Combines like factors.
///
/// `a*a = a^2`
/// `a*a*a = a^3`
/// `a^2*a^3 = a^5`
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
            let (current_factor, mut current_factor_exp) = get_exp(&new_factors[current_factor_idx]);

            // look at every factor after `current_factor`
            let mut next_factor_idx = current_factor_idx + 1;
            while next_factor_idx < new_factors.len() {
                let (next_factor, next_factor_exp) = get_exp(&new_factors[next_factor_idx]);

                // bases must be strictly equal
                if current_factor == next_factor {
                    // if so, apply a^n * a^m = a^(n+m)
                    current_factor_exp += next_factor_exp;
                    new_factors.swap_remove(next_factor_idx);
                } else {
                    next_factor_idx += 1;
                }
            }

            if current_factor_exp.as_number() != Some(&ONE) {
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
        .or_else(|| combine_like_factors(expr, step_collector))
}
