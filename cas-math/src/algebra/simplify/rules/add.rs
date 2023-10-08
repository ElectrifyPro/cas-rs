//! Simplification rules for expressions involving addition, including combining like terms.

use cas_eval::consts::{ZERO, ONE};
use crate::{
    algebra::{expr::{Expr, Primary}, simplify::{rules::do_add, step::Step}},
    step::StepCollector,
};

/// `0+a = a`
/// `a+0 = a`
pub fn add_zero(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_add(expr, |terms| {
        let mut new_terms = terms.iter()
            .filter(|term| {
                // keep all non-zero terms
                term.as_number()
                    .map(|n| !n.is_zero())
                    .unwrap_or(true)
            })
            .cloned()
            .collect::<Vec<_>>();

        if new_terms.len() == terms.len() {
            None
        } else if new_terms.is_empty() {
            Some(Expr::Primary(Primary::Number(ZERO.clone())))
        } else if new_terms.len() == 1 {
            Some(new_terms.remove(0))
        } else {
            Some(Expr::Add(new_terms))
        }
    })?;

    // keep the step collection logic outside of the closure to make it implement `Fn`
    step_collector.push(Step::AddZero);
    Some(opt)
}

/// Combines like terms.
///
/// `a+a = 2a`
/// `a+a+a = 3a`
/// `2a+3a = 5a`
/// etc.
pub fn combine_like_terms(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_add(expr, |terms| {
        let mut new_terms = terms.to_vec();
        let mut current_term_idx = 0;

        /// Utility function to extract the numerical coefficient and factors of an expression. If
        /// the expression is not [`Expr::Mul`], the coefficient is 1.
        ///
        /// - `5` -> `(5, 1)`
        /// - `3*a` -> `(3, a)`
        /// - `a` -> `(1, a)`
        fn get_coeff(expr: &Expr) -> (Expr, Expr) {
            match expr {
                Expr::Primary(primary) => {
                    match primary {
                        Primary::Number(_) =>
                            (expr.clone(), Expr::Primary(Primary::Number(ONE.clone()))),
                        _ => (Expr::Primary(Primary::Number(ONE.clone())), expr.clone()),
                    }
                },
                Expr::Mul(factors) => {
                    let mut factors = factors.clone();
                    let mut coeff = None;

                    let mut idx = 0;
                    while idx < factors.len() {
                        if factors[idx].is_number() {
                            let n = factors.swap_remove(idx);
                            // NOTE: coeff.map() doesn't work because `n` is moved into the map
                            // closure
                            coeff = match coeff {
                                Some(c) => Some(c * n),
                                None => Some(n),
                            };
                        } else {
                            idx += 1;
                        }
                    }

                    (
                        coeff.unwrap_or_else(|| Expr::Primary(Primary::Number(ONE.clone()))),
                        Expr::Mul(factors),
                    )
                },
                _ => (Expr::Primary(Primary::Number(ONE.clone())), expr.clone()),
            }
        }

        // this is O(n^2) worst case, due to scanning the whole vec for each term
        // TODO: optimize with hashing?
        while current_term_idx < new_terms.len() {
            let (mut current_term_coeff, current_term_factors) = get_coeff(&new_terms[current_term_idx]);

            // look at every term after `current_term`
            let mut next_term_idx = current_term_idx + 1;
            while next_term_idx < new_terms.len() {
                let (next_term_coeff, next_term_factors) = get_coeff(&new_terms[next_term_idx]);

                // factors must be strictly equal
                if current_term_factors == next_term_factors {
                    // if so, apply a*n + a*m = (n+m)*a
                    current_term_coeff += next_term_coeff;
                    new_terms.swap_remove(next_term_idx);
                } else {
                    next_term_idx += 1;
                }
            }

            if current_term_coeff.as_number() == Some(&ONE) {
                new_terms[current_term_idx] = current_term_factors;
            } else {
                new_terms[current_term_idx] =
                    current_term_coeff * current_term_factors;
            }

            current_term_idx += 1;
        }

        if new_terms.len() == terms.len() {
            None
        } else if new_terms.len() == 1 {
            Some(new_terms.remove(0))
        } else {
            Some(Expr::Add(new_terms))
        }
    })?;

    step_collector.push(Step::CombineLikeFactors);
    Some(opt)
}

/// Applies all addition rules.
///
/// All addition rules will reduce the complexity of the expression.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    add_zero(expr, step_collector)
        .or_else(|| combine_like_terms(expr, step_collector))
}
