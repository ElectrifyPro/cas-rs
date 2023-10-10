//! Simplification rules for expressions involving addition, including combining like terms.

use cas_eval::consts::ONE;
use crate::{
    algebra::{
        expr::{Expr, Primary},
        simplify::{fraction::{extract_explicit_frac, make_fraction}, rules::do_add, step::Step},
    },
    step::StepCollector,
};

/// Extension of the `+=` implementation for [`Expr`] to also support adding fractions.
fn add_assign(lhs: &mut Expr, mut rhs: Expr) {
    match (extract_explicit_frac(lhs), extract_explicit_frac(&mut rhs)) {
        (Some((num1, den1)), Some((num2, den2))) => {
            // (a / b) + (c / d) = (a*d + b*c) / (b*d)
            let numerator = num1 * &den2 + num2 * &den1;
            let denominator = den1 * den2;
            if denominator == 1 {
                *lhs = Expr::Primary(Primary::Number(numerator));
            } else {
                *lhs = make_fraction(
                    Expr::Primary(Primary::Number(numerator)),
                    Expr::Primary(Primary::Number(denominator)),
                );
            }
        },
        _ => *lhs += rhs,
    }
}

/// `0+a = a`
/// `a+0 = a`
pub fn add_zero(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_add(expr, |terms| {
        let new_terms = terms.iter()
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
        } else {
            Some(Expr::Add(new_terms).downgrade())
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
                Expr::Primary(Primary::Number(_)) => {
                    (expr.clone(), Expr::Primary(Primary::Number(ONE.clone())))
                },
                Expr::Mul(factors) => {
                    let mut factors = factors.clone();
                    let mut coeff = None;

                    let mut idx = 0;
                    while idx < factors.len() {
                        if factors[idx].is_constant() {
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
                        Expr::Mul(factors).downgrade(),
                    )
                },
                Expr::Exp(..) => {
                    if expr.is_number_recip() {
                        (expr.clone(), Expr::Primary(Primary::Number(ONE.clone())))
                    } else {
                        (Expr::Primary(Primary::Number(ONE.clone())), expr.clone())
                    }
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
                    add_assign(&mut current_term_coeff, next_term_coeff);
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
        } else {
            Some(Expr::Add(new_terms).downgrade())
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
