//! Simplification rules for expressions involving addition, including combining like terms.

use crate::primitive::int;
use crate::symbolic::{
    expr::{SymExpr, Primary},
    simplify::{fraction::{extract_explicit_frac, make_fraction, extract_fractional}, rules::do_add, step::Step},
    step_collector::StepCollector,
};

/// Extension of the `+=` implementation for [`Expr`] to also support adding fractions.
fn add_assign(lhs: &mut SymExpr, rhs: SymExpr) {
    // special case to use the `+=` implementation if both are floats, and don't try
    // `extract_explicit_frac`
    if lhs.is_float() && rhs.is_float() {
        *lhs += rhs;
        return;
    }

    match (extract_explicit_frac(&mut lhs.clone()), extract_explicit_frac(&mut rhs.clone())) {
        (Some((num1, den1)), Some((num2, den2))) => {
            // (a / b) + (c / d) = (a*d + b*c) / (b*d)
            let numerator = num1 * &den2 + num2 * &den1;
            let denominator = den1 * den2;
            if denominator == 1 {
                *lhs = SymExpr::Primary(Primary::Integer(numerator));
            } else {
                *lhs = make_fraction(
                    SymExpr::Primary(Primary::Integer(numerator)),
                    SymExpr::Primary(Primary::Integer(denominator)),
                );
            }
        },
        _ => *lhs += rhs,
    }
}

/// `0+a = a`
/// `a+0 = a`
pub fn add_zero(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_add(expr, |terms| {
        let new_terms = terms.iter()
            .filter(|term| {
                // keep all non-zero terms
                term.as_integer()
                    .map(|n| !n.is_zero())
                    .unwrap_or(true)
            })
            .cloned()
            .collect::<Vec<_>>();

        if new_terms.len() == terms.len() {
            None
        } else {
            Some(SymExpr::Add(new_terms).downgrade())
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
pub fn combine_like_terms(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_add(expr, |terms| {
        let mut new_terms = terms.to_vec();
        let mut current_term_idx = 0;

        /// Utility function to extract the rational coefficient and factors of an expression. If
        /// the expression is not [`Expr::Mul`], the coefficient is 1.
        ///
        /// - `5` -> `(5, 1)`
        /// - `3*a` -> `(3, a)`
        /// - `1/4*a*b` -> `(1/4, a*b)`
        /// - `sqrt(6)` -> `(1, sqrt(6))`
        /// - `a` -> `(1, a)`
        fn get_coeff(expr: &SymExpr) -> (SymExpr, SymExpr) {
            match expr {
                SymExpr::Primary(Primary::Integer(_)) | SymExpr::Primary(Primary::Float(_)) => {
                    (expr.clone(), SymExpr::Primary(Primary::Integer(int(1))))
                },
                SymExpr::Mul(factors) => {
                    let mut factors = factors.clone();
                    let fraction = extract_fractional(&mut factors)
                        .unwrap_or(SymExpr::Primary(Primary::Integer(int(1))));

                    (
                        fraction,
                        SymExpr::Mul(factors).downgrade(),
                    )
                },
                SymExpr::Exp(..) => {
                    if expr.is_integer_recip() {
                        (expr.clone(), SymExpr::Primary(Primary::Integer(int(1))))
                    } else {
                        (SymExpr::Primary(Primary::Integer(int(1))), expr.clone())
                    }
                },
                _ => (SymExpr::Primary(Primary::Integer(int(1))), expr.clone()),
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

            if current_term_coeff.as_integer().map(|n| n == &1).unwrap_or(false) {
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
            Some(SymExpr::Add(new_terms).downgrade())
        }
    })?;

    step_collector.push(Step::CombineLikeTerms);
    Some(opt)
}

/// Applies all addition rules.
///
/// All addition rules will reduce the complexity of the expression.
pub fn all(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    add_zero(expr, step_collector)
        .or_else(|| combine_like_terms(expr, step_collector))
}
