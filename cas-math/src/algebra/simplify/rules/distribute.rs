//! Simplification rules related to the distributive property.

use crate::{
    algebra::{expr::Expr, simplify::step::Step},
    step::StepCollector,
};

/// If the expression is a multiplication expression, calls the given transformation function with
/// the factors.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
fn do_multiply(expr: &Expr, f: impl Copy + Fn(&[Expr]) -> Option<Expr>) -> Option<Expr> {
    if let Expr::Mul(factors) = expr {
        f(factors)
    } else {
        None
    }
}

/// `a*(b+c) = a*b + a*c`
pub fn distributive_property(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_multiply(expr, |factors| {
        // find the first `Expr::Add`, and distribute every other factor over it
        let mut factors_to_distribute = factors.to_vec();
        let add_factor_terms = {
            let idx = factors_to_distribute.iter()
                .position(|factor| matches!(factor, Expr::Add(_)));
            if let Some(idx) = idx {
                if let Expr::Add(terms) = factors_to_distribute.swap_remove(idx) {
                    terms
                } else {
                    unreachable!()
                }
            } else {
                return None;
            }
        };

        let new_terms = add_factor_terms.into_iter()
            .map(|term| {
                Expr::Mul(factors_to_distribute.clone()) * term
            })
            .collect::<Vec<_>>();
        Some(Expr::Add(new_terms))
    })?;

    // keep the step collection logic outside of the closure to make it implement `Fn`
    step_collector.push(Step::DistributiveProperty);
    Some(opt)
}

/// Applies all distribution rules.
///
/// The distributive property may or may not reduce the complexity of the expression, since it can
/// introduce additional operations. However, it may be necessary for future rules to apply.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    distributive_property(expr, step_collector)
}
