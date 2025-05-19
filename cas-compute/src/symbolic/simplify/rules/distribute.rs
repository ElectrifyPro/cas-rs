//! Simplification rules related to the distributive property.

use crate::symbolic::{
    expr::SymExpr,
    simplify::{rules::{do_multiply, do_power}, step::Step},
    step_collector::StepCollector,
};

/// `a*(b+c) = a*b + a*c`
pub fn distributive_property(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_multiply(expr, |factors| {
        // find the first `Expr::Add`, and distribute every other factor over it
        let mut factors_to_distribute = factors.to_vec();
        let add_factor_terms = {
            let idx = factors_to_distribute.iter()
                .position(|factor| matches!(factor, SymExpr::Add(_)));
            if let Some(idx) = idx {
                if let SymExpr::Add(terms) = factors_to_distribute.swap_remove(idx) {
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
                SymExpr::Mul(factors_to_distribute.clone()) * term
            })
            .collect::<Vec<_>>();
        Some(SymExpr::Add(new_terms))
    })?;

    // keep the step collection logic outside of the closure to make it implement `Fn`
    step_collector.push(Step::DistributiveProperty);
    Some(opt)
}

/// `(a*b)^c = a^c * b^c`
pub fn distribute_power(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |lhs, rhs| {
        if let SymExpr::Mul(factors) = lhs {
            let new_factors = factors.iter()
                .map(|factor| SymExpr::Exp(
                    Box::new(factor.clone()),
                    Box::new(rhs.clone()),
                ))
                .collect::<Vec<_>>();

            return Some(SymExpr::Mul(new_factors));
        }

        None
    })?;

    step_collector.push(Step::DistributePower);
    Some(opt)
}

/// Applies all distribution rules.
///
/// The distributive property may or may not reduce the complexity of the expression, since it can
/// introduce additional operations. However, it may be necessary for future rules to apply.
pub fn all(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    distributive_property(expr, step_collector)
        .or_else(|| distribute_power(expr, step_collector))
}
