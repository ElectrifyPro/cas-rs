//! Simplification rules for expressions involving addition, including combining like terms.

use crate::{
    algebra::{expr::{Expr, Primary}, simplify::step::Step},
    step::StepCollector,
};

/// If the expression is an add expression, calls the given transformation function with the terms.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
fn do_add(expr: &Expr, f: impl Copy + Fn(&[Expr]) -> Option<Expr>) -> Option<Expr> {
    if let Expr::Add(terms) = expr {
        f(terms)
    } else {
        None
    }
}

/// `0+a = a`
/// `a+0 = a`
pub fn add_zero(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_add(expr, |terms| {
        let mut new_terms = terms.iter()
            .filter(|term| {
                if let Expr::Primary(Primary::Number(n)) = term {
                    if n == "0" {
                        return false;
                    }
                }

                true
            })
            .cloned()
            .collect::<Vec<_>>();

        if new_terms.len() == terms.len() {
            None
        } else if new_terms.is_empty() {
            Some(Expr::Primary(Primary::Number("0".to_string())))
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

/// Applies all addition rules.
///
/// All addition rules will reduce the complexity of the expression.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    add_zero(expr, step_collector)
}
