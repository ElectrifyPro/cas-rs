//! Simplification rules for expressions involving multiplication, including combining like
//! factors.

use cas_eval::consts::{ZERO, ONE};
use crate::{
    algebra::{expr::{Expr, Primary}, simplify::step::Step},
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

/// `0*a = 0`
/// `a*0 = 0`
pub fn multiply_zero(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_multiply(expr, |factors| {
        for factor in factors {
            if let Expr::Primary(Primary::Number(n)) = factor {
                if n.is_zero() {
                    return Some(Expr::Primary(Primary::Number(ZERO.clone())));
                }
            }
        }

        None
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
                if let Expr::Primary(Primary::Number(n)) = factor {
                    if n == &1 {
                        return false;
                    }
                }

                true
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

/// Applies all multiplication rules.
///
/// All multiplication rules will reduce the complexity of the expression.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    multiply_zero(expr, step_collector)
        .or_else(|| multiply_one(expr, step_collector))
}
