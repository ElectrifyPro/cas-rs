//! Simplification rules for power expressions.

use cas_eval::consts::{ZERO, ONE};
use crate::{
    algebra::{expr::{Expr, Primary}, simplify::step::Step},
    step::StepCollector,
};

/// If the expression is a power expression, calls the given transformation function with the left
/// and right-hand-side of the power.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
fn do_power(expr: &Expr, f: impl Copy + Fn(&Expr, &Expr) -> Option<Expr>) -> Option<Expr> {
    if let Expr::Exp(lhs, rhs) = expr {
        f(lhs, rhs)
    } else {
        None
    }
}

/// `a^0 = 1`
///
/// `0^0` is defined as `1` by this rule, though it may be undefined in other mathematical
/// contexts.
pub fn power_zero(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_power(expr, |_, rhs| {
        if let Expr::Primary(Primary::Number(n)) = rhs {
            if n.is_zero() {
                return Some(Expr::Primary(Primary::Number(ONE.clone())));
            }
        }

        None
    })?;

    // keep the step collection logic outside of the closure to make it implement `Fn`
    step_collector.push(Step::PowerZero);
    Some(opt)
}

/// `0^a = 0`
///
/// `0^0` is handled by the [`power_zero`] rule.
pub fn power_zero_left(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_power(expr, |lhs, _| {
        if let Expr::Primary(Primary::Number(n)) = lhs {
            if n.is_zero() {
                return Some(Expr::Primary(Primary::Number(ZERO.clone())));
            }
        }

        None
    })?;

    step_collector.push(Step::PowerZeroLeft);
    Some(opt)
}

/// `1^a = 1`
pub fn power_one_left(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_power(expr, |lhs, _| {
        if let Expr::Primary(Primary::Number(n)) = lhs {
            if n == &1 {
                return Some(Expr::Primary(Primary::Number(ONE.clone())));
            }
        }

        None
    })?;

    step_collector.push(Step::PowerOneLeft);
    Some(opt)
}

/// `a^1 = a`
pub fn power_one(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    let opt = do_power(expr, |lhs, rhs| {
        if let Expr::Primary(Primary::Number(n)) = rhs {
            if n == &1 {
                return Some(lhs.clone());
            }
        }

        None
    })?;

    step_collector.push(Step::PowerOne);
    Some(opt)
}

/// Applies all power rules.
///
/// All power rules will reduce the complexity of the expression.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    power_zero(expr, step_collector)
        .or_else(|| power_zero_left(expr, step_collector))
        .or_else(|| power_one_left(expr, step_collector))
        .or_else(|| power_one(expr, step_collector))
}
