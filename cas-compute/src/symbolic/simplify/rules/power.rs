//! Simplification rules for power expressions.

use crate::primitive::int;
use crate::symbolic::{
    expr::{SymExpr, Primary},
    simplify::{rules::do_power, step::Step},
    step_collector::StepCollector,
};
use rug::ops::Pow;

/// `a^0 = 1`
///
/// `0^0` is defined as `1` by this rule, though it may be undefined in other mathematical
/// contexts.
pub fn power_zero(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |_, rhs| {
        if rhs.as_integer()?.is_zero() {
            Some(SymExpr::Primary(Primary::Integer(int(1))))
        } else {
            None
        }
    })?;

    // keep the step collection logic outside of the closure to make it implement `Fn`
    step_collector.push(Step::PowerZero);
    Some(opt)
}

/// `0^a = 0`
///
/// `0^0` is handled by the [`power_zero`] rule.
pub fn power_zero_left(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |lhs, _| {
        if lhs.as_integer()?.is_zero() {
            Some(SymExpr::Primary(Primary::Integer(int(0))))
        } else {
            None
        }
    })?;

    step_collector.push(Step::PowerZeroLeft);
    Some(opt)
}

/// `1^a = 1`
pub fn power_one_left(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |lhs, _| {
        if lhs.as_integer()? == &1 {
            Some(SymExpr::Primary(Primary::Integer(int(1))))
        } else {
            None
        }
    })?;

    step_collector.push(Step::PowerOneLeft);
    Some(opt)
}

/// `a^1 = a`
pub fn power_one(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |lhs, rhs| {
        if rhs.as_integer()? == &1 {
            Some(lhs.clone())
        } else {
            None
        }
    })?;

    step_collector.push(Step::PowerOne);
    Some(opt)
}

/// `(a^b)^c = a^(b*c)`
pub fn power_power(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |lhs, rhs| {
        if let SymExpr::Exp(base, exponent) = lhs {
            return Some(SymExpr::Exp(
                Box::new(*base.clone()),
                Box::new(*exponent.clone() * rhs.clone()),
            ));
        }

        None
    })?;

    step_collector.push(Step::PowerPower);
    Some(opt)
}

/// Simplifies integer powers.
pub fn integer(expr: &SymExpr, _: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    do_power(expr, |lhs, rhs| {
        if let Some(lhs) = lhs.as_integer() {
            if let Some(rhs) = rhs.as_integer() {
                return Some(SymExpr::Primary(Primary::Integer(lhs.pow(rhs.to_u32()?).into())));
            }
        }

        None
    })
}

/// Applies all power rules.
///
/// All power rules will reduce the complexity of the expression.
pub fn all(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    power_zero(expr, step_collector)
        .or_else(|| power_zero_left(expr, step_collector))
        .or_else(|| power_one_left(expr, step_collector))
        .or_else(|| power_one(expr, step_collector))
        .or_else(|| power_power(expr, step_collector))
        .or_else(|| integer(expr, step_collector))
}
