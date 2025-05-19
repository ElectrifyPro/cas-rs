//! Simplification rules for expressions involving the imaginary unit.

use crate::primitive::int;
use crate::symbolic::{
    expr::{SymExpr, Primary},
    simplify::{rules::do_power, step::Step},
    step_collector::StepCollector,
};

// i^..
// 0 1 2  3
// 1 i -1 -i
//
// 4 5 6  7
// 1 i -1 -i

/// `i^(4n) = 1`
///
/// `i^0` can be handled by `power_zero`, but this rule is more general.
pub fn i_pow_0(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |lhs, rhs| {
        if lhs.as_symbol()? == "i" && int(rhs.as_integer()? % 4).is_zero() {
            Some(SymExpr::Primary(Primary::Integer(int(1))))
        } else {
            None
        }
    })?;

    step_collector.push(Step::I0);
    Some(opt)
}

/// `i^(4n+1) = i`
pub fn i_pow_1(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |lhs, rhs| {
        if lhs.as_symbol()? == "i" && int(rhs.as_integer()? % 4) == 1 {
            Some(SymExpr::Primary(Primary::Symbol("i".to_string())))
        } else {
            None
        }
    })?;

    step_collector.push(Step::I1);
    Some(opt)
}

/// `i^(4n+2) = -1`
pub fn i_pow_2(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |lhs, rhs| {
        if lhs.as_symbol()? == "i" && int(rhs.as_integer()? % 4) == 2 {
            Some(SymExpr::Primary(Primary::Integer(int(-1))))
        } else {
            None
        }
    })?;

    step_collector.push(Step::I2);
    Some(opt)
}

/// `i^(4n+3) = -i`
pub fn i_pow_3(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    let opt = do_power(expr, |lhs, rhs| {
        if lhs.as_symbol()? == "i" && int(rhs.as_integer()? % 4) == 3 {
            Some(-SymExpr::Primary(Primary::Symbol("i".to_string())))
        } else {
            None
        }
    })?;

    step_collector.push(Step::I3);
    Some(opt)
}

/// Applies all imaginary unit rules.
///
/// All imaginary unit rules will reduce the complexity of the expression.
pub fn all(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    i_pow_0(expr, step_collector)
        .or_else(|| i_pow_1(expr, step_collector))
        .or_else(|| i_pow_2(expr, step_collector))
        .or_else(|| i_pow_3(expr, step_collector))
}
