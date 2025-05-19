//! Implementation of many simplification rules.
//!
//! Each rule in this module is a function that takes the expression to simplify as an argument,
//! and returns `Some(expr)` with the simplified expression if the rule applies, or `None` if the
//! rule does not apply.

pub mod add;
pub mod distribute;
pub mod imaginary;
pub mod multiply;
pub mod power;
pub mod root;
pub mod trigonometry;

use crate::symbolic::step_collector::StepCollector;
use super::{SymExpr, Primary, step::Step};

/// If the expression is a function call with the given function name, calls the given
/// transformation function with the arguments.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
pub(crate) fn do_call(
    expr: &SymExpr,
    name: &str,
    f: impl Copy + Fn(&[SymExpr]) -> Option<SymExpr>,
) -> Option<SymExpr> {
    if let SymExpr::Primary(Primary::Call(target_name, args)) = expr {
        if target_name == name {
            return f(args);
        }
    }

    None
}

/// If the expression is an add expression, calls the given transformation function with the terms.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
pub(crate) fn do_add(expr: &SymExpr, f: impl Copy + Fn(&[SymExpr]) -> Option<SymExpr>) -> Option<SymExpr> {
    if let SymExpr::Add(terms) = expr {
        f(terms)
    } else {
        None
    }
}

/// If the expression is a multiplication expression, calls the given transformation function with
/// the factors.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
pub(crate) fn do_multiply(expr: &SymExpr, f: impl Copy + Fn(&[SymExpr]) -> Option<SymExpr>) -> Option<SymExpr> {
    if let SymExpr::Mul(factors) = expr {
        f(factors)
    } else {
        None
    }
}

/// If the expression is a power expression, calls the given transformation function with the left
/// and right-hand-side of the power.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
pub(crate) fn do_power(expr: &SymExpr, f: impl Copy + Fn(&SymExpr, &SymExpr) -> Option<SymExpr>) -> Option<SymExpr> {
    if let SymExpr::Exp(lhs, rhs) = expr {
        f(lhs, rhs)
    } else {
        None
    }
}

/// Applies all rules.
pub fn all(expr: &SymExpr, step_collector: &mut dyn StepCollector<Step>) -> Option<SymExpr> {
    add::all(expr, step_collector)
        .or_else(|| multiply::all(expr, step_collector))
        .or_else(|| power::all(expr, step_collector))
        .or_else(|| distribute::all(expr, step_collector))
        .or_else(|| imaginary::all(expr, step_collector))
        .or_else(|| trigonometry::all(expr, step_collector))
        .or_else(|| root::all(expr, step_collector))
}
