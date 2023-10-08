//! Implementation of many simplification rules.
//!
//! Each rule in this module is a function that takes the expression to simplify as an argument,
//! and returns `Some(expr)` with the simplified expression if the rule applies, or `None` if the
//! rule does not apply.

pub mod add;
pub mod distribute;
pub mod multiply;
pub mod power;

use crate::step::StepCollector;
use super::{Expr, step::Step};

/// If the expression is an add expression, calls the given transformation function with the terms.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
pub(crate) fn do_add(expr: &Expr, f: impl Copy + Fn(&[Expr]) -> Option<Expr>) -> Option<Expr> {
    if let Expr::Add(terms) = expr {
        f(terms)
    } else {
        None
    }
}

/// If the expression is a multiplication expression, calls the given transformation function with
/// the factors.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
pub(crate) fn do_multiply(expr: &Expr, f: impl Copy + Fn(&[Expr]) -> Option<Expr>) -> Option<Expr> {
    if let Expr::Mul(factors) = expr {
        f(factors)
    } else {
        None
    }
}

/// If the expression is a power expression, calls the given transformation function with the left
/// and right-hand-side of the power.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
pub(crate) fn do_power(expr: &Expr, f: impl Copy + Fn(&Expr, &Expr) -> Option<Expr>) -> Option<Expr> {
    if let Expr::Exp(lhs, rhs) = expr {
        f(lhs, rhs)
    } else {
        None
    }
}

/// Applies all rules.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    add::all(expr, step_collector)
        .or_else(|| multiply::all(expr, step_collector))
        .or_else(|| power::all(expr, step_collector))
        .or_else(|| distribute::all(expr, step_collector))
}
