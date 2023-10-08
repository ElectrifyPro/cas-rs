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

/// Applies all rules.
pub fn all(expr: &Expr, step_collector: &mut dyn StepCollector<Step>) -> Option<Expr> {
    add::all(expr, step_collector)
        .or_else(|| multiply::all(expr, step_collector))
        .or_else(|| power::all(expr, step_collector))
        .or_else(|| distribute::all(expr, step_collector))
}
