//! Simplification rules for power expressions.

use crate::algebra::expr::{Expr, Primary};

/// Iterates through the given expression, applying the given transformation function to each power
/// expression. If the transformation function is called, it is guaranteed that the expression
/// provided to the function is a power expression.
///
/// Returns `Some(expr)` with the transformed expression if a transformation was applied.
fn do_power(expr: &Expr, f: impl Copy + Fn(&Expr) -> Option<Expr>) -> Option<Expr> {
    match expr {
        Expr::Primary(_) => None,
        Expr::Add(terms) => {
            for term in terms {
                if let Some(expr) = do_power(term, f) {
                    return Some(expr);
                }
            }
            None
        },
        Expr::Mul(factors) => {
            for factor in factors {
                if let Some(expr) = do_power(factor, f) {
                    return Some(expr);
                }
            }
            None
        },
        Expr::Exp(lhs, rhs) => {
            if let Some(expr) = do_power(lhs, f) {
                return Some(expr);
            }
            if let Some(expr) = do_power(rhs, f) {
                return Some(expr);
            }
            f(expr)
        },
    }
}

/// `a^0 = 1`
///
/// `0^0` is defined as `1` by this rule, though it may be undefined in other mathematical
/// contexts.
pub fn power_zero(expr: &Expr) -> Option<Expr> {
    do_power(expr, |power| {
        if let Expr::Exp(_, rhs) = power {
            if let Expr::Primary(Primary::Number(n)) = &**rhs {
                if n == "0" {
                    return Some(Expr::Primary(Primary::Number("1".to_string())));
                }
            }

            None
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    })
}

/// `0^a = 0`
///
/// `0^0` is handled by the [`power_zero`] rule.
pub fn power_zero_left(expr: &Expr) -> Option<Expr> {
    do_power(expr, |power| {
        if let Expr::Exp(lhs, _) = power {
            if let Expr::Primary(Primary::Number(n)) = &**lhs {
                if n == "0" {
                    return Some(Expr::Primary(Primary::Number("0".to_string())));
                }
            }

            None
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    })
}

/// `1^a = 1`
pub fn power_one_left(expr: &Expr) -> Option<Expr> {
    do_power(expr, |power| {
        if let Expr::Exp(lhs, _) = power {
            if let Expr::Primary(Primary::Number(n)) = &**lhs {
                if n == "1" {
                    return Some(Expr::Primary(Primary::Number("1".to_string())));
                }
            }

            None
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    })
}

/// `a^1 = a`
pub fn power_one(expr: &Expr) -> Option<Expr> {
    do_power(expr, |power| {
        if let Expr::Exp(_, rhs) = power {
            if let Expr::Primary(Primary::Number(n)) = &**rhs {
                if n == "1" {
                    return Some(Expr::Primary(Primary::Number("1".to_string())));
                }
            }

            None
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    })
}

/// Applies all power rules.
///
/// All power rules will reduce the complexity of the expression.
pub fn all(expr: &Expr) -> Option<Expr> {
    power_zero(expr)
        .or_else(|| power_zero_left(expr))
        .or_else(|| power_one_left(expr))
        .or_else(|| power_one(expr))
}
