//! Module to simplify expressions.
//!
//! This module provides the [`simplify`] function, which attempts to reduce the complexity of an
//! expression. It does this by repeatedly applying rewriting rules to the expression in multiple
//! passes, until no more rules apply.
//!
//! Complexity is an informal, arbitrary metric that is used to determine whether one expression is
//! simpler than another. The default complexity heuristic used is [`default_complexity`] (click
//! for more information). However, this can be overridden by providing a custom complexity
//! function to the ``simplify_with`` function.

pub mod rules;

use super::expr::{Expr, Primary};

/// The default complexity heuristic function.
///
/// This function computes complexity using these simple rules:
///
/// - `complexity(number) = length(number)`
/// - `complexity(symbol) = length(symbol)`
/// - `complexity(call) = length(name) + length(args)`
/// - `complexity(add) = 3 + sum(complexity(terms))`
/// - `complexity(mul) = 2 + sum(complexity(factors))`
/// - `complexity(exp) = 1 + complexity(lhs) + complexity(rhs)`
pub fn default_complexity(expr: &Expr) -> usize {
    let mut complexity = 0;
    let mut stack = vec![expr];
    while let Some(expr) = stack.pop() {
        complexity += match expr {
            Expr::Primary(primary) => {
                match primary {
                    Primary::Number(num) => num.len(),
                    Primary::Symbol(sym) => sym.len(),
                    Primary::Call(name, args) => name.len() + args.len(),
                }
            },
            Expr::Add(terms) => {
                stack.extend(terms.iter());
                3
            },
            Expr::Mul(factors) => {
                stack.extend(factors.iter());
                2
            },
            Expr::Exp(lhs, rhs) => {
                stack.push(lhs);
                stack.push(rhs);
                1
            },
        };
    }
    complexity
}

/// Simplify the given expression, using the default complexity heuristic function.
pub fn simplify(expr: &Expr) -> Expr {
    simplify_with(expr, default_complexity)
}

/// Simplify the given expression, using the given complexity heuristic function.
///
/// The complexity heuristic function should return a number that represents the complexity of the
/// given expression. The lower the number, the simpler the expression.
pub fn simplify_with<F>(expr: &Expr, complexity: F) -> Expr
where
    F: Fn(&Expr) -> usize,
{
    let mut expr = expr.clone();
    loop {
        // TODO use complexity
        let mut current_complexity = complexity(&expr);
        let mut changed = false;
        if let Some(new_expr) = rules::all(&expr) {
            changed = true;
            expr = new_expr;
        }
        if !changed {
            break;
        }
    }

    expr
}

#[cfg(test)]
mod tests {
    use cas_parser::parser::{ast::expr::Expr as AstExpr, Parser};
    use pretty_assertions::assert_eq;
    use super::*;

    #[test]
    fn power_rules() {
        let input = String::from("(1^0)^(3x+5b^2i)^1^(3a)");
        let expr = Parser::new(&input).try_parse_full::<AstExpr>().unwrap();
        let math_expr = Expr::from(expr);
        let simplified_expr = simplify(&math_expr);
        assert_eq!(simplified_expr, Expr::Primary(Primary::Number("1".to_string())));
    }
}
