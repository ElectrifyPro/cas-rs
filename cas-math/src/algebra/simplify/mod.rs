//! Simplify expressions algebraically.
//!
//! This module provides the [`simplify`] function, which attempts to reduce the complexity of an
//! expression. It does this by repeatedly applying rewriting rules to the expression in multiple
//! passes, until no more rules apply.
//!
//! Complexity is an informal, arbitrary metric that is used to determine whether one expression is
//! simpler than another. The default complexity heuristic used is [`default_complexity`] (click
//! for more information). However, this can be overridden by providing a custom complexity
//! function to the [`simplify_with`] function.
//!
//! It is also possible to collect the simplification steps taken during simplification, using
//! [`simplify_with_steps`]. This is useful for debugging, and also for displaying the steps taken
//! to the user.
//!
//! # Integers and floating-point numbers
//!
//! Expressions are allowed to contain both integers and floating-point numbers. The simplifier
//! will attempt to keep expressions in the same number type as the input expression, but in the
//! case where two different numeric types are combined, the result will be represented as a
//! rational number.

pub mod fraction;
pub mod rules;
pub mod step;

use crate::step::StepCollector;
use cas_eval::consts::{float, int};
use step::Step;
use super::expr::{Expr, Primary};

/// The default complexity heuristic function.
///
/// This function computes complexity using these simple rules:
///
/// - `complexity(number) = abs(number)`
/// - `complexity(symbol) = length(symbol)`
/// - `complexity(call) = length(name) + length(args)`
/// - `complexity(add) = 3 + sum(complexity(terms))`
/// - `complexity(mul) = 2 + sum(complexity(factors))`
/// - `complexity(exp) = 1 + complexity(lhs) + complexity(rhs)`
pub fn default_complexity(expr: &Expr) -> usize {
    expr.post_order_iter()
        .map(|expr| match expr {
            Expr::Primary(primary) => {
                match primary {
                    Primary::Integer(num) => int(num.abs_ref())
                        .to_usize().unwrap(),
                    Primary::Float(num) => float(num.abs_ref())
                        .to_integer().unwrap()
                        .to_usize().unwrap(),
                    Primary::Symbol(sym) => sym.len(),
                    Primary::Call(name, args) => name.len() + args.len(),
                }
            },
            Expr::Add(terms) => 3 + terms.len(),
            Expr::Mul(factors) => 2 + factors.len(),
            Expr::Exp(_, _) => 1,
        })
        .sum()
}

/// Base implementation of the simplification algorithm.
pub(crate) fn inner_simplify_with<F>(
    expr: &Expr,
    complexity: F,
    step_collector: &mut dyn StepCollector<Step>,
) -> (Expr, bool)
where
    F: Copy + Fn(&Expr) -> usize,
{
    let mut expr = expr.clone();
    let mut changed_at_least_once = false;

    loop {
        // TODO: use complexity
        let mut current_complexity = complexity(&expr);
        let mut changed_in_this_pass = false;

        // try to simplify this expression using all rules
        if let Some(new_expr) = rules::all(&expr, step_collector) {
            expr = new_expr;
            changed_in_this_pass = true;
            changed_at_least_once = true;
            continue;
        }

        // then begin recursing into the expression's children
        match expr {
            Expr::Primary(primary) => return (Expr::Primary(primary), changed_at_least_once),
            Expr::Add(ref terms) => {
                let mut output = Expr::Add(Vec::new());
                for term in terms {
                    let result = inner_simplify_with(term, complexity, step_collector);
                    output += result.0;

                    // use |= instead of = to not reset these variables to false if already true
                    changed_in_this_pass |= result.1;
                    changed_at_least_once |= result.1;
                }
                expr = output;
            },
            Expr::Mul(ref mut factors) => {
                let mut output = Expr::Mul(Vec::new());
                for factor in factors.iter_mut() {
                    let result = inner_simplify_with(factor, complexity, step_collector);
                    output *= result.0;
                    changed_in_this_pass |= result.1;
                    changed_at_least_once |= result.1;
                }
                expr = output;
            },
            Expr::Exp(ref mut lhs, ref mut rhs) => {
                let result_l = inner_simplify_with(lhs, complexity, step_collector);
                let result_r = inner_simplify_with(rhs, complexity, step_collector);

                *lhs = Box::new(result_l.0);
                *rhs = Box::new(result_r.0);
                changed_in_this_pass |= result_l.1 || result_r.1;
                changed_at_least_once |= result_l.1 || result_r.1;
            },
        }

        if !changed_in_this_pass {
            break;
        }
    }

    (expr, changed_at_least_once)
}

/// Simplify the given expression, using the default complexity heuristic function.
pub fn simplify(expr: &Expr) -> Expr {
    inner_simplify_with(expr, default_complexity, &mut ()).0
}

/// Simplify the given expression, using the given complexity heuristic function.
///
/// The complexity heuristic function should return a number that represents the complexity of the
/// given expression. The lower the number, the simpler the expression.
pub fn simplify_with<F>(expr: &Expr, complexity: F) -> Expr
where
    F: Copy + Fn(&Expr) -> usize,
{
    inner_simplify_with(expr, complexity, &mut ()).0
}

/// Simplify the given expression, using the default complexity heuristic function. The steps taken
/// by the simplifier will also be collected and returned. This is useful for debugging, and also
/// for displaying the steps taken to the user.
pub fn simplify_with_steps(expr: &Expr) -> (Expr, Vec<Step>) {
    let mut steps = Vec::new();
    let expr = inner_simplify_with(expr, default_complexity, &mut steps).0;
    (expr, steps)
}

#[cfg(test)]
mod tests {
    use super::*;

    use cas_eval::consts::float_from_str;
    use cas_parser::parser::{ast::expr::Expr as AstExpr, Parser};
    use fraction::make_fraction;
    use pretty_assertions::assert_eq;

    /// Simplifies the given expression, returning the result as a [`Expr`].
    fn simplify_str(input: &str) -> Expr {
        let expr = Parser::new(input).try_parse_full::<AstExpr>().unwrap();
        simplify(&Expr::from(expr))
    }

    /// Simplifies the given expression, returning the result and steps taken.
    fn simplify_str_steps(input: &str) -> (Expr, Vec<Step>) {
        let expr = Parser::new(input).try_parse_full::<AstExpr>().unwrap();
        simplify_with_steps(&Expr::from(expr))
    }

    #[test]
    fn add_rules() {
        // also tests multiply_zero
        let simplified_expr = simplify_str("0+0*(3x+5b^2i)+0+(3a)");
        assert_eq!(simplified_expr, Expr::Mul(vec![
            Expr::Primary(Primary::Symbol(String::from("a"))),
            Expr::Primary(Primary::Integer(int(3))),
        ]));
    }

    #[test]
    fn add_fractions() {
        let simplified_expr = simplify_str("1/2 + 1/3 - 2 + 5/6");
        assert_eq!(simplified_expr, make_fraction(
            Expr::Primary(Primary::Integer(int(-1))),
            Expr::Primary(Primary::Integer(int(3))),
        ));
    }

    #[test]
    fn add_fractions_with_factors() {
        let simplified_expr = simplify_str("pi/2 + 2 - 1/3 - 5pi/6");
        assert_eq!(simplified_expr, Expr::Add(vec![
            make_fraction(
                Expr::Primary(Primary::Integer(int(5))),
                Expr::Primary(Primary::Integer(int(3))),
            ),
            make_fraction(
                -Expr::Primary(Primary::Symbol(String::from("pi"))),
                Expr::Primary(Primary::Integer(int(3))),
            ),
        ]));
    }

    #[test]
    fn combine_like_terms() {
        let simplified_expr = simplify_str("-9(6m-3) + 6(1+4m)");
        assert_eq!(simplified_expr, Expr::Add(vec![
            Expr::Mul(vec![
                Expr::Primary(Primary::Symbol(String::from("m"))),
                Expr::Primary(Primary::Integer(int(-30))),
            ]),
            Expr::Primary(Primary::Integer(int(33))),
        ]));
    }

    #[test]
    fn combine_like_terms_2() {
        let simplified_expr = simplify_str("3x^2y - 16x y + 5x y^2 + 2x^2y - 13x y + 4x y^2 + 2x y + 11x y^2 + x^3y");

        // x^3y + 20y^2x + 5x^2y - 27xy
        assert_eq!(simplified_expr, Expr::Add(vec![
            Expr::Mul(vec![
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Symbol(String::from("x")))),
                    Box::new(Expr::Primary(Primary::Integer(int(3)))),
                ),
                Expr::Primary(Primary::Symbol(String::from("y"))),
            ]),
            Expr::Mul(vec![
                Expr::Primary(Primary::Integer(int(20))),
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Symbol(String::from("y")))),
                    Box::new(Expr::Primary(Primary::Integer(int(2)))),
                ),
                Expr::Primary(Primary::Symbol(String::from("x"))),
            ]),
            Expr::Mul(vec![
                Expr::Primary(Primary::Integer(int(5))),
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Symbol(String::from("x")))),
                    Box::new(Expr::Primary(Primary::Integer(int(2)))),
                ),
                Expr::Primary(Primary::Symbol(String::from("y"))),
            ]),
            Expr::Mul(vec![
                Expr::Primary(Primary::Integer(int(-27))),
                Expr::Primary(Primary::Symbol(String::from("x"))),
                Expr::Primary(Primary::Symbol(String::from("y"))),
            ]),
        ]));
    }

    #[test]
    fn combine_like_terms_3() {
        let simplified_expr = simplify_str("x + 2x");
        assert_eq!(simplified_expr, Expr::Mul(vec![
            Expr::Primary(Primary::Integer(int(3))),
            Expr::Primary(Primary::Symbol(String::from("x"))),
        ]));
    }

    #[test]
    fn combine_like_terms_decimals() {
        let simplified_expr = simplify_str("3.75x + 1.4x - -0.13449 + 11.2x / x");
        assert_eq!(simplified_expr, Expr::Add(vec![
            Expr::Mul(vec![
                Expr::Primary(Primary::Float(float_from_str("5.15"))),
                Expr::Primary(Primary::Symbol(String::from("x"))),
            ]),
            Expr::Primary(Primary::Float(float_from_str("11.33449"))),
        ]));
    }

    #[test]
    fn combine_like_terms_mixed_number_types() {
        let simplified_expr = simplify_str("15x/4 + 1.4x - -0.13449 + 56x / (5x)");
        assert_eq!(simplified_expr, Expr::Add(vec![
            make_fraction(
                Expr::Primary(Primary::Integer(int(103))),
                Expr::Primary(Primary::Integer(int(20))),
            ) * Expr::Primary(Primary::Symbol(String::from("x"))),
            make_fraction(
                Expr::Primary(Primary::Integer(int(1133449))),
                Expr::Primary(Primary::Integer(int(100000))),
            ),
        ]));
    }

    #[test]
    fn combine_like_terms_mixed_number_types_2() {
        // has fractions on x terms, decimals on y terms
        // decimals and fractions should be kept separate
        let simplified_expr = simplify_str("11.75y - x/2 * 14 + -6.24y + 37/6x");
        assert_eq!(simplified_expr, Expr::Add(vec![
            make_fraction(
                Expr::Primary(Primary::Integer(int(-5))),
                Expr::Primary(Primary::Integer(int(6))),
            ) * Expr::Primary(Primary::Symbol(String::from("x"))),
            Expr::Mul(vec![
                // coefficients of y-terms were specially chosen to avoid floating-point errors
                // :)
                Expr::Primary(Primary::Float(float_from_str("5.51"))),
                Expr::Primary(Primary::Symbol(String::from("y"))),
            ]),
        ]));
    }

    #[test]
    fn multiply_rules() {
        let simplified_expr = simplify_str("0*(3x+5b^2i)*1*(3a)");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Integer(int(0))));
    }

    #[test]
    fn multiply_rules_2() {
        // also tests add_zero
        let simplified_expr = simplify_str("1*3*1*1*1*(1+(x^2+5x+6)*0)*1*1");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Integer(int(3))));
    }

    #[test]
    fn combine_like_factors() {
        let simplified_expr = simplify_str("a * b * a^3 * c^2 * d^2 * a^2 * b^4 * d^2");
        assert_eq!(simplified_expr, Expr::Mul(vec![
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol("d".to_string()))),
                Box::new(Expr::Primary(Primary::Integer(int(4)))),
            ),
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol("b".to_string()))),
                Box::new(Expr::Primary(Primary::Integer(int(5)))),
            ),
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol("a".to_string()))),
                Box::new(Expr::Primary(Primary::Integer(int(6)))),
            ),
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol("c".to_string()))),
                Box::new(Expr::Primary(Primary::Integer(int(2)))),
            ),
        ]));
    }

    #[test]
    fn combine_like_factors_strict_eq() {
        let simplified_expr = simplify_str("(a + 1 + b) * (b + a) * (b + a + 1) * (a + b)");
        assert_eq!(simplified_expr, Expr::Mul(vec![
            Expr::Exp(
                Box::new(Expr::Add(vec![
                    Expr::Primary(Primary::Symbol("a".to_string())),
                    Expr::Primary(Primary::Symbol("b".to_string())),
                    Expr::Primary(Primary::Integer(int(1))),
                ])),
                Box::new(Expr::Primary(Primary::Integer(int(2)))),
            ),
            Expr::Exp(
                Box::new(Expr::Add(vec![
                    Expr::Primary(Primary::Symbol("a".to_string())),
                    Expr::Primary(Primary::Symbol("b".to_string())),
                ])),
                Box::new(Expr::Primary(Primary::Integer(int(2)))),
            ),
        ]));
    }

    #[test]
    fn simple_combine_like_factors() {
        let simplified_expr = simplify_str("(a+b)/(a+b)");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Integer(int(1))));
    }

    #[test]
    fn combine_like_factors_mul_numbers() {
        let simplified_expr = simplify_str("-1 * -1 * 2 * 2");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Integer(int(4))));
    }

    #[test]
    fn combine_like_factors_decimals() {
        let simplified_expr = simplify_str("4.125 * -1.99 * 2.57");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Float(float_from_str("-21.0964875"))));
    }

    #[test]
    fn complicated_combine_like_factors() {
        let simplified_expr = simplify_str("3p^-5q^9r^7/(12p^-2q*r^2)");
        assert_eq!(simplified_expr, Expr::Mul(vec![
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol("r".to_string()))),
                Box::new(Expr::Primary(Primary::Integer(int(5)))),
            ),
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol("q".to_string()))),
                Box::new(Expr::Primary(Primary::Integer(int(8)))),
            ),
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol("p".to_string()))),
                Box::new(Expr::Primary(Primary::Integer(int(-3)))),
            ),
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Integer(int(4)))),
                Box::new(Expr::Primary(Primary::Integer(int(-1)))),
            ),
        ]));
    }

    #[test]
    fn radicals() {
        // sqrt(2)/2 * sqrt(3)/2 + sqrt(2)/2 * 1/2
        let simplified_expr = simplify_str("2^(1/2)/2*3^(1/2)/2 + 2^(1/2)/2*1/2");

        // sqrt(2)/4 + sqrt(6)/4
        assert_eq!(simplified_expr, Expr::Add(vec![
            // sqrt(2)/4 = 2^(-3/2)
            // the result is a denominator that is not rationalized
            // TODO: rationalize the denominator
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Integer(int(2)))),
                Box::new(make_fraction(
                    Expr::Primary(Primary::Integer(int(-3))),
                    Expr::Primary(Primary::Integer(int(2))),
                )),
            ),
            // sqrt(6)/4
            make_fraction(
                Expr::Exp(
                    Box::new(Expr::Primary(Primary::Integer(int(6)))),
                    Box::new(Expr::Exp(
                        Box::new(Expr::Primary(Primary::Integer(int(2)))),
                        Box::new(Expr::Primary(Primary::Integer(int(-1)))),
                    )),
                ),
                Expr::Primary(Primary::Integer(int(4))),
            ),
        ]));
    }

    #[test]
    fn distribute() {
        // 1/x * (y+2x) = y/x + 2
        let (simplified_expr, steps) = simplify_str_steps("1/x * (y+2x)");
        assert_eq!(simplified_expr, Expr::Add(vec![
            make_fraction(
                Expr::Primary(Primary::Symbol("y".to_string())),
                Expr::Primary(Primary::Symbol("x".to_string())),
            ),
            Expr::Primary(Primary::Integer(int(2))),
        ]));
        assert!(steps.contains(&Step::DistributiveProperty));
    }

    #[test]
    fn distribute_2() {
        // x^2 * (1 + x + y/x^2) = x^2 + x^3 + y
        let (simplified_expr, steps) = simplify_str_steps("x^2 * (1 + x + y/x^2)");
        assert_eq!(simplified_expr, Expr::Add(vec![
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol("x".to_string()))),
                Box::new(Expr::Primary(Primary::Integer(int(2)))),
            ),
            Expr::Exp(
                Box::new(Expr::Primary(Primary::Symbol("x".to_string()))),
                Box::new(Expr::Primary(Primary::Integer(int(3)))),
            ),
            Expr::Primary(Primary::Symbol("y".to_string())),
        ]));
        assert!(steps.contains(&Step::DistributiveProperty));
    }

    #[test]
    fn power_rules() {
        let simplified_expr = simplify_str("(1^0)^(3x+5b^2i)^1^(3a)");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Integer(int(1))));
    }

    #[test]
    fn power_rules_2() {
        let simplified_expr = simplify_str("(0^1)^0");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Integer(int(1))));
    }

    #[test]
    fn power_rules_3a() {
        let simplified_expr = simplify_str("x^3 * x^-2");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Symbol("x".to_string())));
    }

    #[test]
    fn power_rules_3b() {
        let simplified_expr = simplify_str("x^3 / x^2");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Symbol("x".to_string())));
    }

    #[test]
    fn power_rule_steps() {
        let (simplified_expr, steps) = simplify_str_steps("(1^0)^(3x+5b^2i)^1^(3a)");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Integer(int(1))));
        assert_eq!(steps, vec![
            Step::PowerPower,
            Step::PowerOneLeft,
        ]);
    }

    #[test]
    fn imaginary_num() {
        let simplified_expr = simplify_str("i^372 + i^145 - i^215 - i^807");
        assert_eq!(simplified_expr, Expr::Add(vec![
            Expr::Mul(vec![
                Expr::Primary(Primary::Integer(int(3))),
                Expr::Primary(Primary::Symbol("i".to_string())),
            ]),
            Expr::Primary(Primary::Integer(int(1))),
        ]));
    }

    #[test]
    fn trigonometric_sine() {
        let simplified_expr = simplify_str("sin(pi/6 + pi/4 + pi/2 + pi/12)");
        assert_eq!(simplified_expr, Expr::Primary(Primary::Integer(int(0))));
    }

    #[test]
    fn trigonometric_sine_2() {
        let simplified_expr = simplify_str("sin(47pi/4 + 31pi/2)");

        // -sqrt(2)/2 = -2^(1/2)/2 = -2^(-1/2)
        assert_eq!(simplified_expr, -Expr::Exp(
            Box::new(Expr::Primary(Primary::Integer(int(2)))),
            Box::new(make_fraction(
                Expr::Primary(Primary::Integer(int(-1))),
                Expr::Primary(Primary::Integer(int(2))),
            )),
        ));
    }
}
