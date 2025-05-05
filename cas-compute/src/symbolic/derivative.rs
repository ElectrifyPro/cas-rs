
use std::ops::Mul;

use cas_parser::parser::token::Symbol;
use rug::{Float, Integer};

use crate::primitive::{float, int};
use crate::symbolic::derivative;

use super::expr::Primary;
use super::Expr;

// Something that is not trivially zero - intended to clean up ASTs and not mathetmatically
// rigorous
fn non_zero(e: &Expr) -> bool {
    match e {
        Expr::Primary(Primary::Float(f)) => {
            !f.is_zero()
        },
        Expr::Primary(Primary::Integer(i)) => {
            !i.is_zero()
        },
        Expr::Add(sum) => {
            sum.iter().any(non_zero)
        },
        Expr::Mul(mul) => {
            mul.iter().all(non_zero)
        }
        _ => true
    }
}

// Something that is not trivially one - intended to clean up ASTs and not mathetmatically
// rigorous
fn non_unity(e: &Expr) -> bool {
    match e {
        Expr::Primary(Primary::Float(f)) => !f.eq(&float(1)),
        Expr::Primary(Primary::Integer(i)) => !i.eq(&int(1)),
        Expr::Mul(exprs) => {
            exprs.iter().any(non_unity)
        },
        Expr::Exp(expr, expr1) => {
            non_zero(expr1) && non_unity(expr)
        },
        _ => true
    }
}

// produces the derivative of the given expression
pub fn derivative(f: Expr, with: &str) -> Expr {
    if !non_zero(&f) {
        return Expr::Primary(Primary::Integer(int(0)))
    }
    let expr = match f {
        Expr::Primary(Primary::Float(_)) | Expr::Primary(Primary::Integer(_)) => {
            Expr::Primary(Primary::Integer(int(0)))
        },
        Expr::Primary(Primary::Symbol(sym)) => {
            if sym == with {
                Expr::Primary(Primary::Integer(int(1)))
            } else {
                Expr::Primary(Primary::Integer(int(0)))
            }
        }
        Expr::Primary(Primary::Call(func, args)) => {
            // TODO: context and chain rule?
            let mut mult_group = Vec::with_capacity(2);
            match func.as_str() {
                "sqrt" => {
                    assert_eq!(args.len(), 1, "sqrt has only one argument");
                    return derivative(Expr::Exp(Box::new(args[0].clone()), Box::new(Expr::Primary(Primary::Float(float(0.5))))), with);
                },
                "sin" => {
                    assert_eq!(args.len(), 1, "sin has exactly one argument");
                    mult_group.push(derivative(args[0].clone(), with));
                    mult_group.push(Expr::Primary(Primary::Call("sin".to_string(), vec![args[0].clone()])));
                },
                "cos" => {
                    assert_eq!(args.len(), 1, "cos has exactly one argument");

                    let mut mult_group = Vec::with_capacity(3);
                    mult_group.push(derivative(args[0].clone(), with));
                    mult_group.push(Expr::Primary(Primary::Integer(int(-1))));
                    mult_group.push(Expr::Primary(Primary::Call("sin".to_string(), vec![args[0].clone()])));
                },
                _ => todo!("cannot differentiate this function yet")
            };
            Expr::Mul(mult_group)
        }
        Expr::Add(add) => {
            Expr::Add(add.into_iter().map(|add_elem| derivative(add_elem, with)).filter(non_zero).collect())
        },
        Expr::Mul(exprs) => {
            let mut outer_sum = Vec::new();

            // Produces a derivative according the product rule:
            // f'*g*h + f*g'*h + f*g*h'
            for derivative_index in 0..exprs.len() {
                let mut inner_mult = Vec::new();
                for term_index in 0..exprs.len() {
                    let term = if derivative_index == term_index {
                        derivative(exprs[derivative_index].clone(), with)
                    } else {
                        exprs[term_index].clone()
                    };

                    // anything times zero is zero
                    if non_zero(&term) {
                        if non_unity(&term) {
                            inner_mult.push(term);
                        }
                    } else { 
                        inner_mult = vec![Expr::Primary(Primary::Integer(int(0)))];
                        break;
                    }
                }
                
                let expr = Expr::Mul(inner_mult);
                if non_zero(&expr) {
                    outer_sum.push(expr);
                }
            }

            Expr::Add(outer_sum)
        },
        Expr::Exp(expr, expr1) => {
            match &*expr1 {
                Expr::Primary(Primary::Integer(i)) => {
                    let mut mult_group = vec![derivative((*expr).clone(), with)];

                    mult_group.push(Expr::Primary(Primary::Integer(int(i))));
                    mult_group.push(Expr::Exp(expr, Expr::Primary(Primary::Integer(i - Integer::from(1))).into()));

                    // Apply the power rule (integers)
                    Expr::Mul(mult_group)
                },
                Expr::Primary(Primary::Float(i)) => {
                    let mut mult_group = vec![derivative((*expr).clone(), with)];
                    mult_group.push(Expr::Primary(Primary::Float(float(i))));
                    mult_group.push(Expr::Exp(expr, Expr::Primary(Primary::Float(i - float(1))).into())); 
                    Expr::Mul(mult_group)
                },
                _ => todo!()
            }
        }
    };

    //TODO: call simplify between operations?
    if non_zero(&expr) {
        expr
    } else {
        Expr::Primary(Primary::Integer(int(0)))
    }
}
