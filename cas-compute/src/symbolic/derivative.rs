
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

#[derive(Default)]
struct MultBuilder(Vec<Expr>);

impl From<MultBuilder> for Expr {
    fn from(value: MultBuilder) -> Self {
        if value.0.len() > 1 {
            Expr::Mul(value.0)
        } else if value.0.len() == 1 {
            value.0[0].clone()
        } else {
            Expr::Primary(Primary::Integer(int(1)))
        }
    }
}

impl MultBuilder {
    fn mult(&mut self, e: Expr) {
        if !non_zero(&e) || self.0.get(0).is_some_and(|e| !non_zero(e)) {
            self.0 = vec![Expr::Primary(Primary::Integer(int(0)))];
            return;
        }

        if non_unity(&e) {
            self.0.push(e)
        }
    }
}

#[derive(Default)]
struct SumBuilder(Vec<Expr>);

impl From<_SumBuilder> for Expr {
    fn from(value: SumBuilder) -> Self {
        if value.0.len() > 1 {
            Expr::Add(value.0)
        } else if value.0.len() == 1 {
            value.0[0].clone()
        } else {
            Expr::Primary(Primary::Integer(int(0)))
        }
    }
}

impl SumBuilder {
    fn add(&mut self, e: Expr) {
        if non_zero(&e) {
            self.0.push(e)
        }
    }
}

fn sum_rule(exprs: Vec<Expr>, var: &str) -> Expr {
    let mut sum = SumBuilder::default();
    for elem in exprs {
        sum.add(derivative(elem, var));
    }
    sum.into()
}

fn product_rule(product: Vec<Expr>, with: &str) -> Expr {
    let mut outer_sum = SumBuilder::default();

    // Produces a derivative according the product rule:
    // f'*g*h + f*g'*h + f*g*h'
    for derivative_index in 0..product.len() {
        let mut inner_mult = MultBuilder::default();
        for term_index in 0..product.len() {
            let term = if derivative_index == term_index {
                derivative(product[derivative_index].clone(), with)
            } else {
                product[term_index].clone()
            };

            inner_mult.mult(term);
        }
        
        outer_sum.add(inner_mult.into());
    }

    outer_sum.into()
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
            let mut mult_group = MultBuilder::default();
            match func.as_str() {
                "sqrt" => {
                    assert_eq!(args.len(), 1, "sqrt has only one argument");
                    return derivative(Expr::Exp(Box::new(args[0].clone()), Box::new(Expr::Primary(Primary::Float(float(0.5))))), with);
                },
                "sin" => {
                    assert_eq!(args.len(), 1, "sin has exactly one argument");
                    mult_group.mult(derivative(args[0].clone(), with));
                    mult_group.mult(Expr::Primary(Primary::Call("sin".to_string(), vec![args[0].clone()])));
                },
                "cos" => {
                    assert_eq!(args.len(), 1, "cos has exactly one argument");
                    mult_group.mult(derivative(args[0].clone(), with));
                    mult_group.mult(Expr::Primary(Primary::Integer(int(-1))));
                    mult_group.mult(Expr::Primary(Primary::Call("sin".to_string(), vec![args[0].clone()])));
                },
                _ => todo!("cannot differentiate this function yet")
            };
            mult_group.into()
        }
        Expr::Add(exprs) => sum_rule(exprs, with),
        Expr::Mul(exprs) => product_rule(exprs, with),
        Expr::Exp(expr, expr1) => {
            match &*expr1 {
                Expr::Primary(Primary::Integer(i)) => {
                    let mut mult_group = MultBuilder::default();
                    mult_group.mult(derivative((*expr).clone(), with));
                    mult_group.mult(Expr::Primary(Primary::Integer(int(i))));
                    mult_group.mult(Expr::Exp(expr, Expr::Primary(Primary::Integer(i - Integer::from(1))).into()));

                    // Apply the power rule (integers)
                    mult_group.into()
                },
                Expr::Primary(Primary::Float(i)) => {
                    let mut mult_group = MultBuilder::default();
                    mult_group.mult(derivative((*expr).clone(), with));
                    mult_group.mult(Expr::Primary(Primary::Float(float(i))));
                    mult_group.mult(Expr::Exp(expr, Expr::Primary(Primary::Float(i - float(1))).into())); 
                    mult_group.into()
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
