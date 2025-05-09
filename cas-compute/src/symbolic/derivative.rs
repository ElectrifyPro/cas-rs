
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

impl From<SumBuilder> for Expr {
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

fn sum_rule(exprs: &[Expr], var: &str) -> Expr {
    let mut sum = SumBuilder::default();
    for elem in exprs {
        sum.add(derivative(elem, var));
    }
    sum.into()
}

fn product_rule(product: &[Expr], with: &str) -> Expr {
    let mut outer_sum = SumBuilder::default();

    // Produces a derivative according the product rule:
    // f'*g*h + f*g'*h + f*g*h'
    for derivative_index in 0..product.len() {
        let mut inner_mult = MultBuilder::default();
        for term_index in 0..product.len() {
            let term = if derivative_index == term_index {
                derivative(&product[derivative_index], with)
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
pub fn derivative(f: &Expr, with: &str) -> Expr {
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
                    return derivative(&Expr::Exp(Box::new(args[0].clone()), Box::new(Expr::Primary(Primary::Float(float(0.5))))), with);
                },
                "sin" => {
                    assert_eq!(args.len(), 1, "sin has exactly one argument");
                    mult_group.mult(derivative(&args[0], with));
                    mult_group.mult(Expr::Primary(Primary::Call("sin".to_string(), vec![args[0].clone()])));
                },
                "cos" => {
                    assert_eq!(args.len(), 1, "cos has exactly one argument");
                    mult_group.mult(derivative(&args[0], with));
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
            //TODO: make more expansive. variables are allowed in the exponent for the power rule
            //if they are not the variable we're integrating over. otherwise we need to apply a
            //different formula
            match &**expr1 {
                Expr::Primary(Primary::Integer(i)) => {
                    let mut mult_group = MultBuilder::default();
                    mult_group.mult(derivative(&expr, with));
                    mult_group.mult(Expr::Primary(Primary::Integer(i.clone())));
                    mult_group.mult(Expr::Exp(expr.clone(), Expr::Primary(Primary::Integer(i - Integer::from(1))).into()));

                    // Apply the power rule (integers)
                    mult_group.into()
                },
                Expr::Primary(Primary::Float(i)) => {
                    let mut mult_group = MultBuilder::default();
                    mult_group.mult(derivative(&*expr, with));
                    mult_group.mult(Expr::Primary(Primary::Float(float(i.clone()))));
                    mult_group.mult(Expr::Exp(expr.clone(), Expr::Primary(Primary::Float(i - float(1))).into())); 
                    mult_group.into()
                },
                // d/dx a^x = a^x ln(a)
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

#[cfg(test)]
pub mod tests {
    use crate::numerical::{ctxt::Ctxt, eval::Eval, value::Value};
    use cas_parser::parser::{ast::Expr as AstExpr, Parser};

    use super::{derivative, Expr};

    /// Boilerplate helper function for evaluating an expression and substituting in "x"
    fn eval_x(e: &AstExpr, x: f64) -> f64 {
        let mut context = Ctxt::new();
        context.add_var("x", x.into());

        if let Value::Float(f) = e.eval(&mut context).unwrap().coerce_float() {
            return f.to_f64()
        } else {
            unreachable!("Evaluated value should yield a float")
        }
    }

    // Performs finite difference to approximate the derivative of the provided expression 
    fn finite_difference(e: &AstExpr, x: f64) -> f64 {
        const DX: f64 = 0.00001;
        let e = e.into();
        (eval_x(e, x + DX) - eval_x(e, x))/DX
    }

    fn test_for_function(function: &'static str, points: impl IntoIterator<Item = f64>) {
        const TOL: f64 = 0.0001;

        let mut parser = Parser::new(function);
        let ast_expr = parser.try_parse_full::<AstExpr>().unwrap();

        let expr = Expr::try_from(ast_expr.clone()).expect("Parsed expression must be representable as a symbolic expression").into();
        let symbolic = derivative(&expr, "x").into();

        for point in points.into_iter() {
            let symbolically_computed = eval_x(&symbolic, point);
            let numerically_computed = finite_difference(&ast_expr, point);

            assert!((symbolically_computed - numerically_computed).abs() < TOL, "For \"{function}\" at x={point}, symbolically computed derivative was {symbolically_computed} but numerically computed derivative was {numerically_computed}, which was out of tolerance {TOL}")
        }
    }

    //TODO(Dhruv): more similar tests
    #[test]
    fn power_rule() {
        test_for_function("x^2 + x + 1", [0., 1., 2., 5., 8.]);
    }

    //TODO(Dhruv): test for correct treatment of partial derivatives too - maybe we pass in the
    //contxet

}
