use rug::Integer;

use crate::primitive::{float, int};

use super::expr::Primary;
use super::Expr;

mod function;

/// Returns `true` if the given [`Expr`] is "clearly" nonzero. This is intended to clean up ASTs
/// and is not mathetmatically rigorous.
fn is_trivially_zero(e: &Expr) -> bool {
    match e {
        Expr::Primary(Primary::Float(f)) => f.is_zero(),
        Expr::Primary(Primary::Integer(i)) => i.is_zero(),
        Expr::Add(sum) => sum.iter().all(is_trivially_zero),
        Expr::Mul(mul) => mul.iter().any(is_trivially_zero),
        Expr::Exp(base, exponent) => is_trivially_zero(&base) && !is_trivially_zero(&exponent),
        _ => false,
    }
}

/// Returns `true` if the given [`Expr`] is "clearly" not one. This is intended to clean up ASTs
/// and is not mathetmatically rigorous.
fn is_trivially_unity(e: &Expr) -> bool {
    match e {
        Expr::Primary(Primary::Float(f)) => *f == 1,
        Expr::Primary(Primary::Integer(i)) => *i == 1,
        Expr::Mul(exprs) => exprs.iter().all(is_trivially_unity),
        Expr::Exp(expr, expr1) => is_trivially_zero(expr1) || is_trivially_unity(expr),
        Expr::Add(expr) => expr.len() == 1 && is_trivially_unity(&expr[0]),
        _ => false
    }
}

/// Helper struct to build a product of expressions while applying basic simplification rules. If
/// any of the expressions are zero, the product is reduced to zero.
#[derive(Default)]
struct MultBuilder(Vec<Expr>);

impl From<MultBuilder> for Expr {
    fn from(value: MultBuilder) -> Self {
        Expr::Mul(value.0).downgrade()
    }
}

impl MultBuilder {
    fn mult(&mut self, e: Expr) {
        if is_trivially_zero(&e) || self.0.get(0).is_some_and(|e| is_trivially_zero(e)) {
            self.0 = vec![Expr::Primary(Primary::Integer(int(0)))];
            return;
        }

        if !is_trivially_unity(&e) {
            self.0.push(e)
        }
    }
}

/// Helper struct to build a summation of expressions while applying basic simplification rules.
/// only non-zero expressions are added to the sum.
#[derive(Default)]
struct SumBuilder(Vec<Expr>);

impl From<SumBuilder> for Expr {
    fn from(value: SumBuilder) -> Self {
        Expr::Add(value.0).downgrade()
    }
}

impl SumBuilder {
    fn add(&mut self, e: Expr) {
        if !is_trivially_zero(&e) {
            self.0.push(e)
        }
    }
}

/// `(f + g)' = f' + g'`
fn sum_rule(exprs: &[Expr], var: &str) -> Result<Expr, SymbolicDerivativeError> {
    let mut sum = SumBuilder::default();
    for elem in exprs {
        sum.add(derivative(elem, var)?);
    }
    Ok(sum.into())
}

/// `(f * g * h)' = f' * g * h + f * g' * h + f * g * h'`
fn product_rule(product: &[Expr], with: &str) -> Result<Expr, SymbolicDerivativeError> {
    let mut outer_sum = SumBuilder::default();

    // Produces a derivative according the product rule:
    // f'*g*h + f*g'*h + f*g*h'
    for derivative_index in 0..product.len() {
        let mut inner_mult = MultBuilder::default();
        for term_index in 0..product.len() {
            let term = if derivative_index == term_index {
                derivative(&product[derivative_index], with)?
            } else {
                product[term_index].clone()
            };

            inner_mult.mult(term);
        }
        
        outer_sum.add(inner_mult.into());
    }

    Ok(outer_sum.into())
}

#[derive(Debug)]
pub enum SymbolicDerivativeError {
    /// The function may be differentiable, but we do not support symbolically computing it yet
    Unsupported,

    /// The provided sub-expression is not differentiable
    Undifferentiable(Expr)
}

/// Computes the derivative of the given expression. Returns [`Err`] if the derivative could not
/// be symbolically computed.
pub fn derivative(f: &Expr, with: &str) -> Result<Expr, SymbolicDerivativeError> {
    if is_trivially_zero(&f) {
        return Ok(Expr::Primary(Primary::Integer(int(0))))
    }
    let expr = match f {
        Expr::Primary(Primary::Float(_)) | Expr::Primary(Primary::Integer(_)) => {
            Ok(Expr::Primary(Primary::Integer(int(0))))
        },
        Expr::Primary(Primary::Symbol(sym)) => {
            if sym == with {
                Ok(Expr::Primary(Primary::Integer(int(1))))
            } else {
                Ok(Expr::Primary(Primary::Integer(int(0))))
            }
        }
        Expr::Primary(Primary::Call(func, args)) => {
            function::function_derivative(func, args, with)
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
                    mult_group.mult(derivative(&expr, with)?);
                    mult_group.mult(Expr::Primary(Primary::Integer(i.clone())));
                    mult_group.mult(Expr::Exp(expr.clone(), Expr::Primary(Primary::Integer(i - Integer::from(1))).into()));

                    // Apply the power rule (integers)
                    Ok(mult_group.into())
                },
                Expr::Primary(Primary::Float(i)) => {
                    let mut mult_group = MultBuilder::default();
                    mult_group.mult(derivative(&*expr, with)?);
                    mult_group.mult(Expr::Primary(Primary::Float(float(i.clone()))));
                    mult_group.mult(Expr::Exp(expr.clone(), Expr::Primary(Primary::Float(i - float(1))).into())); 
                    Ok(mult_group.into())
                },
                // TODO(Dhruv): d/dx a^x = a^x ln(a)
                _ => {
                    Err(SymbolicDerivativeError::Unsupported)
                }
            }
        }
    };

    //TODO: call simplify between operations?
    if expr.as_ref().is_ok_and(|e| !is_trivially_zero(e)) {
        expr
    } else {
        Ok(Expr::Primary(Primary::Integer(int(0))))
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
        let symbolic = derivative(&expr, "x").expect(&format!("Derivative for \"{function}\" was unable to be computed symbolically")).into();

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
