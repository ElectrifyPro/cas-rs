mod assign;
mod binary;
mod block;
mod break_expr;
mod call;
mod continue_expr;
mod error;
mod expr;
mod if_expr;
mod index;
mod literal;
mod loops;
mod primary;
mod return_expr;
mod stmt;
mod unary;

use super::{ctxt::Ctxt, error::Error, value::Value};

pub use block::eval_stmts;

/// Any type that can be evaluated to produce a value.
pub trait Eval {
    /// Evaluate the expression to produce a value, using the given context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error>;

    /// Evaluate the expression to produce a value, using the default context. The expression should
    /// return [`None`] if it cannot be evaluated.
    fn eval_default(&self) -> Result<Value, Error> {
        self.eval(&mut Default::default())
    }
}

/// Eval tests depend on the parser, so ensure that parser tests pass before running these.
#[cfg(test)]
mod tests {
    use crate::consts;
    use crate::funcs::miscellaneous::{Abs, Factorial};
    use crate::numerical::builtin::Builtin;
    use crate::primitive::float;
    use rug::ops::Pow;
    use super::*;

    use cas_parser::parser::{ast::expr::Expr, Parser};

    #[test]
    fn binary_expr() {
        let mut parser = Parser::new("1 + 2");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), 3.into());
    }

    #[test]
    fn binary_expr_2() {
        let mut parser = Parser::new("1 + 2 * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), 7.into());
    }

    #[test]
    fn binary_and_unary() {
        let mut parser = Parser::new("3 * -5 / 5! + 6");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), 5.875.into());
    }

    #[test]
    fn parenthesized() {
        let mut parser = Parser::new("((1 + 9) / 5) * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), 6.into());
    }

    #[test]
    fn degree_to_radian() {
        let mut parser = Parser::new("90 * 2 * pi / 360");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        // assert approximate equality for floating point numbers
        let val1 = expr.eval_default().unwrap();
        let val2 = (&*consts::PI / float(2)).into();
        assert!(val1.approx_eq(&val2));
    }

    #[test]
    fn precision() {
        let mut parser = Parser::new("e^2 - tau");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval_default().unwrap(), Value::Float(consts::E.clone().pow(2) - &*consts::TAU));
    }

    #[test]
    fn precision_2() {
        let mut parser = Parser::new("pi^2 * 17! / -4.9 + e");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        let val1 = expr.eval_default().unwrap();
        let fac_17 = if let Value::Integer(fac_17) = Factorial::eval_static(float(17)) {
            fac_17
        } else {
            unreachable!("factorial of 17 is an integer")
        };
        let val2 = Value::Float(consts::PI.clone().pow(2) * fac_17 / -float(4.9) + &*consts::E);
        assert!(val1.approx_eq(&val2));
    }

    #[test]
    fn func_call() {
        let mut ctxt = Ctxt::default();

        // assign function
        let mut parser = Parser::new("f(x) = x^2 + 5x + 6");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(&mut ctxt).unwrap(), Value::Unit);

        // call function
        let mut parser = Parser::new("f(7)");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(&mut ctxt).unwrap(), 90.into());
    }

    #[test]
    fn complicated_func_call() {
        let mut ctxt = Ctxt::default();

        // assign function
        let mut parser = Parser::new("f(n = 3, k = 6) = n * k");
        let expr = parser.try_parse_full::<Expr>().unwrap();
        assert_eq!(expr.eval(&mut ctxt).unwrap(), Value::Unit);

        // call function
        let tries = [
            (None, None, 18),
            // (None, Some(4.0), 12.0), // TODO: there is currently no way to pass only the second argument
            (Some(9), None, 54),
            (Some(8), Some(14), 112),
        ];
        for (n, k, expected_result) in tries {
            let source = format!(
                "f({}{})",
                n.map_or("".to_string(), |n| n.to_string()),
                k.map_or("".to_string(), |k| format!(", {}", k)),
            );
            let mut parser = Parser::new(&source);
            let expr = parser.try_parse_full::<Expr>().unwrap();
            assert_eq!(
                expr.eval(&mut ctxt).unwrap(),
                expected_result.into(),
                "source: {}",
                source,
            );
        }
    }

    #[test]
    fn builtin_func_arg_check() {
        assert_eq!(Abs.eval(Default::default(), &mut [Value::from(4.0)].into_iter()).unwrap().coerce_float(), 4.0.into());
        assert!(Abs.eval(Default::default(), &mut [Value::Unit].into_iter()).is_err());
    }
}
