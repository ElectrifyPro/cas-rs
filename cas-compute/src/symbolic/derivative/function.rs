//! Symbolic derivatives of functions supported in src/funcs/mod.rs

use crate::primitive::float;
use crate::primitive::int;
use crate::symbolic::expr::Primary;
use crate::symbolic::Expr;

use super::MultBuilder;
use super::derivative;
use super::SymbolicDerivativeError;

/// Computes the derivative of a supported function and performs the chain rule 
pub(super) fn function_derivative(func: &str, args: &[Expr], with: &str) -> Result<Expr, SymbolicDerivativeError> {
    let mut mult_group = MultBuilder::default();

    match func {
        "sqrt" => {
            assert_eq!(args.len(), 1, "sqrt has only one argument");
            return derivative(&Expr::Exp(Box::new(args[0].clone()), Box::new(Expr::Primary(Primary::Float(float(0.5))))), with);
        },
        "sin" => {
            assert_eq!(args.len(), 1, "sin has exactly one argument");
            mult_group.mult(derivative(&args[0], with)?);
            mult_group.mult(Expr::Primary(Primary::Call("sin".to_string(), vec![args[0].clone()])));
        },
        "cos" => {
            assert_eq!(args.len(), 1, "cos has exactly one argument");
            mult_group.mult(derivative(&args[0], with)?);
            mult_group.mult(Expr::Primary(Primary::Integer(int(-1))));
            mult_group.mult(Expr::Primary(Primary::Call("sin".to_string(), vec![args[0].clone()])));
        },
        _ => {
            return Err(SymbolicDerivativeError::Unsupported);
        }
    };

    Ok(mult_group.into())
}
