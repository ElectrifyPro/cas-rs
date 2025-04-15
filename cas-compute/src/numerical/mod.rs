//! Numerical evaluation of CalcScript expressions, like a simple handheld calculator.
//!
//! # Usage
//!
//! In any case, use `cas-parser` to parse CalcScript expressions before passing them to the
//! evaluator. Here is an example of how to parse and evaluate CalcScript expressions:
//!
//! ```rust
//! use cas_compute::numerical::eval::Eval;
//! use cas_parser::parser::{ast::Expr, Parser};
//!
//! let mut parser = Parser::new("5sin(pi/2) * 6!");
//! let ast_expr = parser.try_parse_full::<Expr>().unwrap();
//! let result = ast_expr.eval_default().unwrap();
//!
//! // 5sin(pi/2) * 6!
//! // = 5 * 1 * 720 = 3600
//! assert_eq!(result, 3600.into());
//! ```
//!
//! The [`Eval`](eval::Eval) trait handles the evaluation of expressions. It optionally takes a
//! context [`Ctxt`](ctxt::Ctxt) that specifies the varables and functions that are in scope when
//! evaluating the expression. If you need to manipulate or inspect the state of the context
//! before, during, or after evaluation, use [`Eval::eval`](eval::Eval::eval). Otherwise, you can
//! use [`Eval::eval_default`](eval::Eval::eval_default) to evaluate the expression with a default
//! context, which includes some useful constants in addition to all builtin functions.
//!
//! ```rust
//! use cas_compute::numerical::{ctxt::Ctxt, eval::Eval};
//! use cas_parser::parser::{ast::Expr, Parser};
//!
//! let mut parser = Parser::new("x * f(y)");
//! let ast_expr = parser.try_parse_full::<Expr>().unwrap();
//!
//! let mut ctxt = Ctxt::default();
//!
//! // define a function `f(y) = y^2`
//! let (f_target, f_value) = {
//!     use cas_parser::parser::ast::{Assign, AssignTarget};
//!     let mut parser = Parser::new("f(z) = z^2");
//!     let ast_expr = parser.try_parse_full::<Assign>().unwrap();
//!     let AssignTarget::Func(f_target) = ast_expr.target else {
//!         panic!("Expected function assignment");
//!     };
//!     (f_target, ast_expr.value)
//! };
//! ctxt.add_func(f_target, *f_value, false);
//!
//! let cases = [
//!     // (x, y, expected)
//!     (2, 3, 18),
//!     (4, 5, 100),
//!     (6, 7, 294),
//!     (35, 45, 70875),
//! ];
//!
//! for (x, y, expected) in cases.into_iter() {
//!     // set the values of `x` and `y` in the context
//!     ctxt.add_var("x", x.into());
//!     ctxt.add_var("y", y.into());
//!     let result = ast_expr.eval(&mut ctxt).unwrap();
//!     assert_eq!(result, expected.into());
//! }
//! ```

#![cfg(feature = "numerical")]

pub mod builtin;
pub mod ctxt;
pub mod error;
pub mod eval;
pub mod fmt;
pub mod value;

#[cfg(test)]
mod tests {
    use cas_parser::parser::Parser;
    use crate::primitive::float;
    use super::{eval::eval_stmts, value::Value};

    /// Evaluates the given statements and returns the result.
    fn eval(source: &str) -> Value {
        let ast = Parser::new(source).try_parse_full_many().unwrap();
        eval_stmts(&ast, &mut Default::default()).unwrap()
    }

    #[test]
    fn bad_lcm() {
        let source = include_str!("../../../examples/bad_lcm.calc");
        assert_eq!(eval(source), 1517.into());
    }

    #[test]
    fn factorial() {
        let source = include_str!("../../../examples/factorial.calc");
        assert_eq!(eval(source), true.into());
    }

    #[test]
    fn function_scope() {
        let source = include_str!("../../../examples/function_scope.calc");
        assert_eq!(eval(source), 14.into());
    }

    #[test]
    fn if_branching() {
        let source = include_str!("../../../examples/if_branching.calc");
        assert_eq!(eval(source).coerce_float(), float(5).log2().into());
    }

    #[test]
    fn manual_abs() {
        let source = include_str!("../../../examples/manual_abs.calc");
        assert_eq!(eval(source), 4.into());
    }
}
