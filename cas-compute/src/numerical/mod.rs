//! Helpers for numerical evaluation of CalcScript expressions.
//!
//! This module provides helper tools used to evaluate CalcScript expressions. It does not actually
//! evalute the expressions, but
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
pub mod fmt;
pub mod func;
pub mod trig_mode;
pub mod value;
