//! Algebraic manipulation of expressions.
//!
//! # Expression representation
//!
//! Algebraic expressions in this module are represented as a tree of [`expr::Expr`] nodes, which
//! should be distinguished from the [`cas_parser::parser::ast::Expr`] nodes produced by
//! [`cas_parser`]. The main difference is that [`expr::Expr`] nodes **flatten** out the tree
//! structure.
//!
//! For example, the expression `x + (y + z)` would be represented internally as a single
//! [`expr::Expr::Add`] node with _three_ children, `x`, `y`, and `z`, where as the
//! [`cas_parser::parser::ast::Expr`] node would have two children, `x` and `(y + z)`.
//!
//! This is done to make it easier to perform algebraic manipulations on the expression. A common
//! step in simplifying an expression is to combine "like terms", that is, to combine terms that
//! share the same factors (e.g. `x + x = 2x`). This is much easier to do when the terms in
//! question are all at the same level in the tree.
//!
//! If you have a [`cas_parser::parser::ast::Expr`], you can convert it to an [`expr::Expr`] using
//! the [`From`] trait. It should be noted that conversion to [`expr::Expr`] is lossy, as
//! [`expr::Expr`] does not store span information and is free to rearrange the terms and / or
//! factors during conversion, however the resulting expression will be semantically equivalent to
//! the original.
//!
//! ```
//! use cas_compute::symbolic::expr::{Expr, Primary};
//! use cas_parser::parser::{ast::Expr as AstExpr, Parser};
//!
//! let mut parser = Parser::new("x + (y + z)");
//! let ast_expr = parser.try_parse_full::<AstExpr>().unwrap();
//!
//! let expr: Expr = ast_expr.into();
//! assert_eq!(expr, Expr::Add(vec![
//!     Expr::Primary(Primary::Symbol("x".to_string())),
//!     Expr::Primary(Primary::Symbol("y".to_string())),
//!     Expr::Primary(Primary::Symbol("z".to_string())),
//! ]));
//! ```
//!
//! # Simplification
//!
//! A primary use case for algebraic manipulation is to reduce expressions to some canonical form.
//! This is done with the [`simplify()`] function, which accepts an expression and returns a
//! "simplified" version of it.
//!
//! The definition of "simplified" is, of course, somewhat subjective. We define an expression to
//! be simplified if it has the lowest _complexity_ in the set of all expressions **semantically
//! equivalent** to it, where complexity is roughly defined as the number of nodes in the
//! expression tree. For example, `x + x` is not simplified, because it can be reduced to one term
//! `2x`, which has lower complexity.
//!
//! Simplification is done by applying a set of simplification rules to the expression in multiple
//! passes. Each rule is simply a function that accepts an expression and returns [`Option<Expr>`];
//! if the rule is applicable to the expression, the rule is applied and the result is returned.
//!
//! The current set of rules is defined in [`simplify::rules`], and covers things like combining
//! like terms / factors, distributing multiplication over addition, basic power rules, and more.
//!
//! ```
//! use cas_compute::numerical::consts::int;
//! use cas_compute::symbolic::{expr::{Expr, Primary}, simplify};
//! use cas_parser::parser::{ast::Expr as AstExpr, Parser};
//!
//! let mut parser = Parser::new("x + x + x");
//! let ast_expr = parser.try_parse_full::<AstExpr>().unwrap();
//! let simplified = simplify(&ast_expr.into());
//!
//! // `x + x + x = 3x`
//! assert_eq!(simplified, Expr::Mul(vec![
//!     Expr::Primary(Primary::Integer(int(3))),
//!     Expr::Primary(Primary::Symbol("x".to_string())),
//! ]));
//! ```
//!
//! For more information, see the [`simplify`] module.

pub mod expr;
pub mod simplify;
pub mod step_collector;

pub use expr::Expr;
pub use simplify::{simplify, simplify_with, simplify_with_steps};
pub use step_collector::StepCollector;
