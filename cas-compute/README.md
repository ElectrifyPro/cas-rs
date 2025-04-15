Numerical and symbolic evaluation for CalcScript.

This crate provides an evaluator for CalcScript. It is split into two main
submodules, `numerical` and `symbolic`, which handle numerical and symbolic
evaluation respectively. Currently, symbolic manipulation is limited to
simplification, but this will be extended in the future.

# Usage

## Numerical evaluation

The `numerical` module implements an evaluator for CalcScript expressions.
Expressions are evaluated using the [`Eval`](numerical::eval::Eval) trait,
optionally with a custom [`Ctxt`](numerical::ctxt::Ctxt) context, used to
specify the variables and functions in scope, and the trigonometric mode to use
for trigonometric functions.

The default [`Ctxt::default()`](numerical::ctxt::Ctxt::default) includes some
useful constants in the [`consts`] module and every builtin function in the
[`funcs`] module.

See its [module-level documentation](numerical) for more information.

```rust
use cas_compute::numerical::eval::Eval;
use cas_parser::parser::{ast::Expr, Parser};

let mut parser = Parser::new("5sin(pi/2) * 6!");
let ast_expr = parser.try_parse_full::<Expr>().unwrap();
let result = ast_expr.eval_default().unwrap();

// 5sin(pi/2) * 6!
// = 5 * 1 * 720 = 3600
assert_eq!(result, 3600.into());
```

## Simplification

The `symbolic` module currently provides [`simplify`](symbolic::simplify()) to
simplify expressions symbolically.

See its [module-level documentation](symbolic) for more information.

```rust
use cas_compute::primitive::int;
use cas_compute::symbolic::{expr::{Expr, Primary}, simplify};
use cas_parser::parser::{ast::Expr as AstExpr, Parser};

let mut parser = Parser::new("x + x + x");
let ast_expr = parser.try_parse_full::<AstExpr>().unwrap();

let simplified = simplify(&ast_expr.into());

// `x + x + x = 3x`
assert_eq!(simplified, Expr::Mul(vec![
    Expr::Primary(Primary::Integer(int(3))),
    Expr::Primary(Primary::Symbol("x".to_string())),
]));
```

# Feature flags

- `mysql`: Derives [`mysql_common`] traits for various types provided by this
crate.
- `serde`: Derives [`Serialize`] and [`Deserialize`] for various types provided
by this crate.

[`mysql_common`]: https://docs.rs/mysql-common/latest/mysql_common/
[`Serialize`]: https://docs.rs/serde/latest/serde/trait.Serialize.html
[`Deserialize`]: https://docs.rs/serde/latest/serde/trait.Deserialize.html
