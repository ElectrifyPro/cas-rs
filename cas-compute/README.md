Tools for evaluation of CalcScript expressions.

This crate provides tools to help evaluate CalcScript code. It is split into two
main submodules, `funcs` and `symbolic`, which provide implementations of useful
mathematical functions, and symbolic evaluation respectively. Currently,
symbolic manipulation is limited to simplification, but this will be extended in
the future.

# Usage

## Function implementations

The `funcs` module statically implements a variety of algorithms and functions
used in CalcScript, such as trigonometric, combinatoric, and probability
distribution functions. The functions use the arbitrary-precision types from
the [`rug`] crate to ensure that they can handle high-precision calculations.

To use a function, import it from the `funcs` module and call it with the
appropriate arguments. You may need to convert the arguments to the expected
[`rug`] these conversions.

```rust
use cas_compute::funcs::miscellaneous::{Factorial, Gamma};
use cas_compute::numerical::value::Value;
use cas_compute::primitive::{complex, float};

let z = complex((8.0, 0.0)); // 8 + 0i
// let z = complex(8); // equivalent
let gamma_z = Gamma::eval_static(z);

// Factorial supports floats, so it returns a `Value` for it to support both
// floating-point and integer return types
let factorial_i = Factorial::eval_static(float(7)); // 7! = 5040

// the effect is that we need to wrap `gamma_z` in a `Value` to compare it with
// factorial_i, or extract a `rug::Complex` from `factorial_i` to compare it
// with `gamma_z`
//
// this will most likely be improved in the future

assert_eq!(Value::Complex(gamma_z).coerce_integer(), factorial_i);
```

## Numerical helpers

The `numerical` module provides helpers used in other `cas-rs` libraries, mainly
the [`Value`] type and [`Value`] formatting. Since version 0.2.0, it no longer
provides numerical evaluation; instead, that functionality was moved to `cas-vm`
as CalcScript became a compiled language.

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

[`rug`]: https://gitlab.com/tspiteri/rug
[`Value`]: https://docs.rs/cas-compute/latest/cas_compute/numerical/value/enum.Value.html
[`mysql_common`]: https://docs.rs/mysql-common/latest/mysql_common/
[`Serialize`]: https://docs.rs/serde/latest/serde/trait.Serialize.html
[`Deserialize`]: https://docs.rs/serde/latest/serde/trait.Deserialize.html
