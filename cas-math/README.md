# cas-math

This subcrate provides the computer algebra facilities for `cas-rs`, and additional mathematical utilities, such as unit conversion.

# Simplification

```rust
use cas_eval::consts::float;
use cas_parser::parser::{ast::Expr as AstExpr, Parser};
use cas_math::algebra::{expr::{Expr, Primary}, simplify};

let mut parser = Parser::new("x + x + x");
let ast_expr = parser.try_parse_full::<AstExpr>().unwrap();
let simplified = simplify(&ast_expr.into());

// `x + x + x = 3x`
assert_eq!(simplified, Expr::Mul(vec![
    Expr::Primary(Primary::Number(float(3))),
    Expr::Primary(Primary::Symbol("x".to_string())),
]));
```
