# cas-compute

This subcrate provides the computer algebra facilities for `cas-rs`.

`cas-compute` is split further into two submodules, `numerical` and `symbolic`.

# Simplification

```rust
use cas_compute::numerical::consts::int;
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
