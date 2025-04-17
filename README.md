# cas-rs

`cas-rs` is an opinionated computer algebra system written in Rust, originally made for use with the Discord bot [CalcBot](https://discord.com/application-directory/674457690646249472/). It is currently in a very early stage of development.

See below for a guide on the REPL and some code examples.

# Features

- Simplification of algebraic expressions, learn more [here](cas-compute/README.md)
- Graphing calculator, learn more [here](cas-graph/README.md)
- Robust expression parser and evaluator with very human-friendly output
- Arbitrary precision arithmetic
- Real and complex numbers
- Radix notation (e.g. `2'1010` = 10)
- 60+ built-in functions, covering many expected use cases ([see here](https://github.com/ElectrifyPro/cas-rs/blob/dev/cas-compute/src/funcs/mod.rs))
- Powerful formatting options, including LaTeX code output
- Builtin REPL
- And more!

# Quick start

Below are some routine examples of how `cas-rs` can be used.

## REPL

`cas-rs` comes with a builtin scripting language called "CalcScript" along with a REPL to help you try out the library. CalcScript is a compiled, mostly imperative, expression-oriented language, and attempts to keep syntax and visual noise minimal, while still readable. See the [`examples/`](examples) directory for examples of basic programs written in CalcScript.

To install the REPL, run the following command (Rust needs to be installed):

```sh
cargo install cas-rs --locked
```

You can then run the REPL with:

```sh
cas-rs
```

Which will immediately drop you into a REPL session where you can run CalcScript code:

```text
> x = (1 + sqrt(5))/2
1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475408807538689175212663386222353693179318006077
> x == phi
true
```

To learn more about CalcScript and example REPL usage, see the [`cas-parser`](cas-parser/README.md) crate.

## General expression evaluation

`cas-rs` can also be used as a library to evaluate CalcScript expressions. Here is an example of evaluating the expression `x^2 + 5x + 6` where `x = 2`:

```rust
use cas_compute::numerical::{ctxt::Ctxt, eval::Eval};
use cas_parser::parser::{ast::Expr, Parser};

let mut parser = Parser::new("x^2 + 5x + 6");
let ast_expr = parser.try_parse_full::<Expr>().unwrap();

let mut ctxt = Ctxt::default();
ctxt.add_var("x", 2.into());

let result = ast_expr.eval(&mut ctxt).unwrap();
assert_eq!(result, 24.into());
```

Learn more about this in the [`cas-compute`](cas-compute/README.md) crate.

## Graphing calculator

A customizable graphing calculator is provided with the `cas-graph` crate. You can create a graph of multiple functions and points, customize the appearance of the viewport, functions, and points, and render the graph to a PNG file (or any format supported by the [`cairo`](https://gtk-rs.org/gtk-rs-core/stable/latest/docs/cairo/) crate.

```rust
use cas_graph::Graph;
use std::fs::File;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let surface = Graph::default()
        .try_add_expr("x^2 + 5x + 6").unwrap()
        .add_point((-5.0, 6.0))
        .add_point((-4.0, 2.0))
        .add_point((-3.0, 0.0))
        .add_point((-2.0, 0.0))
        .add_point((-1.0, 2.0))
        .center_on_points()
        .draw()?;

    let mut file = File::create("parabola.png")?;
    surface.write_to_png(&mut file)?;

    Ok(())
}
```

Output (note: colors were randomly chosen; random color selection is not
included in the example code):

<img src="https://raw.githubusercontent.com/ElectrifyPro/cas-rs/main/cas-graph/img/parabola.png" width="500" height="500"/>

# Acknowledgements

- Arbitrary precision arithmetic is implemented using the [`rug`](https://gitlab.com/tspiteri/rug) crate.
- The design of `cas-parser` is heavily inspired by the Rust compiler, [`syn`](https://github.com/dtolnay/syn), and [`ariadne`](https://github.com/zesterer/ariadne).
