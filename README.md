# cas-rs

`cas-rs` is an opinionated computer algebra system written in Rust, made for use with [CalcBot](https://discord.com/application-directory/674457690646249472/). It is currently in a very early stage of development.

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

# REPL

`cas-rs` comes with a builtin REPL to help you try out the library. Clone this repository and run the following command to start it:

```sh
cargo run --example repl
```

`cas-rs` utilizes a custom scripting language called "CalcScript" to enable interaction with all of its features. CalcScript is a mostly imperative, expression-oriented language, and attempts to keep syntax and visual noise minimal, while still readable. See the [`examples/`](examples) directory for examples of basic programs written in CalcScript.

To learn more about the language, see the [`cas-parser`](cas-parser/README.md) crate.

# Acknowledgements

- Arbitrary precision arithmetic is implemented using the [`rug`](https://gitlab.com/tspiteri/rug) crate.
- The design of `cas-parser` is heavily inspired by the Rust compiler, [`syn`](https://github.com/dtolnay/syn), and [`ariadne`](https://github.com/zesterer/ariadne).
