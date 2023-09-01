# cas-rs

`cas-rs` is an opinionated computer algebra system written in Rust, made for use with [CalcBot](https://discord.com/application-directory/674457690646249472/). It is currently in a very early stage of development.

# Features

- Robust expression parser and evaluator with very human-friendly output
- Arbitrary precision arithmetic
- Real and complex numbers
- Radix notation (e.g. `2'1010` = 10)
- 60+ built-in functions, covering many expected use cases ([see here](https://github.com/ElectrifyPro/cas-rs/blob/main/cas-eval/src/builtins/mod.rs))
- Powerful formatting options
- Builtin REPL
- And more!

# Examples

`cas-rs` comes with a REPL to help you try out the library. Clone this repository and run `cargo run` within the `cas-rs` directory to start it. Here's a quick guide:

(user-input is prefixed with `> `)

## Operators

Common operators used in programming are supported, including:

- `+`, `-`, `*`, `/`, `%`
- `^` (exponentiation)
- `!` (factorial)
- `>`, `<`, `>=`, `<=`, `==`, `!=`, `&&`, `||`, `not`
- `~==`, `~!=` (approximate equality)
- `&`, `|`, `<<`, `>>`, `~` (bitwise operators)

```
> 1 + 3
4

> 2 * 3 > 5
true

> 41!
3.3452526613163807108170062053440751665152 × 10 ^ 49
```

Implicit multiplication is also reasonably supported:

```
> x = 2
2

> 2x
4

> 2(x + 3(x + 4))
40
```

## Assignment

Variables and functions are created using the `=` operator:

```
> x = 2
2

> f(x) = x^2 + 2x + 1
()

> f(x)
9
```

Functions also support default parameters:

```
> log(100)
2

> log(8, 2)
3

> f(x, factor = 1) = x * factor
()

> f(5)
5

> f(5, 2)
10
```

## Error reporting

`cas-rs` tries to be as helpful as possible when reporting errors. For example, this is the generated error if the user inputs incomplete radix notation:

```
> 2' + 3
Error: missing value in radix notation
   ╭─[input:1:2]
   │
 1 │ 2' + 3
   │  ┬
   │  ╰── I was expecting to see a number in base 2, directly after this quote
   │
   │ Help: base 2 uses these digits (from lowest to highest value): 01
───╯
```

What if the user accidentally placed the `+` operator right next to the quote?

```
> 2'+ 3
Error: invalid digits in radix notation: `+`
   ╭─[input:1:3]
   │
 1 │ 2'+ 3
   │   ┬
   │   ╰── if you're trying to add two values, add a space between each value and this operator
   │
   │ Help: base 2 uses these digits (from lowest to highest value): 01
───╯
```

The parser always attempts to recover from syntax errors in order to be able to report as many errors as possible. For example, here's an example filled with syntax errors:

```
> (        ) + f(8) = 1'999 )
Error: missing expression inside parenthesis
   ╭─[input:1:1]
   │
 1 │ (        ) + f(8) = 1'999 )
   │ ─────┬────
   │      ╰────── add an expression here
───╯

Error: invalid base in radix notation
   ╭─[input:1:21]
   │
 1 │ (        ) + f(8) = 1'999 )
   │                     ┬
   │                     ╰── this value is too small
   │
   │ Help: the base must be between 2 and 64, inclusive
───╯

Error: invalid left-hand-side of assignment operator
   ╭─[input:1:1]
   │
 1 │ (        ) + f(8) = 1'999 )
   │ ────────┬──────── ┬
   │         ╰──────────── (1) this expression should be a symbol or function header...
   │                   │
   │                   ╰── (2) ...to work with this assignment operator
   │
   │ Help: maybe you meant to compare expressions with `==`?
───╯

Error: expected end of file
   ╭─[input:1:27]
   │
 1 │ (        ) + f(8) = 1'999 )
   │                           ┬
   │                           ╰── I could not understand the remaining expression here
───╯
```

Admittedly, these are rather handpicked examples. However, it is a design goal to make the parser as helpful as possible, and this is a good start.

# Acknowledgements

- Arbitrary precision arithmetic is implemented using the [`rug`](https://gitlab.com/tspiteri/rug) crate.
- The design of `cas-parser` is heavily inspired by the Rust compiler, [`syn`](https://github.com/dtolnay/syn), and [`ariadne`](https://github.com/zesterer/ariadne).
