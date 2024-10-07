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

`cas-rs` comes with a builtin REPL to help you try out the library. Clone this repository and run `cargo run` within the `cas-rs` directory to start it. Here's a quick guide:

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

See [here](#implicit-multiplication) for caveats related to implicit multiplication.

## Assignment

Variables and functions are created using the `=` operator. Compound assignments are also supported:

```
> x = 2
2

> f(x) = x^2 + 2x + 1
()

> f(x)
9

> x *= x
4

> x ^= x
256

> f(x)
66049
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
> () + f(8) = 1'999 )
Error: invalid base in radix notation
   ╭─[input:1:13]
   │
 1 │ () + f(8) = 1'999 )
   │             ┬
   │             ╰── this value is too small
   │
   │ Help: the base must be between 2 and 64, inclusive
───╯

Error: invalid left-hand-side of assignment operator
   ╭─[input:1:1]
   │
 1 │ () + f(8) = 1'999 )
   │ ────┬──── ┬
   │     ╰──────── (1) this expression should be a symbol or function header...
   │           │
   │           ╰── (2) ...to work with this assignment operator
   │
   │ Help: maybe you meant to compare expressions with `==`?
───╯

Error: expected end of file
   ╭─[input:1:19]
   │
 1 │ () + f(8) = 1'999 )
   │                   ┬
   │                   ╰── I could not understand the remaining expression here
```

Admittedly, these are rather handpicked examples. However, it is a design goal to make the parser as helpful as possible, and this is a good start.

# Code examples

`cas-rs` utilizes a custom scripting language called "CalcScript" to enable interaction with all of its features. CalcScript is a compiled, mostly imperative, expression-oriented language, and attempts to keep syntax and visual noise minimal, while still readable. See the [`examples/`](examples) directory for examples of basic programs written in CalcScript.

# CalcScript features

## Expression-oriented

CalcScript is an expression-oriented language, meaning that every statement is an expression that evaluates to a value. Certain statements, such as blocks, are expressions that evaluate to the last statement within them.

## Block expressions

Block expressions are expressions that contain multiple statements / expressions, wrapped in curly braces. The value of a block expression is the value of the last statement within it. The value of a block expression is the value of the last expression / statement within it, or `()` if the block is empty.

In the complete CalcScript program below, the value of `t` is the value of the last statement, `x + y`, which evaluates to 5. Each of the assignment statements are also expressions, but their return values are discarded:

```
t = {
    x = 2
    y = 3
    x + y
}
```

## Scope

A scope is a region of of the program where a variable can be accessed. In CalcScript, scopes are created by the following constructs:

- Curly braces `{}`
- Function definitions `f(x) = ...`
- [`loop` expression](#loop--while--for-loops)
- [`while` expression](#loop--while--for-loops)
- [`for` expression](#loop--while--for-loops)
- [`sum` expression](#sum--product-expressions)
- [`product` expression](#sum--product-expressions)

Variables defined within a scope are **only accessible inside of that scope**. For example, in the below program, the variables `x` and `y` are only accessible within the block expression:

```
t = {
    x = 2
    y = 3
    x + y
}

// print(x) // ERROR: unknown variable `x`
// print(y) // ERROR: unknown variable `y`
```

There are nice reasons to have scopes. First, they provide a tool for organization. For instance, a calculation that uses a lot of temporary variables could declare them within a scope to avoid cluttering the global scope:

```
{
    a = 2
    b = 3
    c = 4
    d = 5
    e = 6
    f = 7
    g = 8
    h = 9
    i = 10

    print(a + b + c + d + e + f + g + h + i)
}

// print(a) // ERROR: unknown variable `a`
// print(b) // ERROR: unknown variable `b`
// ...
```

Second, scopes can increase flexibility in CalcScript. For example, builtin variables and functions cannot be overriden at the global scope:

```
// ERROR: cannot override builtin constant: `pi`
// pi = 3

// ERROR: cannot override builtin function: `hypot`
// hypot(a, b) = a * (5b^4 + 20a^2b^2 + 16a^4) / (b^4 + 12a^2b^2 + 16a^4)
```

There could be valid reasons to override these variables and functions, for instance, if you wanted to use a an approximation of `pi` for certain problems or to try another user implementation of `hypot`. This can be done by defining these variables and functions within a scope, while still keeping the global scope clean:

```
{
    pi = 3
    L = 5
    g = 9.8
    period = 2pi sqrt(L / g)
    print(period) // 4.2857...

    hypot(a, b) = a * (5b^4 + 20a^2b^2 + 16a^4) / (b^4 + 12a^2b^2 + 16a^4)
    hypotenuse = hypot(3, 4)
    print(hypotenuse) // 4.99...
}

print(pi) // 3.1415...
print(hypot) // <builtin function: hypot>
```

## Function environment capture

Functions in CalcScript capture their "environment" by value when they are defined. This means that any variables used by the function that were declared outside the function are copied "into" the function, and reused whenever the function is called. This behavior is useful for creating functions that depend on certain variables, but are not necessarily called in the same scope as those variables.

```
x = 2
y = 3
f() = x + y // `x` and `y` are captured by value

x = 4
y = 5
f() // 2 + 3 = 5
```

## Radix notation

Radix notation is a standard method of writing numbers in bases other than base-10. To type a number in radix notation, type the base, followed by a single quote, followed by the digits of the number. For example, this is the number 1072, expressed in various different bases:

```
a = 2'10000110000
b = 8'2060
c = 25'1hm
d = 32'11g
f = 47'mC

a == b && b == c && c == d && d == f
```

## Implicit multiplication

`cas-rs` features implicit multiplication as a convenience. In `cas-rs`, implicitly inserted multiplication has the **same precedence** as explicit multiplication, division, and remainder division. Adding an explicit multiplication operator in the place of implicit multiplication will **always** evaluate to the same result.

This is contrary to some calculators and mathematical literature, which will often treat implicit multiplication as having higher precedence than explicit multiplication. This would have the following behavior:

```
// THIS IS NOT THE BEHAVIOR OF CAS-RS!

a = 4

f = 1 / 2a
g = 1 / (2a)
```

In `cas-rs`, the three variables `f`, `g`, and `h` in the below example evaluate to the **same value**:

```
a = 4

f = 1 / 2a
g = (1 / 2)a
h = 1 / 2 * a
```

### Whitespace

In the below example, there must be whitespace between `a` and `c`, otherwise the parser will treat `ac` as a single symbol.

```
discriminant(a, b, c) = b^2 - 4a c
```

Implicit multiplication is restricted to individual lines, meaning that newlines **are significant** in the context of implicit multiplication. In the past, it was noted that allowing implicit multiplication to span multiple lines could lead to ambiguity and unexpected results. For example, this code produced no output prior to the change, instead of the expected `true` (the result of `fact(14) == 14!`). This was because the parser interpreted `fact(14) == 14!` as being implicitly multiplied with the preceding block expression:

```
// PREVIOUSLY, THIS CODE PRODUCED NO OUTPUT

fact(n) = {
    out = n; // semicolons were required to avoid weird implicit multiplication
    while n > 1 then {
        n -= 1;
        out *= n;
    };
    out
}
// <-- implicit multiplication was inserted here
fact(14) == 14!
```

Today, this code can be written as expected:

```
// THIS CODE PRINTS `true`

fact(n) = {
    out = n
    while n > 1 {
        n -= 1
        out *= n
    }
    out
}

fact(14) == 14!
```

## Comments

Comments in CalcScript are denoted by `//` and continue until the end of the line. Comments can be placed anywhere in the code, and any text following `//` will be ignored by the parser.

Comments are typically used to describe or explain the reasoning behind your code, or to temporarily disable a line of code for debugging purposes:

```
// the x and y-position of a point, in meters
x = 2
y = 3

// computes the distance from the origin
// distance = sqrt(x^2 + y^2)
distance = hypot(x, y) // faster than sqrt(x^2 + y^2)

distance
```

## Programming constructs

`cas-rs` supports usual programming constructs, such as `if` / `else` expressions, `loop`s, `while`, and `for` loops.

### `if` / `else` expressions

In the case of `if` / `else` expressions, you often will not need to enclose conditions or branches with any special syntax (you can do so with curly braces or parentheses if needed):

```
my_abs(x) = if x < 0 then -x else x
quadratic_formula(a, b, c, plus = true) = {
    discriminant = b^2 - 4 a c
    if discriminant >= 0 {
        left = -b / (2a)
        right = sqrt(discriminant) / (2a)
        if plus then left + right else left - right
    }
}
```

### `loop` / `while` / `for` loops

`loop`s, `while`, and `for` loops are also supported. A `loop` expression will execute its body forever, a `while` expression will run its body for as long as the given condition is true, and a `for` expression will execute its body for each integer in a range. Within the scope of a `loop` / `while` / `for` expression, the `break` and `continue` keywords can be used to break out of the loop or skip to the next iteration, respectively:

```
my_factorial(n) = {
    result = 1
    for i in 1..=n {
        result *= i
    }
    result
}
```

The `break` keyword can also be used to give a value to the loop expression. For example, the following function returns the least common multiple of two numbers:

```
lcm(a, b) = {
    i = 0
    loop {
        i += 1
        if i % a == 0 && i % b == 0 {
            break i
        }
    }
}
```

#### `then` keyword

The `then` keyword is used within the context of `if` / `else` expressions to separate the condition from the code to execute if the condition is true, and within `while` / `for` loops to separate the condition / range from the loop body. It is typically used when the `if` or loop body is "short enough", and can be omitted if the body is "clearly" the next expression, which is true for block, return, break, and continue expressions.

```
my_abs(x) = if x < 0 then -x else x
my_abs(x) = if x < 0 {
    -x
} else {
    x
}

wait(n) = {
    i = 0
    while i < n then i += 1
}
wait(n) = {
    i = 0
    while i < n {
        i += 1
    }
}
```

### `sum` / `product` expressions

`sum` and `product` expressions can be used to sum or multiply a sequence of values, represented by a **range expression**. The range expression is a sequence of values separated by `..` or `..=`. The `sum` and `product` expressions are followed by a variable name, which is used to represent the current value in the sequence. The value of the `sum` or `product` expression is the sum or product of the values in the sequence.

```
n = 100
sum i in 1..=n of i // 1 + 2 + 3 + ... + 100 = 5050
```

```
catalan(n) = product k in 2..=n of (n + k) / k
catalan(10) // (10 + 2) / 2 * (10 + 3) / 3 * ... * (10 + 10) / 10 = 16796
```

These expressions compile down to the equivalent of a `while` loop expression.

#### `of` keyword

The `of` keyword is similar to the `then` keyword in that it merely separates the range expression from the body of the `sum` or `product` expression. It can similarly be omitted if the body is "clearly" the next expression, which is true for block, return, break, and continue expressions.

```
catalan(n) = product k in 2..=n {
    (n + k) / k
}

geometric_series(a, r, n) = sum i in 0..n {
    a * r^i
}
```

## Unit type

The unit type `()` is a special type that has only one value, also called `()`. It is used to indicate that a value is not particularly useful, and is the return type of functions that don't manually return anything. This is similar to `void` in C-like languages, `None` in Python, or `undefined` in JavaScript.

Adding a semicolon to the end of an expression will evaluate, then discard the value of that expression and return `()` instead.

Using most operators with `()` will result in an evaluation error, with the exception of comparison-based operators, such as `==`, `!=`, `>`, `<`, etc. This can be useful for checking if a function call succeeded or not:

```
quadratic_formula(a, b, c, plus = true) = {
    discriminant = b^2 - 4 a c
    if discriminant >= 0 {
        left = -b / (2a)
        right = sqrt(discriminant) / (2a)
        if plus then left + right else left - right
    }
}

if quadratic_formula(1, 2, 3) == () {
    // has no real roots
} else {
    // has real roots
}
```

# Acknowledgements

- Arbitrary precision arithmetic is implemented using the [`rug`](https://gitlab.com/tspiteri/rug) crate.
- The design of `cas-parser` is heavily inspired by the Rust compiler, [`syn`](https://github.com/dtolnay/syn), and [`ariadne`](https://github.com/zesterer/ariadne).
