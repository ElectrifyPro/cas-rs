Parser for the CalcScript language.

`cas-rs` utilizes a custom scripting language called "CalcScript" to enable
interaction with all of its features. CalcScript is a mostly imperative,
expression-oriented language. It attempts to be as close to common mathematical
notation as possible, and keep syntax and visual noise minimal and readable,
while still adding useful features. See the [`examples/`](../examples) directory
for examples of basic programs written in CalcScript.

# Usage

This crate is responsible for making sense of code written in CalcScript, before
passing the parsed code to `cas-vm` to be compiled and executed, so it's typical
to use `cas-parser` and `cas-vm` together. Here is an example of how to parse
CalcScript code:

```rust
use cas_parser::parser::{ast::Stmt, Parser};

let code = "f(x) = x^2 + 5x + 6; f(25)";
let expr = Parser::new(code).try_parse_full_many::<Stmt>().unwrap();
println!("{:#?}", expr);

// output (simplified):
// [
//     Stmt {
//         expr: Assign(Assign {
//             target: Func(FuncHeader {
//                 name: LitSym { name: "f", span: 0..1 },
//                 params: [
//                     Symbol(LitSym { name: "x", span: 2..3 }),
//                 ],
//                 span: 0..4,
//             }),
//             op: AssignOp { kind: Assign, span: 5..6 },
// ...
```

See `cas-vm`'s documentation for examples of compiling and executing CalcScript
programs.

# Language guide

Below is a guide to the language. To try out the examples below quickly, install
the REPL with the following command:

```sh
cargo install cas-rs --locked
```

Then run:

```sh
cas-rs
```

User-input is prefixed with `> `.

## Operators

Common operators used in math and programming are supported, including:

- `+`, `-`, `*`, `/`, `%` (modulo)
- `^` (exponentiation)
- `!` (factorial)
- `>`, `<`, `>=`, `<=`, `==`, `!=`, `&&`, `||`, `not`
- `~==`, `~!=` (approximate equality)
- `&`, `|`, `<<`, `>>`, `~` (bitwise operators)

```calcscript
> 1 + 3
4

> 2 * 3 > 5
true

> 41!
3.3452526613163807108170062053440751665152 × 10 ^ 49
```

Implicit multiplication (writing multiplication without the `*` operator) is
also reasonably supported:

```calcscript
> x = 2
2

> 2x
4

> 2(x + 3(x + 4))
40
```

See [here](#implicit-multiplication) for more information (and caveats) about
implicit multiplication.

## Assignment to variables and functions

Variables and functions are created using the `=` operator. Compound assignments
are also supported:

```calcscript
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

```calcscript
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

## Expression-oriented

CalcScript is an expression-oriented language. This means that all statements
are expressions that evaluate to some value. For example, these are all valid
expressions that yield integers or floating-point numbers:

```calcscript
> 1 + 2
3

> 3(4 + 5)
27

> 6 * 7 / 8
5.25
```

These are also valid expressions:

```calcscript
> t = 0
0

> (x = 2) + (y = 3)
5

> while t < 10 {
  x *= y % 416
  t += 1
}; x
118098
```

## Block expressions

The block expression, `{ ... }`, can contain multiple expressions inside of it.
It evaluates to the last expression within it:

```calcscript
> {
  x = 2
  y = 3
  x + y
}
5
```

Being an expression, a block can be used in any place where an expression would
be expected. As an example:

```calcscript
> a = 3; b = 4
4

> if { if a > b then a else b } == a then a + b else a - b
-1
```

While this example is contrived, it provides a good example of how expressive
CalcScript can be.

Block expressions

## Scope

A scope is a region of of the program where a variable can be accessed. In
CalcScript, scopes are created by the following constructs:

- Block expressions (curly braces) `{}`
- Function definitions `f(x) = ...`
- [`loop` expression](#loop--while--for-loops)
- [`while` expression](#loop--while--for-loops)
- [`for` expression](#loop--while--for-loops)
- [`sum` expression](#sum--product-expressions)
- [`product` expression](#sum--product-expressions)

Variables defined within a scope are **only accessible inside of that scope**.
For example, in the below program, attempting to access the variables `x` and
`y` outside the block expression will result in an error:

```calcscript
t = {
    x = 2
    y = 3
    x + y
}

// print(x) // ERROR: unknown variable `x`
// print(y) // ERROR: unknown variable `y`
```

There are nice reasons to have scopes. First, they provide a tool for
organization. For instance, a calculation that uses a lot of temporary variables
could declare them within a scope to avoid cluttering the global scope:

```calcscript
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

Second, scopes allow builtin variables and functions to be temporarily
overridden. By default, they cannot be overridden at the global scope:

```calcscript
// ERROR: cannot override builtin constant: `pi`
// pi = 3

// ERROR: cannot override builtin function: `hypot`
// hypot(a, b) = a * (5b^4 + 20a^2b^2 + 16a^4) / (b^4 + 12a^2b^2 + 16a^4)
```

There are valid reasons to override these variables and functions. You may want
to use an approximation of `pi` for certain problems, or try a user
implementation of a builtin function, or use `i` as a counting variable instead
of the imaginary unit. This can be done by redefining these variables and
functions within a scope, and all their meanings will change for all scopes
nested inside. And, of course, the global scope will remain clean:

```calcscript
{
    pi = 3
    L = 5
    g = 9.8
    period = 2pi sqrt(L / g)
    print(period) // 4.2857...

    for i in 0..10 {
        print(i)
    }

    hypot(a, b) = a * (5b^4 + 20a^2b^2 + 16a^4) / (b^4 + 12a^2b^2 + 16a^4)
    hypotenuse = hypot(3, 4)
    print(hypotenuse) // 4.99...
}

print(pi) // 3.1415...
print(i) // i
print(hypot) // <builtin function: hypot>
```

## Function environment capture

Functions in CalcScript capture their "environment" by value when they are
defined. This means that any variables used by the function that were declared
outside the function are copied "into" the function, and reused whenever the
function is called. This behavior is useful for creating functions that depend
on certain variables, but are not necessarily called in the same scope as those
variables.

```calcscript
x = 2
y = 3
f() = x + y // `x` and `y` are captured by value

x = 4
y = 5
f() // 2 + 3 = 5
```

## Comments

Comments in CalcScript are denoted by `//` and continue until the end of the
line. Comments can be placed anywhere in the code, and any text following `//`
will be ignored by the parser.

Comments are typically used to describe or explain the reasoning behind your
code, or to temporarily disable a line of code for debugging purposes:

```calcscript
// the x and y-position of a point, in meters
x = 2
y = 3

// computes the distance from the origin
// distance = sqrt(x^2 + y^2)
distance = hypot(x, y) // faster than sqrt(x^2 + y^2)

distance
```

## Programming constructs

`cas-rs` supports usual programming constructs, such as `if` / `else`
expressions, `loop`s, `while`, and `for` loops.

### `if` / `else` expressions

In the case of `if` / `else` expressions, you often will not need to enclose
conditions or branches with any special syntax (you can do so with curly braces
or parentheses if needed):

```calcscript
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

`loop`s, `while`, and `for` loops are also supported. A `loop` expression will
execute its body forever, a `while` expression will run its body for as long as
the given condition is true, and a `for` expression will execute its body for
each integer in a range. Within the scope of a `loop` / `while` / `for`
expression, the `break` and `continue` keywords can be used to break out of the
loop or skip to the next iteration, respectively:

```calcscript
my_factorial(n) = {
    result = 1
    for i in 1..=n {
        result *= i
    }
    result
}
```

The `break` keyword can also be used to exit a loop while also returning a value
from the loop. For example, the following function returns the least common
multiple of two numbers:

```calcscript
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

The `then` keyword is used within the context of `if` / `else` expressions to
separate the condition from the code to execute if the condition is true, and
within `while` / `for` loops to separate the condition / range from the loop
body. It is typically used when the `if` or loop body is "short enough", and can
be omitted if the body is "clearly" the next expression, which is true for
block, return, break, and continue expressions.

In the below example, each pair of declarations are equivalent:

```calcscript
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

`sum` and `product` expressions can be used to sum or multiply a sequence of
values, represented by a **range expression**. The range expression is a
sequence of values separated by `..` or `..=`. The `sum` and `product`
expressions are followed by a variable name, which is used to represent the
current value in the sequence. The value of the `sum` or `product` expression is
the sum or product of the values in the sequence.

```calcscript
n = 100
sum i in 1..=n of i // 1 + 2 + 3 + ... + 100 = 5050
```

```calcscript
catalan(n) = product k in 2..=n of (n + k) / k
catalan(10) // (10 + 2) / 2 * (10 + 3) / 3 * ... * (10 + 10) / 10 = 16796
```

These expressions compile down to the equivalent of a loop expression.

#### `of` keyword

The `of` keyword is similar to the `then` keyword in that it merely separates
the range expression from the body of the `sum` or `product` expression. It can
similarly be omitted if the body is "clearly" the next expression, which is true
for block, return, break, and continue expressions.

```calcscript
catalan(n) = product k in 2..=n {
    (n + k) / k
}

geometric_series(a, r, n) = sum i in 0..n {
    a * r^i
}
```

## Radix notation

Radix notation is CalcScript's standard method of writing integers in bases
other than base-10. To type a number in radix notation, type the base, followed
by a single quote, followed by the digits of the number. For example, this is
the number 1072, expressed in various different bases:

```calcscript
> a = 2'10000110000
1072

> b = 8'2060
1072

> c = 25'1hm
1072

> d = 32'11g
1072

> f = 47'mC
1072
```

Each base is defined in terms of the following alphabet:

```calcscript
0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+/
```

## Unit type

The unit type `()` is a special type that has only one value, also called `()`.
It is used to indicate that a value is not particularly useful, and is the
return type of functions that don't have an explicit `return` expression. This
is similar to `void` in C-like languages, `None` in Python, `undefined` in
JavaScript, and `()` in Rust.

Adding a semicolon to the end of an expression will evaluate, then discard the
value of that expression and return `()` instead.

Using most operators with `()` will result in an evaluation error, with the
exception of comparison-based operators, such as `==`, `!=`, `>`, `<`, etc. This
can be useful for checking if a function call succeeded or not:

```calcscript
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


## Implicit multiplication

CalcScript features implicit multiplication as a convenience. This means in many
cases, you can omit the `*` symbol when multiplying two expressions, as one
might in commonly accepted mathematical notation. For example, the following
code is valid:

```calcscript
> x = 2
2

> 2(x + 3)
10
```

However, there are some important things to note about implicit multiplication:

### Shares precedence with explicit multiplication

In CalcScript, implicitly inserted multiplication has the **same precedence** as
explicit multiplication, division, and remainder division. Adding an explicit
multiplication operator in the place of implicit multiplication will **always**
evaluate to the same result.

This is contrary to some calculators and mathematical literature, which will
often treat implicit multiplication as having higher precedence than explicit
multiplication. For example, running this example on some other calculators
would result in `f` and `g` having the same value:

```calcscript
// !!! THIS IS NOT THE BEHAVIOR OF CAS-RS! !!!

a = 4

f = 1 / 2a
g = 1 / (2a)
```

In the following CalcScript example, `f`, `g`, and `h` evaluate to the **same
value** (`1 / 2 * 4 = 2`):

```calcscript
a = 4

f = 1 / 2a
g = (1 / 2)a
h = 1 / 2 * a
```

It's important to remember this distinction when copying mathematical notation
into CalcScript.

### Whitespace

CalcScript is parsed deterministically, meaning that the parser will always
produce the same result for the same input. However, implicit multiplication and
whitespace can have unexpected interactions that may appear ambiguous.

#### Between symbols

In the below example, there must be whitespace between `a` and `c`, otherwise
the parser will treat `ac` as a single symbol.

```calcscript
discriminant(a, b, c) = b^2 - 4a c
```

This example would fail when calling the function at runtime, due to the
variable `ac` not being defined:

```calcscript
discriminant(a, b, c) = b^2 - 4ac
```

#### Function calls

Currently, any expression like `f(x)` is interepted as a function call, not `f`
multiplied by `x`. Additionally, `f (x)` (with one or more spaces in between) is
_also_ interpreted as a function call.

I am considering changing this behavior in the future; see
[this issue](https://github.com/ElectrifyPro/cas-rs/issues/3) where I've
weighed multiple alternatives.

#### Newlines

Implicit multiplication is restricted to **individual lines**.

This may seem like an obvious choice, but in the past, this wasn't the case,
which made it incredibly easy to write ambiguous code that produced unexpected
results. For example, today, this code will output `true`, as expected:

```calcscript
my_factorial(n) = {
    out = n
    while n > 1 {
        n -= 1
        out *= n
    }
    out
}

my_factorial(14) == 14!
```

But in the past, this code would not have compiled due to implicit
multiplication being inserted everywhere (literally). A semicolon (`;`) was
required if one wanted to avoid this behavior:

```calcscript
my_factorial(n) = {
    out = n;
    while n > 1 then {
        n -= 1;
        out *= n;
    };
    out
};

my_factorial(14) == 14!
```

Today, these semicolons are optional, and are only necessary if you want to
write multiple statements on a single line.

## High-quality error reporting

It is a design goal to make the parser as helpful as possible. For example, this
is the generated error if the user inputs incomplete [radix
notation](#radix-notation):

```calcscript
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

Here is a variant of the above error:

```calcscript
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

There is a lot of room for improvement in these error messages, but this is a
good start.
