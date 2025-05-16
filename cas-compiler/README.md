Bytecode compiler for CalcScript.

This crate provides a complete bytecode compiler for CalcScript. It generates
bytecode instructions that can then be executed by CalcScript's virtual machine
([`Vm`]).

# Usage

For many use cases, you can use `cas-compiler` compilation feature from the
[`cas_vm`] crate. See its documentation for more information. However, if
needed, `cas-compiler` provides plenty of tools to help you manipulate the
bytecode instructions directly, before you pass them to the virtual machine.

The [`Compiler`] type is the main entry point to the compiler. To compile code,
simply call [`Compiler::compile`] with any parsed code to add its corresponding
bytecode instructions to the compiler.

```rust
use cas_compiler::Compiler;
use cas_parser::parser::{ast::Expr, Parser};

let ast = Parser::new("e^(i pi) + 1").try_parse_full::<Expr>().unwrap();
let compiler = Compiler::compile(ast).unwrap();

// compilation is done to the "main" chunk, `chunk[0]`
// other functions are compiled to their own chunks
println!("{:#?}", compiler.chunks[0]);

// spans below are mainly used for error reporting
use cas_compiler::{item::Symbol::Builtin, Instruction, InstructionKind::*};
use cas_parser::parser::token::op::BinOpKind::*;
assert_eq!(compiler.chunks[0].instructions, vec![
  Instruction { kind: LoadVar(Builtin("e")), spans: vec![0..1] },
  Instruction { kind: LoadVar(Builtin("i")), spans: vec![3..4] },
  Instruction { kind: LoadVar(Builtin("pi")), spans: vec![5..7] },
  Instruction { kind: Binary(Mul), spans: vec![3..4, 4..5, 5..7] },
  Instruction { kind: Binary(Exp), spans: vec![0..1, 1..2, 2..8] },
  Instruction { kind: LoadConst(1.into()), spans: vec![] },
  Instruction { kind: Binary(Add), spans: vec![0..8, 9..10, 11..12] },
]);
```

Or, compile a complete program with multiple chunks:

```rust
use cas_compiler::Compiler;
use cas_parser::parser::{ast::Stmt, Parser};

let ast = Parser::new("cache = [-1; 64]
fibonacci(n) = if n <= 1 then n else {
    if cache[n] != -1 return cache[n]
    cache[n] = fibonacci(n - 1) + fibonacci(n - 2)
}

fibonacci(64)").try_parse_full_many::<Stmt>().unwrap();
let compiler = Compiler::compile_program(ast).unwrap();

// body of fibonacci function is compiled to chunk[1]
println!("{:#?}", compiler.chunks[0]); // main chunk
println!("{:#?}", compiler.chunks[1]); // fibonacci(n)
```

# CalcScript semantics

These sections will quickly discuss CalcScript's semantics and how they are
implemented in the compiler. For a more high-level / user-friendly overview of
CalcScript, please refer to
[`cas_parser`](https://docs.rs/cas_parser/latest/cas_parser/index.html).

## Chunks and functions

The compiler generates bytecode instructions in **chunks**. A chunk is the
compiler's logical representation of a function, with each chunk containing the
sequence of instructions that represent the function's body. The compiler
generates a new chunk for each function declaration in the source code, with one
predefined chunk for the implicit "main" function, i.e. entry point of the
program.

So, for example, in this example CalcScript program:

```calcscript
cache = [-1; 64]
fibonacci(n) = if n <= 1 then n else {
    if cache[n] != -1 return cache[n]
    cache[n] = fibonacci(n - 1) + fibonacci(n - 2)
}

fibonacci(64)
```

There are two chunks generated:

- the main chunk, which contains the instructions for the `cache` variable
  assignment and the call to `fibonacci(64)`.
- the `fibonacci` chunk, which contains the instructions for the function
  declaration and its body.

## Lexical scoping

Functions in CalcScript are lexically scoped, meaning the scope of a variable is
determined by its position in the source code.

Under the hood, the compiler uses a scope stack as it visits the AST to keep
track of the current scope, and also handle symbol resolution. When a new scope
is entered (e.g. through a function declaration, block, etc.), a new scope is
pushed onto the stack. When the scope is exited, the corresponding scope is
popped from the stack. This ensures variables are resolved correctly based on
their lexical scope.

```calcscript
distance = {
    a = 3
    b = 4
    sqrt(a^2 + b^2)
}

print(distance) // 5
// print(a) // error: a is not defined
// print(b) // error: b is not defined
```

## Function environment capture

Functions in CalcScript capture their environment at definition. Whenever a
function is declared, it copies the values of the variables from its enclosing
scope that it uses "into" its own scope. Then, when the function is called, it
uses the values of those variables from the time of its definition, not the time
of its call.

This behavior is similar to how closures work in many programming languages,
and in fact, all CalcScript functions are closures. This ensures consistent and
predictable behavior when using functions.

```calcscript
x = 10

// The function `f` captures the value of `x` at the time it is defined, which
// is 10 in this case. Environment capture means that the value of `x` used
// inside `f` will always be 10, even if we change `x` later below.
f() = x * 2
print(f()) // 20

x = 55
print(f()) // 20

[x, f()]   // [55, 20]
```

## Shadowing

CalcScript allows shadowing of builtin symbols _outside of the global scope_.
This means that if a function / variable is declared with the same name as a
builtin symbol (e.g. `pi`, `e`, `i`, `sqrt`, etc.), using that name will resolve
to the newly declared symbol instead of the builtin symbol. This is only legal
outside of the global scope, meaning that builtins cannot be overriden at the
global scope.

This allows some scenarios where it would be useful to override a builtin
constant for a specific operation, or to use a builtin symbol as a loop
variable.

```calcscript
// Shadowing makes it legal to use `i` as a loop variable (`for` loops create
// new scopes).
for i in 0..10 {
    print(i)
}

// Using shadowing to approximate `pi = 3`:
period = {
    pi = 3
    L = 5
    g = 9.8
    2pi sqrt(L / g)
}

print(period) // 4.2857...
```

## Value stack

The [`Vm`] is a stack-based virtual machine. It uses a value stack to store
intermediate values during the execution of bytecode instructions. Each
CalcScript statement is expected to finish executing without leaving any extra
values on the stack, unless if the statement is the last statement in a block
or chunk, in which case, its return value is left on the stack.

These invariants are maintained automatically by the compiler when compiling a
program using [`Compiler::compile_program`]. If you need to compile expressions
manually or one-by-one, you will need to uphold these invariants yourself. See
[`Compiler`] for more information.

[`Vm`]: https://docs.rs/cas_vm/latest/cas_vm/struct.Vm.html
[`cas_vm`]: https://docs.rs/cas_vm/latest/cas_vm/index.html
[`Compiler::compile_program`]: https://docs.rs/cas_compiler/latest/cas_compiler/struct.Compiler.html#method.compile_program
[`Compiler`]: https://docs.rs/cas_compiler/latest/cas_compiler/struct.Compiler.html
