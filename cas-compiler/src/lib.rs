#![doc = include_str!("../README.md")]

pub mod expr;
pub mod error;
pub mod instruction;
pub mod item;
pub mod sym_table;

use cas_compute::{consts::all as all_consts, funcs::all as all_funcs};
use cas_error::Error;
use cas_parser::parser::ast::{FuncHeader, LitSym, Stmt};
use error::{
    OverrideBuiltinConstant,
    OverrideBuiltinFunction,
    UnknownVariable,
};
use std::collections::{HashMap, HashSet};
use expr::compile_stmts;
pub use instruction::{Instruction, InstructionKind};
use item::{FuncDecl, Item, Symbol, SymbolDecl};
use std::ops::Range;
use sym_table::{Scope, SymbolTable};

/// A label that can be used to reference a specific instruction in the bytecode.
///
/// The internal value is simply a unique ID that is resolved to the actual instruction during
/// execution.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Label(usize);

/// The state of the compiler.
#[derive(Clone, Debug, Default)]
pub struct CompilerState {
    /// The label pointing to the start of the current loop.
    pub loop_start: Option<Label>,

    /// The label pointing to the end of the current loop.
    pub loop_end: Option<Label>,

    /// Whether the current statement is the last statement in a block or the program, indicating
    /// that its return value is the return value of the block / program.
    pub last_stmt: bool,

    /// Whether the expression being compiled is a top-level assignment expression, indicating that
    /// its return value is not used.
    ///
    /// This is used to determine whether to use the [`InstructionKind::StoreVar`] or
    /// [`InstructionKind::AssignVar`] instruction.
    ///
    /// For example, in the following code:
    ///
    /// ```calcscript
    /// x = y = 2
    /// ```
    ///
    /// The `x = ...` expression is a top-level assignment expression, so its return value is not
    /// used within the same statement. However, the `y = 2` expression is not a top-level
    /// assignment, as its return value is then passed to the `x = ...` expression. In this case,
    /// the compiler will generate a [`InstructionKind::AssignVar`] instruction for `x` and a
    /// [`InstructionKind::StoreVar`] instruction for `y`.
    // TODO: this is not implemented yet
    pub top_level_assign: bool,
}

/// A chunk containing a function definition.
#[derive(Clone, Debug, Default)]
pub struct Chunk {
    /// The instructions in this chunk.
    pub instructions: Vec<Instruction>,

    /// The number of arguments the function takes.
    pub arity: usize,
}

impl Chunk {
    /// Creates a new chunk with the given arity.
    pub fn new(arity: usize) -> Self {
        Self {
            instructions: Vec::new(),
            arity,
        }
    }
}

/// Value returned by [`Compiler::new_chunk`].
pub struct NewChunk {
    /// The unique identifier for the function.
    pub id: usize,

    /// The index of the new chunk.
    pub chunk: usize,

    /// The symbols captured by the function from parent scopes.
    pub captures: HashSet<usize>,
}

/// Returns the corresponding error if a symbol matches the name of a builtin symbol or function.
fn check_override_builtin(symbol: &LitSym) -> Result<(), Error> {
    if all_consts().contains(&*symbol.name) {
        return Err(Error::new(vec![symbol.span.clone()], OverrideBuiltinConstant {
            name: symbol.name.to_string(),
        }));
    }
    if all_funcs().contains_key(&*symbol.name) {
        return Err(Error::new(vec![symbol.span.clone()], OverrideBuiltinFunction {
            name: symbol.name.to_string(),
        }));
    }
    Ok(())
}

/// Returns the builtin constant or function with the given name, if it exists.
fn resolve_builtin(symbol: &LitSym) -> Option<Symbol> {
    all_consts()
        .get(&*symbol.name)
        .map(|name| Symbol::Builtin(name))
        .or_else(|| {
            all_funcs()
                .get(&*symbol.name)
                .map(|func| Symbol::Builtin(func.name()))
        })
}

/// A compiler that provides tools to generate bytecode instructions for `cas-rs`'s virtual machine
/// (see [`Vm`]).
///
/// **Note: If you're looking to run a CalcScript program, you should use the [`Vm`] struct found
/// in `cas-vm` instead.**
///
/// This is the main entry point to the compiler. The compiler translates a CalcScript AST
/// (produced by [`cas_parser`]) into a series of bytecode [`Chunk`]s, which can then be executed
/// by `cas-rs`'s [`Vm`]. It also is mostly responsible for managing CalcScript's semantics through
/// lexical scoping and symbol resolution, value stack layout, and generation of chunks. These
/// details are described later in this documentation, but are not important if you're just looking
/// to run a program.
///
/// To compile a complete program, it is recommended that you use [`Compiler::compile_program`],
/// which is the easiest way to ensure the resulting bytecode is valid. However, there are also a
/// number of other methods that can be used to manually compile CalcScript. There is one important
/// rule to keep in mind when taking this approach (which would otherwise be handled by the
/// compiler), which requires a quick explanation of [`Vm`]'s value stack.
///
/// During execution, [`Vm`] uses a value stack to keep track of values generated by and used
/// around the bytecode. The compiler must ensure that the instructions it generates manipulates
/// the value stack's semantics correctly.
///
/// The most important rule is that **the value stack must have exactly one value on it when the
/// program finishes executing**. This is the value that is returned by the program (printed when
/// using the `cas-rs` REPL). When manually compiling a program, you must ensure that each
/// statement's instructions leave no value on the stack when the statement completes, except for
/// the last statement in a block or chunk.
///
/// (Note that failing to uphold this rule will never result in undefined behavior; it will most
/// likely either panic or result in an error during execution.)
///
/// Most CalcScript programs consist of a sequence of statements, for example:
///
/// ```calcscript
/// x = 3
/// y = 4
/// z = hypot(x, y)
/// ```
///
/// In this case, the compiler generates these instructions:
///
/// ```rust
/// use cas_compiler::Compiler;
/// use cas_parser::parser::Parser;
///
/// let ast = Parser::new("x = 3
/// y = 4
/// z = hypot(x, y)").try_parse_full_many().unwrap();
///
/// let compiler = Compiler::compile_program(ast).unwrap();
///
/// use cas_compiler::{item::Symbol, Instruction, InstructionKind::*};
/// assert_eq!(compiler.chunks[0].instructions, vec![
///     // x = 3
///     Instruction { kind: LoadConst(3.into()), spans: vec![] },
///     Instruction { kind: StoreVar(0), spans: vec![] },
///     Instruction { kind: Drop, spans: vec![] },
///
///     // x = 4
///     Instruction { kind: LoadConst(4.into()), spans: vec![] },
///     Instruction { kind: StoreVar(1), spans: vec![] },
///     Instruction { kind: Drop, spans: vec![] },
///
///     // z = hypot(x, y)
///     Instruction { kind: LoadVar(Symbol::User(0)), spans: vec![22..23] },
///     Instruction { kind: LoadVar(Symbol::User(1)), spans: vec![25..26] },
///     Instruction { kind: LoadVar(Symbol::Builtin("hypot")), spans: vec![16..21] },
///     Instruction { kind: Call(2), spans: vec![16..22, 26..27, 22..23, 25..26] },
///     Instruction { kind: StoreVar(2), spans: vec![] }
/// ]);
/// ```
///
/// Notice that each statement is terminated by a [`InstructionKind::Drop`] instruction, except for
/// the last one. For example, the first statement, `x = 3`, loads the constant `3` onto the stack.
/// The [`InstructionKind::StoreVar`] instruction stores the value into the variable `x` (0), but
/// does not remove it from the stack. The `Drop` instruction then removes the value from the
/// stack, leaving the stack empty. (It is more optimal to use [`InstructionKind::AssignVar`] in
/// this case, but the compiler does not implement this behavior yet.)
///
/// The final statement, `z = hypot(x, y)`, stores the computed value into the variable `z` (2),
/// but does not drop the value from the stack, making it the final value on the stack, and thus
/// the return value of the program.
///
/// You need to be mindful of this behavior when manually compiling programs.
///
/// [`Vm`]: https://docs.rs/cas-vm/latest/cas_vm/vm/struct.Vm.html.
#[derive(Clone, Debug)]
pub struct Compiler {
    /// The bytecode chunks generated by the compiler.
    ///
    /// The entire program is represented as multiple chunks of bytecode, where each chunk
    /// represents a function body. The first chunk represents the implicit "main" function.
    pub chunks: Vec<Chunk>,

    /// Labels generated by the compiler, mapped to the index of the instruction they reference.
    ///
    /// When created, labels aren't associated with any instruction. Before the bytecode is
    /// executed, the compiler will resolve these labels to the actual instruction indices.
    pub labels: HashMap<Label, Option<(usize, usize)>>,

    /// A symbol table that maps identifiers to information about the values they represent.
    ///
    /// This is used to store information about variables and functions that are defined in the
    /// program.
    pub sym_table: SymbolTable,

    /// Index of the current chunk.
    ///
    /// This value is manually updated by the compiler.
    chunk: usize,

    /// Next unique identifier for a symbol or function.
    next_item_id: usize,

    /// Holds state for the current loop.
    state: CompilerState,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            chunks: vec![Chunk::default()], // add main chunk
            labels: Default::default(),
            sym_table: Default::default(),
            chunk: 0,
            next_item_id: 0,
            state: Default::default(),
        }
    }
}

impl Compiler {
    /// Creates a new compiler.
    pub fn new() -> Self {
        Self::default()
    }

    /// Compiles the given type into a sequence of [`Instruction`]s.
    pub fn compile<T: Compile>(expr: T) -> Result<Self, Error> {
        let mut compiler = Self::new();
        expr.compile(&mut compiler)?;
        Ok(compiler)
    }

    /// Compiles multiple statements into a sequence of [`Instruction`]s.
    pub fn compile_program(stmts: Vec<Stmt>) -> Result<Self, Error> {
        let mut compiler = Self::new();
        compile_stmts(&stmts, &mut compiler)?;
        Ok(compiler)
    }

    /// Creates a new compilation scope with the given modified state. Compilation that occurs in
    /// this scope will then use the modified state.
    pub fn with_state<F, G>(&mut self, modify_state: F, compile: G) -> Result<(), Error>
    where
        F: FnOnce(&mut CompilerState),
        G: FnOnce(&mut Self) -> Result<(), Error>,
    {
        let old_state = self.state.clone();
        modify_state(&mut self.state);
        compile(&mut *self)?;
        self.state = old_state;
        Ok(())
    }

    /// Returns an immutable reference to the current chunk.
    pub fn chunk(&self) -> &Chunk {
        self.chunks.get(self.chunk).unwrap()
    }

    /// Returns a mutable reference to the current chunk.
    pub fn chunk_mut(&mut self) -> &mut Chunk {
        self.chunks.get_mut(self.chunk).unwrap()
    }

    /// Add an item to the symbol table at the current scope.
    ///
    /// If the item to add matches that of a builtin item, one of the following will occur:
    ///
    /// - If this function is called from the global scope, an [`OverrideBuiltinConstant`] or
    ///   [`OverrideBuiltinFunction`] error is returned.
    /// - If this function is called anywhere else, the symbol table will successfully be updated
    ///   with the new item. This item shadows the existing builtin, meaning the builtin will not
    ///   be accessible until the scope in which this item was declared, ends.
    pub fn add_item(&mut self, symbol: &LitSym, item: Item) -> Result<(), Error> {
        // if we are in the global scope, ensure we don't accidentally override builtin constants
        // and functions. this is because there would be no way to access the builtin constants and
        // functions after they are overridden

        // it's ok to allow overriding in deeper scopes, as the user will still be able to access
        // the builtin constants and functions afterward

        // TODO: shadowing should probably be explicit (i.e. with `let` keyword)
        if self.sym_table.is_global_scope() {
            // existence of higher order functions means that a symbol could potentially override
            // a builtin symbol or function, even if it is not explicitly declared as a function or
            // symbol (see no_override_builtin test at bottom of file)
            check_override_builtin(symbol)?;
        }

        // add to this final table
        self.sym_table.insert(symbol.name.to_string(), item);
        Ok(())
    }

    /// Creates a new scope in the symbol table. Within the provided function, all `compiler`
    /// methods that add or mutate symbols will do so in the new scope.
    ///
    /// The scope is popped off the symbol table stack when the function returns. If no symbols
    /// were added to the scope, it will not be added to the symbol table.
    pub fn new_scope<F>(&mut self, f: F) -> Result<(), Error>
        where F: FnOnce(&mut Compiler) -> Result<(), Error>
    {
        self.sym_table.enter_scope();
        f(self)?;
        self.sym_table.exit_scope();
        Ok(())
    }

    /// Creates a new scope in the symbol table. Within the provided function, all `compiler`
    /// methods that add or mutate symbols will do so in the new scope.
    ///
    /// The scope is popped when the function returns, and a reference to the scope is returned. It
    /// will always be added to the symbol table, even if no symbols were added to it.
    pub(crate) fn new_scope_get<F>(&mut self, f: F) -> Result<&Scope, Error>
        where F: FnOnce(&mut Compiler) -> Result<(), Error>
    {
        self.sym_table.enter_scope();
        f(self)?;
        Ok(self.sym_table.exit_scope_get())
    }

    /// Creates a new chunk and a scope for compilation. Within the provided function, all
    /// `compiler` methods that add or edit instructions will do so to the new chunk.
    ///
    /// Returns the unique identifier for the function and the index of the new chunk, which will
    /// be used to add corresponding [`InstructionKind::LoadConst`] and [`InstructionKind::StoreVar`]
    /// instructions to the parent chunk.
    pub fn new_chunk<F>(&mut self, header: &FuncHeader, f: F) -> Result<NewChunk, Error>
        where F: FnOnce(&mut Compiler) -> Result<(), Error>
    {
        let old_chunk_idx = self.chunk;
        self.chunks.push(Chunk::new(header.params.len()));
        let new_chunk_idx = self.chunks.len() - 1;

        let id = self.next_item_id;
        self.add_item(
            &header.name,
            Item::Func(FuncDecl::new(
                id,
                self.sym_table.next_id(),
                new_chunk_idx,
                header.params.clone(),
            )),
        )?;
        self.next_item_id += 1;

        self.chunk = new_chunk_idx;
        let scope = self.new_scope_get(f)?;
        let captures = scope.captures()
            .iter()
            .map(|symbol| match symbol {
                Symbol::User(id) => *id,
                _ => unreachable!(),
            })
            .collect();
        self.chunk = old_chunk_idx;

        Ok(NewChunk {
            id,
            chunk: new_chunk_idx,
            captures,
        })
    }

    /// Creates a new temporary chunk for compilation. Within the provided function, all `compiler`
    /// methods that add or edit instructions will do so to the new chunk.
    ///
    /// The chunk is returned at the end of this function, without being added to the list of
    /// chunks.
    pub(crate) fn new_chunk_get<F>(&mut self, f: F) -> Result<Chunk, Error>
        where F: FnOnce(&mut Compiler) -> Result<(), Error>
    {
        let old_chunk_idx = self.chunk;
        self.chunks.push(Chunk::default());
        let new_chunk_idx = self.chunks.len() - 1;

        self.chunk = new_chunk_idx;
        f(self)?;
        let chunk = self.chunks.pop().unwrap();
        self.chunk = old_chunk_idx;

        Ok(chunk)
    }

    /// Adds a symbol to the symbol table at the current scope.
    ///
    /// This is a shortcut for [`Compiler::add_item`] that creates a new [`Item::Symbol`] from the
    /// given symbol and returns the unique identifier for the symbol.
    ///
    /// # Manual compilation
    ///
    /// [`Compiler::add_symbol`] can be used to declare the existence of uninitialized variables.
    /// This is useful for creating a symbol and acquiring its unique identifier in order to
    /// manipulate it in a virtual machine.
    ///
    /// If you do this, you must ensure that the symbol is initialized before it is used. This can
    /// be done in the virtual machine. See the [`cas-vm`] crate for an example.
    ///
    /// [`cas-vm`]: https://docs.rs/cas-vm/latest/cas_vm/
    pub fn add_symbol(&mut self, symbol: &LitSym) -> Result<usize, Error> {
        let id = self.next_item_id;
        self.add_item(symbol, Item::Symbol(SymbolDecl { id }))?;
        self.next_item_id += 1;
        Ok(id)
    }

    /// Resolves a path to a user-created symbol, inserting it into the symbol table if it doesn't
    /// exist.
    ///
    /// If the symbol name matches that of a builtin constant, one of the following will occur:
    ///
    /// - If this function is called from the global scope, an [`OverrideBuiltinConstant`]
    ///   error is returned.
    /// - If this function is called anywhere else, the symbol table will successfully be updated
    ///   with the new symbol. This symbol shadows the existing builtin constant, meaning the
    ///   builtin will not be accessible until the scope in which this symbol was declared, ends.
    ///
    /// Returns the unique identifier for the symbol, which can be used to reference the symbol in
    /// the bytecode.
    pub fn resolve_user_symbol_or_insert(&mut self, symbol: &LitSym) -> Result<usize, Error> {
        if let Some(item) = self.sym_table.resolve_item(&symbol.name) {
            // symbol was found in the current or a parent scope
            Ok(item.id())
        } else {
            // if not, insert it
            self.add_symbol(symbol)
        }
    }

    /// Resolves a path to a symbol without inserting it into the symbol table. If the symbol is
    /// determined to be captured from a parent scope, the enclosing function will be marked as
    /// capturing the symbol.
    ///
    /// Returns the unique identifier for the symbol, or an error if the symbol is not found within
    /// the current scope.
    pub fn resolve_symbol(&mut self, symbol: &LitSym) -> Result<Symbol, Error> {
        if let Some(symbol) = self.sym_table.resolve_item_mark_capture(&symbol.name) {
            // symbol was found in the current or a parent scope
            Ok(symbol)
        } else {
            // maybe it refers to a builtin constant or function
            if let Some(symbol) = resolve_builtin(symbol) {
                Ok(symbol)
            } else {
                // no matching symbol found
                Err(Error::new(vec![symbol.span.clone()], UnknownVariable {
                    name: symbol.name.clone(),
                }))
            }
        }
    }

    /// Adds an instruction to the current chunk with no associated source code span.
    pub fn add_instr(&mut self, instruction: impl Into<Instruction>) {
        let chunk = self.chunk_mut();
        chunk.instructions.push(instruction.into());
    }

    /// Adds an instruction to the current chunk with an associated source code span(s).
    pub fn add_instr_with_spans(
        &mut self,
        instruction: impl Into<Instruction>,
        spans: Vec<Range<usize>>,
    ) {
        let mut instruction = instruction.into();
        instruction.spans = spans;
        let chunk = self.chunk_mut();
        chunk.instructions.push(instruction);
    }

    /// Adds a sequence of instructions from a chunk to the current chunk.
    pub(crate) fn add_chunk_instrs(&mut self, new_chunk: Chunk) {
        let chunk = self.chunk_mut();
        chunk.instructions.extend(new_chunk.instructions);
    }

    /// Replaces an instruction at the given index in the current chunk with a new instruction.
    pub fn replace_instr(&mut self, idx: usize, instruction: Instruction) {
        let chunk = self.chunk_mut();
        chunk.instructions[idx] = instruction;
    }

    /// Creates a unique label with no associated instruction. This label can be used to reference
    /// a specific instruction in the bytecode.
    pub fn new_unassociated_label(&mut self) -> Label {
        let label = Label(self.labels.len());
        self.labels.insert(label, None);
        label
    }

    /// Creates a unique label pointing to the end of the currently generated bytecode in the
    /// current chunk.
    ///
    /// When this method is called and [`Compile::compile`] is called immediately after, the label
    /// will point to the first instruction generated by the compilation.
    pub fn new_end_label(&mut self) -> Label {
        let label = Label(self.labels.len());
        let chunk_instrs = self.chunk().instructions.len();
        self.labels.insert(label, Some((self.chunk, chunk_instrs)));
        label
    }

    /// Associates the given label with the end of the currently generated bytecode.
    ///
    /// This is useful for creating labels that point to the end of a loop, for example.
    pub fn set_end_label(&mut self, label: Label) {
        let chunk_instrs = self.chunk().instructions.len();
        self.labels.insert(label, Some((self.chunk, chunk_instrs)));
    }
}

/// Trait for types that can be compiled into bytecode [`Instruction`]s.
///
/// The compiler is responsible for converting a CalcScript abstract syntax tree into a bytecode
/// representation that can be executed by the
/// [`Vm`](https://docs.rs/cas-vm/latest/cas_vm/vm/struct.Vm.html). The available instructions are
/// defined in the [`InstructionKind`] `enum`.
pub trait Compile {
    /// Compiles the type into a sequence of [`Instruction`]s.
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error>;
}

impl<T: Compile> Compile for &T {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        (*self).compile(compiler)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cas_parser::parser::{ast::stmt::Stmt, Parser};

    /// Compile the given source code.
    fn compile(source: &str) -> Result<Compiler, Error> {
        let mut parser = Parser::new(source);
        let stmts = parser.try_parse_full_many::<Stmt>().unwrap();

        Compiler::compile_program(stmts)
    }

    #[test]
    fn function_declaration() {
        compile("f(x) = {
    g(x) = {
        h(x) = x
        h(x) % 2 == 0
    }
    x % 3 == 0 && g(x)
}

f(18)").unwrap();
    }

    #[test]
    fn scoping() {
        let err = compile("f() = j + 6
g() = {
    j = 10
    f()
}
g()").unwrap_err();

        // error is in the definition of `f`
        // variable `j` is defined in `g`, so `f` can only access it if `x` is passed as an
        // argument, or `j` is in a higher scope
        assert_eq!(err.spans[0], 6..7);
    }

    #[test]
    fn advanced_scoping() {
        // `{}` curly braces, `f(x) = ...` function declarations, `loop`, `while`, `for`, `sum`,
        // and `product` introduce new scopes
        compile("{ x = 25 }; x").unwrap_err();

        // but if a variable is already defined in a parent scope, it can be accessed in a child
        // scope
        compile("x = 25; { x *= 2 }; x").unwrap();

        // this wouldn't make sense if a scope was not created
        //
        // functions aren't called at declaration, so `y` wouldn't be initialized when `y` is
        // accessed
        compile("f(x) = y = 25; y").unwrap_err();

        compile("f(x) = { y = 25 }; y").unwrap_err();
        compile("loop { t = rand(); if t < 0.2 break t }; t").unwrap_err();
        compile("a = for i in 1..5 { t = i }; t").unwrap_err();

        // `loop` and `while` _do_ introduce new scopes, specifically to avoid the first test
        // below: if scopes were not introduced, `t` would be uninitialized after the loop
        // immediately breaks, causing an error when `t` is accessed outside the loop
        //
        // despite that real issue, in general, scopes aren't all that useful in loops, since the
        // loop body will usually need to have multiple statements. the user would have to use
        // curly braces `{}` to write multiple statements, and thus, introduce a new scope
        //
        // otherwise, the loop body would just be a single statement, and the scope would be
        compile("loop t = break 2; t").unwrap_err();
        compile("while true break t = 2; t").unwrap_err();

        compile("(sum n in 1..5 of n) > n").unwrap_err();
    }

    #[test]
    fn scoping_with_compiler_declared_variables() {
        compile("for n in 1..n then print(n)").unwrap_err();
        compile("n = 50; for n in 0..n then print(n)").unwrap();
        compile("for n in n..50 then print(n)").unwrap_err();
    }

    #[test]
    fn shadowing() {
        compile("pi = 5").unwrap_err();
        compile("f() = pi = 5").unwrap(); // implicit shadowing occurs in non-global scopes
    }

    #[test]
    fn no_override_builtin() {
        compile("i = 5").unwrap_err(); // i is a builtin constant
        compile("pi(x) = x").unwrap_err(); // pi is a builtin constant
        compile("sqrt = 3").unwrap_err(); // sqrt is a builtin function
        compile("ncr(a) = a").unwrap_err(); // ncr is a builtin function
    }

    #[test]
    fn define_and_call() {
        compile("f(x) = return x + 1/sqrt(x)
g(x, y) = f(x) + f(y)
g(2, 3)").unwrap();
    }

    #[test]
    fn refer_to_parent() {
        compile("f(x) = g(x) = h(x) = f(x)").unwrap();
        compile("f(x) = g(x) = h(x) = g(x)").unwrap();
        compile("f(x) = g(x) = h(x) = h(x)").unwrap();
    }

    #[test]
    fn derivative() {
        compile("f(x) = x^2; f'(2)").unwrap();
        // TODO: it is not possible to derivate `ncr` (due to 2 parameters), so this should be an
        // error
        // however, ever since higher order functions were supported, functions couldn't be checked
        // if they were valid for derivation until runtime
        // however again, it is possible to determine if a function reference is referring to a
        // builtin function, so technically we have enough information to determine if this is
        // derivable at compile time. maybe we should do that
        compile("ncr''(5, 3)").unwrap();
    }

    #[test]
    fn list_index() {
        compile("arr = [1, 2, 3]
arr[0] = 5
arr[0] + arr[1] + arr[2] == 10").unwrap();
    }
}
