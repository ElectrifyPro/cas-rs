pub mod expr;
pub mod error;
pub mod instruction;
pub mod item;

use cas_compute::funcs::all;
use cas_parser::parser::ast::{FuncHeader, LitSym, Stmt};
use error::{kind, Error};
use std::collections::HashMap;
use expr::compile_stmts;
pub use instruction::Instruction;
use item::{Func, FuncDecl, Item, Symbol};

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

    /// Whether the expression being parsed is a top-level assignment expression, indicating that
    /// its return value is not used.
    ///
    /// This is used to determine whether to use the [`Instruction::StoreVar`] or
    /// [`Instruction::AssignVar`] instruction.
    ///
    /// For example, in the following code:
    ///
    /// ```calc
    /// x = y = 2
    /// ```
    ///
    /// The `x = ...` expression is an top-level assignment expression, so its return value is not
    /// used within the same statement. However, the `y = 2` expression is not a top-level
    /// assignment, as its return value is then passed to the `x = ...` expression. In this case,
    /// the compiler will generate a [`Instruction::AssignVar`] instruction for `x` and a
    /// [`Instruction::StoreVar`] instruction for `y`.
    pub top_level_assign: bool,

    /// Path representing the current scope.
    ///
    /// A new scope is introduced inside each function definition (TODO: block scopes). When this
    /// is empty, we are currently in the global scope.
    pub path: Vec<String>,
}

/// A chunk containing a function definition.
#[derive(Debug, Default)]
pub struct Chunk {
    /// The instructions in this chunk.
    pub instructions: Vec<Instruction>,
}

/// A compiler that provides tools to generate bytecode instructions for a virtual machine (see
/// [`Vm`]).
#[derive(Debug)]
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
    pub symbols: HashMap<String, Item>,

    /// Index of the current chunk.
    ///
    /// This value is manually updated by the compiler.
    chunk: usize,

    /// Next unique identifier for a symbol.
    next_symbol_id: usize,

    /// Holds state for the current loop.
    state: CompilerState,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            chunks: vec![Chunk::default()], // add main chunk
            labels: Default::default(),
            symbols: Default::default(),
            chunk: 0,
            next_symbol_id: 0,
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

    /// Add a symbol to the symbol table.
    pub fn add_symbol(&mut self, name: &str, item: Item) {
        let mut table = &mut self.symbols;
        let (last_path, path) = self.state.path.as_slice().split_last().unwrap();
        for component in path.iter() {
            if table.contains_key(component) {
                let Item::Func(func) = table.get_mut(component).unwrap() else {
                    panic!("oops");
                };
                table = &mut func.symbols;
            } else {
                panic!("oops");
            }
        }

        // add to this final table
        table.insert(name.to_string(), item);
    }

    /// Creates a new chunk and a scope for compilation. All methods that edit instructions will do
    /// so to the new chunk.
    pub fn new_chunk<F>(&mut self, header: &FuncHeader, f: F) -> Result<(), Error>
        where F: FnOnce(&mut Compiler) -> Result<(), Error>
    {
        let old_chunk_idx = self.chunk;
        self.state.path.push(header.name.name.to_string());
        self.chunks.push(Chunk::default());
        let new_chunk_idx = self.chunks.len() - 1;

        self.add_symbol(&header.name.name, Item::Func(FuncDecl {
            chunk: new_chunk_idx,
            symbols: HashMap::new(),
        }));

        self.chunk = new_chunk_idx;
        f(self)?;
        self.state.path.pop().unwrap();
        self.chunk = old_chunk_idx;

        Ok(())
    }

    /// Resolves a path to a symbol, inserting it into the symbol table if it doesn't exist.
    ///
    /// Returns the unique identifier for the symbol, which can be used to reference the symbol in
    /// the bytecode.
    pub fn resolve_symbol_or_insert(&mut self, symbol: &LitSym) -> usize {
        let mut result = None;

        let mut table = &mut self.symbols;

        // is the symbol in the global scope?
        if let Some(Item::Symbol(symbol)) = table.get(&symbol.name) {
            result = Some(symbol.id);
        }

        // work our way up to the current scope
        for component in self.state.path.iter() {
            // is the symbol in this scope?
            if let Some(Item::Symbol(symbol)) = table.get(&symbol.name) {
                result = Some(symbol.id);
            }

            // let's check the next scope
            if table.contains_key(component) {
                let Item::Func(func) = table.get_mut(component).unwrap() else {
                    panic!("oops");
                };
                table = &mut func.symbols;
            } else {
                panic!("oops");
            }
        }

        if let Some(Item::Symbol(symbol)) = table.get(&symbol.name) {
            // is the symbol in the current scope?
            symbol.id
        } else if let Some(symbol) = result {
            // use the last one we found
            symbol
        } else {
            // if not, insert it
            let id = self.next_symbol_id;
            table.insert(symbol.name.to_string(), Item::Symbol(Symbol { id }));
            self.next_symbol_id += 1;
            id
        }
    }

    /// Resolves a path to a symbol without inserting it into the symbol table.
    ///
    /// Returns the unique identifier for the symbol, or an error if the symbol is not found within
    /// the current scope.
    pub fn resolve_symbol(&self, symbol: &LitSym) -> Result<usize, Error> {
        let mut result = None;

        let mut table = &self.symbols;

        // is the symbol in the global scope?
        if let Some(Item::Symbol(symbol)) = table.get(&symbol.name) {
            result = Some(symbol.id);
        }

        // work our way up to the current scope
        for component in self.state.path.iter() {
            // is the symbol in this scope?
            if let Some(Item::Symbol(symbol)) = table.get(&symbol.name) {
                result = Some(symbol.id);
            }

            // let's check the next scope
            if table.contains_key(component) {
                let Item::Func(func) = table.get(component).unwrap() else {
                    panic!("oops");
                };
                table = &func.symbols;
            } else {
                panic!("oops");
            }
        }

        if let Some(Item::Symbol(symbol)) = table.get(&symbol.name) {
            // is the symbol in the current scope?
            Ok(symbol.id)
        } else if let Some(symbol) = result {
            // use the last one we found
            Ok(symbol)
        } else {
            Err(Error::new(vec![symbol.span.clone()], kind::UnknownVariable {
                name: symbol.name.clone(),
            }))
        }
    }

    /// Resolves a path to a function.
    ///
    /// Returns the index of the chunk containing the function.
    pub fn resolve_function(&self, name: &str) -> Result<Func, Error> {
        let mut result = None;

        let mut table = &self.symbols;

        // is the function in the global scope?
        if let Some(Item::Func(func)) = table.get(name) {
            result = Some(func.chunk);
            table = &func.symbols;
        }

        // work our way up to the current scope
        // if we find items with the same name, replace result
        for component in self.state.path.iter() {
            if table.contains_key(component) {
                let Item::Func(func) = table.get(component).unwrap() else {
                    panic!("oops");
                };
                // result = Some(func.chunk); // TODO what??
                table = &func.symbols;
            } else {
                break;
            }
        }

        if let Some(Item::Func(func)) = table.get(name) {
            // is the function in the current scope?
            Ok(Func::UserFunc(func.chunk))
        } else if let Some(chunk) = result {
            // use the last one we found
            Ok(Func::UserFunc(chunk))
        } else {
            // is there a native function with this name?
            all()
                .get(name)
                .map(|builtin| Func::Builtin(builtin.name(), builtin.num_args()))
                .ok_or_else(|| Error::new(vec![], kind::UnknownFunction {
                    name: name.to_string(),
                    suggestions: Vec::new(), // TODO
                }))
        }
    }

    /// Adds an instruction to the current chunk.
    pub fn add_instr(&mut self, instruction: Instruction) {
        let chunk = self.chunk_mut();
        chunk.instructions.push(instruction);
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
/// representation that can be executed by the [`Vm`](crate::vm::Vm). The available instructions
/// are defined in the [`Instruction`](crate::instruction::Instruction) enum.
///
/// The compiler is implemented as a visitor over the AST, and is responsible for generating the
/// bytecode instructions that correspond to the AST nodes. The compiler is implemented as a
/// struct that implements the [`Visitor`](crate::visitor::Visitor) trait.
pub trait Compile {
    /// Compiles the type into a sequence of [`Instruction`]s.
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error>;
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
    g(x) = x % 2 == 0
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
    fn define_and_call() {
        compile("f(x) = return x + 1/sqrt(x)
g(x, y) = f(x) + f(y)
g(2, 3)").unwrap();
    }
}
