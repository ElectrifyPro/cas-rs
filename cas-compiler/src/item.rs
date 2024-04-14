use std::collections::HashMap;

/// An item declaration in the program.
#[derive(Clone, Debug)]
pub enum Item {
    /// A symbol declaration.
    Symbol(SymbolDecl),

    /// A function declaration.
    Func(FuncDecl),
}

/// A symbol declaration.
#[derive(Clone, Debug)]
pub struct SymbolDecl {
    /// A unique identifier for the symbol.
    pub id: usize,
}

/// A function declaration.
#[derive(Clone, Debug)]
pub struct FuncDecl {
    /// The chunk containing the function body.
    pub chunk: usize,

    /// Symbol table for symbols declared inside this function.
    pub symbols: HashMap<String, Item>,
}

/// An identifier for a symbol, user-defined or builtin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbol {
    /// A user-defined symbol. The inner value represents the index of the symbol in the symbol
    /// table.
    User(usize),

    /// A builtin symbol. The inner value is the name of the symbol.
    Builtin(&'static str),
}

impl Symbol {
    /// Returns [`Ok`] with the index of the symbol if it is a user-defined symbol, or [`Err`] with
    /// the name of the symbol if it is a builtin symbol.
    pub fn index(&self) -> Result<usize, &'static str> {
        match self {
            Self::User(index) => Ok(*index),
            Self::Builtin(name) => Err(name),
        }
    }
}

/// An identifier for a function, user-defined or builtin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Func {
    /// A user-defined function. The inner value represents the index of the chunk containing the
    /// function body.
    User(usize),

    /// A builtin function. The inner value is a tuple containing the name of the function and the
    /// number of arguments it takes.
    Builtin(&'static str, usize),
}
