use std::collections::HashMap;

/// An item declaration in the program.
#[derive(Clone, Debug)]
pub enum Item {
    /// A symbol declaration.
    Symbol(Symbol),

    /// A function declaration.
    Func(FuncDecl),
}

/// A symbol declaration.
#[derive(Clone, Debug)]
pub struct Symbol {
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

/// An identifier for a function, user-defined or builtin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Func {
    /// A user-defined function. The inner value represents the index of the chunk containing the
    /// function body.
    UserFunc(usize),

    /// A builtin function. The inner value is a tuple containing the name of the function and the
    /// number of arguments it takes.
    Builtin(&'static str, usize),
}
