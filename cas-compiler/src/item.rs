use std::collections::HashMap;

/// An item declaration in the program.
#[derive(Clone, Debug)]
pub enum Item {
    /// A symbol declaration.
    Symbol(Symbol),

    /// A function declaration.
    Func(Func),
}

/// A symbol declaration.
#[derive(Clone, Debug)]
pub struct Symbol {
    /// A unique identifier for the symbol.
    pub id: usize,
}

/// A function declaration.
#[derive(Clone, Debug)]
pub struct Func {
    /// The chunk containing the function body.
    pub chunk: usize,

    /// Symbol table for symbols declared inside this function.
    pub symbols: HashMap<String, Item>,
}
