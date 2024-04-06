use std::collections::HashMap;

/// An item declaration in the program.
#[derive(Clone, Debug)]
pub enum Item {
    /// A variable declaration.
    Var(Var),

    /// A function declaration.
    Func(Func),
}

/// A variable declaration.
#[derive(Clone, Debug)]
pub struct Var {
    /// The name of the variable.
    pub name: String,

    /// The scopes in which this variable is accessible.
    pub scopes: Vec<Item>,
}

/// A function declaration.
#[derive(Clone, Debug)]
pub struct Func {
    /// The chunk containing the function body.
    pub chunk: usize,

    /// Symbol table for symbols declared inside this function.
    pub symbols: HashMap<String, Item>,
}
