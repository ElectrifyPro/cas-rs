use cas_compute::numerical::builtin::Builtin;
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
    /// The unique identifier for the symbol.
    pub id: usize,
}

/// A function declaration.
#[derive(Clone, Debug)]
pub struct FuncDecl {
    /// The index of the chunk containing the function body.
    pub chunk: usize,

    /// The arity of the function.
    pub arity: usize,

    /// Symbol table for items declared inside this function.
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

/// An identifier for a function call to a user-defined or builtin function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Func {
    /// A call to a user-defined function.
    User(UserCall),

    /// A call to a builtin function.
    Builtin(BuiltinCall),
}

impl Func {
    /// Returns the arity / number of parameters of the function, including optional arguments.
    pub fn arity(&self) -> usize {
        match self {
            Self::User(call) => call.arity,
            Self::Builtin(call) => call.builtin.num_args(),
        }
    }
}

/// An identifier to a call to a user-defined function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UserCall {
    /// The index of the chunk containing the function body.
    pub chunk: usize,

    /// The arity of the function.
    pub arity: usize,

    /// The number of arguments passed to the function in the call.
    pub num_given: usize,
}

/// An identifier to a call to a builtin function.
#[derive(Clone, Debug)]
pub struct BuiltinCall {
    /// The builtin function.
    pub builtin: &'static dyn Builtin,

    /// The number of arguments passed to the function in the call.
    pub num_given: usize,
}

/// Manual implementation of [`PartialEq`] to support `dyn Builtin` by comparing pointers.
impl PartialEq for BuiltinCall {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.builtin, other.builtin) && self.num_given == other.num_given
    }
}

/// Manual implementation of [`Eq`] to support `dyn Builtin` by comparing pointers.
impl Eq for BuiltinCall {}
