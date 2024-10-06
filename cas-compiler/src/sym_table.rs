use crate::item::{Item, Symbol, SymbolDecl};
use std::collections::{HashMap, HashSet};

/// An identifier used to distinguish between different scopes where variables are defined.
///
/// A new [`ScopeId`] is created for each new scope that is entered. A new scope is created in one
/// of the following situations:
///
/// - A curly brace `{}` block is entered.
/// - A function definition `f(x) = ...` is entered.
/// - A `loop`, `while`, `sum`, or `product` loop is entered.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(usize);

/// A scope that holds a set of symbols.
#[derive(Debug, Clone)]
pub struct Scope {
    /// The unique identifier for this scope.
    id: ScopeId,

    /// The table of symbol declarations.
    symbols: HashMap<String, Item>,

    /// Set of symbols used in this scope that were declared in a parent scope.
    ///
    /// This is important for functions. Functions can still be used long after they are declared;
    /// however, the variables they use may no longer be in scope. To ensure that the function can
    /// still access these variables, the symbols they use are captured and stored in this set to
    /// be used when the function is called.
    captures: HashSet<Symbol>,
}

impl Scope {
    /// Creates a new scope with the given unique identifier.
    pub fn new(id: ScopeId) -> Self {
        Self {
            id,
            symbols: HashMap::new(),
            captures: HashSet::new(),
        }
    }

    /// Returns the unique identifier for this scope.
    pub fn id(&self) -> ScopeId {
        self.id
    }

    /// Returns the set of symbols captured by this scope.
    pub fn captures(&self) -> &HashSet<Symbol> {
        &self.captures
    }

    /// Inserts a symbol into this scope.
    pub fn insert(&mut self, name: String, item: Item) {
        self.symbols.insert(name, item);
    }

    /// Resolves the item with the given name if it exists in this scope.
    pub fn resolve_item(&self, name: &str) -> Option<&Item> {
        self.symbols.get(name)
    }

    /// Adds a symbol to the set of captures for this scope.
    pub fn add_capture(&mut self, symbol: Symbol) {
        self.captures.insert(symbol);
    }
}

/// A symbol table that maps identifiers to information about the values they represent.
///
/// This is used to store information about variables and functions that are defined in the
/// program.
#[derive(Debug, Clone)]
pub struct SymbolTable {
    /// The next unique identifier to assign to a symbol.
    next_id: usize,

    /// The table of symbol declarations.
    symbols: HashMap<ScopeId, Scope>,

    /// A stack of scopes that are currently active.
    ///
    /// When traversing the AST, scopes are pushed onto this stack upon entering a new scope, and
    /// popped off when leaving the scope. Once popped, the scope is added to the `symbols` table.
    active_scopes: Vec<Scope>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self {
            next_id: 1,
            symbols: HashMap::new(),
            active_scopes: vec![Scope::new(ScopeId(0))], // global scope
        }
    }
}

impl SymbolTable {
    /// Creates a new symbol table with an initial global scope.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns true if the symbol table is in the global scope.
    pub fn is_global_scope(&self) -> bool {
        self.active_scopes.len() == 1
    }

    /// Returns the next unique identifier to assign to a symbol.
    pub fn next_id(&self) -> ScopeId {
        ScopeId(self.next_id)
    }

    /// Creates a new scope and makes it the active scope.
    pub fn enter_scope(&mut self) {
        let id = ScopeId(self.next_id);
        self.next_id += 1;

        let scope = Scope::new(id);
        self.active_scopes.push(scope);
    }

    /// Exits the current scope and makes the parent scope the active scope.
    ///
    /// If the current scope has no symbols declared in it when it is exited, it will not be added
    /// to the symbol table.
    pub fn exit_scope(&mut self) {
        let scope = self.active_scopes.pop().expect("no scope to exit");
        if !scope.symbols.is_empty() {
            self.symbols.insert(scope.id(), scope);
        }
    }

    /// Exits the current scope, makes the parent scope the active scope, then returns a reference
    /// to the current scope that was exited. This is useful for finding the scope's captured
    /// symbols.
    ///
    /// The scope is always added to the symbol table, even if it has no symbols declared in it.
    pub(crate) fn exit_scope_get(&mut self) -> &Scope {
        let scope = self.active_scopes.pop().expect("no scope to exit");
        self.symbols.entry(scope.id()).or_insert(scope)
    }

    /// Returns a reference to the active scope.
    pub fn active_scope(&self) -> &Scope {
        self.active_scopes
            .last()
            .expect("no active scope")
    }

    /// Returns a mutable reference to the active scope.
    pub fn active_scope_mut(&mut self) -> &mut Scope {
        self.active_scopes
            .last_mut()
            .expect("no active scope")
    }

    /// Inserts a symbol into the current scope.
    pub fn insert(&mut self, name: String, item: Item) {
        self.active_scope_mut().insert(name, item);
    }

    /// Resolves the item with the given name if it exists in the current scope.
    pub fn resolve_item(&self, name: &str) -> Option<&Item> {
        self.active_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.resolve_item(name))
    }

    /// Resolves the item with the given name if it exists in the current scope.
    ///
    /// If the item was declared in a parent scope, it is added to the set of captures for the
    /// current scope.
    pub fn resolve_item_mark_capture(&mut self, name: &str) -> Option<Symbol> {
        let (last, rest) = self.active_scopes.split_last_mut()?;
        if let Some(item) = last.resolve_item(name) {
            return Some(Symbol::User(item.id()));
        }

        // go in reverse order to find the nearest parent scope that contains the item
        rest.iter_mut().rev().find_map(|scope| {
            scope.resolve_item(name).map(|item| {
                last.add_capture(Symbol::User(item.id()));
                Symbol::User(item.id())
            })
        })
    }

    /// Resolves an [`Item::Symbol`] with the given name if it exists in the current scope.
    pub fn resolve_symbol(&self, name: &str) -> Option<SymbolDecl> {
        self.resolve_item(name)
            .and_then(|item| match item {
                Item::Symbol(decl) => Some(*decl),
                _ => None,
            })
    }
}
