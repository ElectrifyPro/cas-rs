use cas_compute::numerical::builtin::{Builtin, ParamKind};
use cas_parser::parser::ast::{Call, Param as ParserParam};
use crate::error::{kind, Error};
use std::{cmp::Ordering, collections::HashMap};

enum Signature<'a> {
    Builtin(&'static dyn Builtin),
    Parser(&'a [ParserParam]),
}

impl Signature<'_> {
    /// Returns the indices of the required parameters that were not provided by the user in the
    /// call.
    ///
    /// This indicates an error in the user's code.
    pub fn missing_args(&self, num_given: usize) -> Vec<usize> {
        match self {
            Self::Builtin(builtin) => {
                // NOTE: we will later enforce default arguments to be at the end of the signature
                // so we can safely assume that all required arguments are at the beginning
                let idx_of_first_default = builtin.sig()
                    .iter()
                    .position(|param| param.kind == ParamKind::Optional);
                // the missing arguments are the required arguments before the first default
                // argument, and after the number of arguments given
                (0..idx_of_first_default.unwrap_or(builtin.sig().len()))
                    .filter(|&i| i >= num_given)
                    .collect()
            },
            Self::Parser(params) => {
                let idx_of_first_default = params.iter()
                    .position(|param| match param {
                        ParserParam::Symbol(_) => false,
                        ParserParam::Default(..) => true,
                    });
                (0..idx_of_first_default.unwrap_or(params.len()))
                    .filter(|&i| i >= num_given)
                    .collect()
            },
        }
    }

    fn signature(&self) -> String {
        match self {
            Self::Builtin(builtin) => builtin.sig_str().to_owned(),
            Self::Parser(params) => params.iter()
                .map(|param| match param {
                    ParserParam::Symbol(name) => format!("{}", name),
                    ParserParam::Default(name, value) => format!("{} = {}", name, value),
                })
                .collect::<Vec<_>>()
                .join(", "),
        }
    }
}

/// Given a function call to a function with the given signature, checks if the call matches the
/// signature in argument count.
///
/// Returns [`Ok`] if the call matches the signature, or [`Err`] otherwise.
fn check_call(sig: Signature<'_>, sig_len: usize, call: &Call) -> Result<(), Error> {
    match call.args.len().cmp(&sig_len) {
        Ordering::Greater => {
            // add span of extraneous arguments
            let mut spans = call.outer_span().to_vec();
            spans.push(call.arg_span(sig_len..call.args.len() - 1));
            Err(Error::new(
                spans,
                kind::TooManyArguments {
                    name: call.name.name.to_string(),
                    expected: sig_len,
                    given: call.args.len(),
                    signature: sig.signature(),
                },
            ))
        },
        Ordering::Equal => Ok(()),
        Ordering::Less => {
            let indices = sig.missing_args(call.args.len());
            if indices.is_empty() {
                Ok(())
            } else {
                Err(Error::new(
                    call.outer_span().to_vec(),
                    kind::MissingArgument {
                        name: call.name.name.to_string(),
                        indices,
                        expected: sig_len,
                        given: call.args.len(),
                        signature: sig.signature(),
                    },
                ))
            }
        },
    }
}

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

    /// The function signature.
    pub signature: Vec<ParserParam>,

    /// Symbol table for items declared inside this function.
    pub symbols: HashMap<String, Item>,
}

impl FuncDecl {
    /// Checks if this function call matches the signature of its target function in argmuent
    /// count.
    ///
    /// Returns [`Ok`] if the call matches the signature, or [`Err`] otherwise.
    pub fn check_call(&self, call: &Call) -> Result<(), Error> {
        check_call(
            Signature::Parser(&*self.signature),
            self.signature.len(),
            call
        )
    }
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

/// An identifier for a function call to a [`UserCall`] or [`BuiltinCall`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Func {
    /// A call to a user-defined function.
    User(UserCall),

    /// A call to a builtin function.
    Builtin(BuiltinCall),
}

impl Func {
    /// Returns the number of arguments in the function signature.
    pub fn arity(&self) -> usize {
        match self {
            Self::User(user) => user.signature.len(),
            Self::Builtin(builtin) => builtin.builtin.sig().len(),
        }
    }

    /// Checks if this function call matches the signature of its target function in argmuent
    /// count.
    ///
    /// Returns [`Ok`] if the call matches the signature, or [`Err`] otherwise.
    pub fn check_call(&self, call: &Call) -> Result<(), Error> {
        match self {
            Self::User(user) => check_call(
                Signature::Parser(&*user.signature),
                user.signature.len(),
                call,
            ),
            Self::Builtin(builtin) => check_call(
                Signature::Builtin(builtin.builtin),
                builtin.builtin.sig().len(),
                call,
            ),
        }
    }

    /// Returns the number of default arguments being used in the call.
    ///
    /// Returns [`None`] if the function call has more arguments than the signature, which shuold
    /// cause a compilation error.
    pub fn num_defaults_used(&self) -> Option<usize> {
        match self {
            Self::User(call) => call.signature.len().checked_sub(call.num_given),
            Self::Builtin(call) => call.builtin.sig().len().checked_sub(call.num_given),
        }
    }
}

/// An identifier to a call to a user-defined function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UserCall {
    /// The index of the chunk containing the function body.
    pub chunk: usize,

    /// The function signature.
    pub signature: Vec<ParserParam>,

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
