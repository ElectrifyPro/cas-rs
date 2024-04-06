use cas_compute::numerical::value::Value;
use cas_parser::parser::token::op::{BinOpKind, UnaryOpKind};
use crate::Label;

/// Bytecode instructions emitted by the compiler.
///
/// Bytecode can be thought of as a significantly high-level assembly language. Each instruction
/// corresponds to a single operation that can be executed by the virtual machine, which itself can
/// be thought of as a simple CPU.
///
/// Our bytecode contains more high-level instructions than a typical assembly language, which makes
/// it easier to generate and execute. In particular, we aren't as limited by the number of registers
/// available to us, and we can use a stack-based model for our virtual machine.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Load a constant value (one known at compile time) onto the stack.
    LoadConst(Value),

    /// Load a value stored in a variable onto the stack.
    LoadVar(usize),

    /// Store the top value on the stack in the current stack frame. This value is **not** removed
    /// from the stack.
    ///
    /// This behavior is important since assignment expressions can be used as subexpressions in
    /// larger expressions.
    StoreVar(usize),

    /// Store the top value on the stack in the current stack frame. This value **is** removed from
    /// the stack.
    ///
    /// This is used for assignment statements, where the value being assigned is not used in any
    /// further expressions.
    AssignVar(usize),

    /// Drops the top value from the stack.
    ///
    /// No error is returned if the stack is empty.
    Drop,

    /// Performs the binary operation on the second-to-top and top stack values.
    Binary(BinOpKind),

    /// Performs the unary operation on the top stack value.
    Unary(UnaryOpKind),

    /// Calls the function at the given chunk.
    ///
    /// Arguments are passed to the function via the value stack.
    Call(usize),

    /// Returns from the current function.
    Return,

    /// Jump to the specified label.
    Jump(Label),

    /// Jumps to the specified label if the top value on the stack is the boolean `false`.
    ///
    /// This will result in an error if the top value is not a boolean.
    JumpIfFalse(Label),

    /// Output the top value on the stack.
    Output,
}
