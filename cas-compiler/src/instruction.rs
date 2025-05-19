use cas_compute::numerical::value::Value;
use cas_parser::parser::{ast::range::RangeKind, token::op::{BinOpKind, UnaryOpKind}};
use crate::{item::Symbol, Label};
use std::ops::Range;

/// Kinds of bytecode instructions emitted by the compiler.
///
/// Bytecode can be thought of as a significantly high-level assembly language. Each instruction
/// corresponds to a single operation that can be executed by the virtual machine, which itself can
/// be thought of as a simple CPU.
///
/// During compilation, the compiler will associate most instructions with spans of the source code
/// that they originated from. This is meant for debugging and to report high quality error
/// messages for the user during runtime. The `spans` field of the [`Instruction`] struct contains
/// this information.
///
/// See the `Span information` section of each variant for how to interpret the spans.
#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    /// Initializes key VM registers for a call to a user-defined function.
    ///
    /// This is a special instruction used in user function argument validation, and is compiled in
    /// as the first instruction of a user function.
    ///
    /// The fields of this instruction are:
    ///
    /// - `fn_name`: The name of the function being executed.
    /// - `fn_signature`: The signature of the function being executed.
    /// - `num_params`: The total number of parameters in the function signature.
    /// - `num_default_params`: The number of default parameters in the function signature.
    InitFunc(String, String, usize, usize),

    /// Checks if there are enough arguments on the stack to start executing the user function
    /// body, or throws a runtime error if there are too many arguments.
    ///
    /// This is a special instruction used in user function argument validation, and is compiled in
    /// just before compiling a default argument expression. The specific order of operations is:
    ///
    /// - Compare the number of currently available arguments (set in the argument counter) to the
    /// number of arguments expected by the function.
    /// - If they are equal, the function is ready to be executed. The VM will then assign the
    /// given values to its internal variables representing the function parameters and execute the
    /// function body, jumping around as needed.
    /// - If there are more arguments than expected, an error is thrown.
    /// - Otherwise, fall-through to the default argument expression.
    /// - The default argument expression is compiled.
    CheckExecReady,

    /// Increments the argument counter by 1.
    ///
    /// This is a special instruction used in user function argument validation, and is compiled in
    /// after each default argument expression.
    NextArg,

    /// Checks if there are missing or extra arguments by the time the function body is reached,
    /// throwing a runtime error if so.
    ///
    /// This is a special instruction used in user function argument validation, and is compiled in
    /// after all default argument expressions and before the function body.
    ErrorIfMissingArgs,

    /// Load a constant value (one known at compile time) onto the stack.
    LoadConst(Value),

    /// Create a new list with the specified number of elements pulled from the stack.
    CreateList(usize),

    /// Create a new list by repeating the top value on the stack `count` times.
    ///
    /// The value to repeat and the count are specified by the second-to-top and the top values on
    /// the stack, respectively.
    CreateListRepeat,

    /// Create a new range with the start and end values on the stack.
    CreateRange(RangeKind),

    /// Load a value stored in a variable onto the stack.
    LoadVar(Symbol),

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

    /// Store the top value on the stack in the list at the index. The list and index are then both
    /// removed from the stack, while the value **is retained**.
    ///
    /// The value, list, and index are specified by the third-to-top, second-to-top, and top values
    /// on the stack, respectively.
    StoreIndexed,

    /// Load the value at the specified index in the list onto the stack. The list and index are
    /// then both removed from the stack.
    ///
    /// The list and index are specified by the second-to-top and the top values on the stack,
    /// respectively.
    LoadIndexed,

    /// Drops the top value from the stack.
    Drop,

    /// Performs the binary operation on the second-to-top and top stack values.
    Binary(BinOpKind),

    /// Performs the unary operation on the top stack value.
    Unary(UnaryOpKind),

    /// Call the function at the top of the value stack, passing the specified number of arguments.
    ///
    /// Arguments are passed to the function via the value stack. The function will be popped from
    /// the stack first, followed by the arguments in reverse order.
    ///
    /// # Span information
    ///
    /// ```rust,ignore
    /// [
    ///     0: outer_span[0], // span including the function name to the opening parenthesis
    ///     1: outer_span[1], // closing parenthesis
    ///     2: arg1,
    ///     3: arg2,
    ///     ...
    /// ]
    /// ```
    Call(usize),

    /// Computes the `n`th numerical derivative of the function at the top of the stack.
    CallDerivative(u8),

    /// Returns from the current function.
    Return,

    /// Jump to the specified label.
    Jump(Label),

    /// Jumps to the specified label if the top value on the stack is the boolean `true`.
    ///
    /// This will result in an error if the top value is not a boolean.
    JumpIfTrue(Label),

    /// Jumps to the specified label if the top value on the stack is the boolean `false`.
    ///
    /// This will result in an error if the top value is not a boolean.
    JumpIfFalse(Label),
}

/// Represents a single instruction in the bytecode, along with its associated metadata, such as
/// source code spans.
#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    /// The kind of instruction.
    pub kind: InstructionKind,

    /// The span(s) of the source code that this instruction originated from.
    pub spans: Vec<Range<usize>>,
}

impl From<InstructionKind> for Instruction {
    fn from(kind: InstructionKind) -> Self {
        Self { kind, spans: Vec::new() }
    }
}
