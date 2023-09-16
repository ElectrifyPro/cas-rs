use cas_error::ErrorKind;
use cas_parser::parser::{assign::Assign, binary::Binary};
use crate::error::{kind::{BitshiftOverflow, InvalidBinaryOperation}, Error};
use std::ops::Range;

/// Trait implemented on [`Binary`] and [`Assign`] to extract the spans of the operands and the
/// operator.
pub trait BinaryLike {
    /// Get the spans of the operands and the operator.
    fn spans(&self) -> Vec<Range<usize>>;
}

impl BinaryLike for Binary {
    fn spans(&self) -> Vec<Range<usize>> {
        vec![self.lhs.span(), self.op.span.clone(), self.rhs.span()]
    }
}

impl BinaryLike for Assign {
    fn spans(&self) -> Vec<Range<usize>> {
        vec![self.target.span(), self.op.span.clone(), self.value.span()]
    }
}

/// Represents an error that can occur while evaluating an expression.
#[derive(Debug)]
pub enum EvalError {
    /// Attempted to apply an operator to incompatible operands.
    InvalidBinaryOperation(InvalidBinaryOperation),

    /// Attempted to bitshift by a value that is too large.
    BitshiftOverflow(BitshiftOverflow),
}

impl From<InvalidBinaryOperation> for EvalError {
    fn from(e: InvalidBinaryOperation) -> Self {
        EvalError::InvalidBinaryOperation(e)
    }
}

impl From<BitshiftOverflow> for EvalError {
    fn from(e: BitshiftOverflow) -> Self {
        EvalError::BitshiftOverflow(e)
    }
}

impl EvalError {
    /// Convert the [`EvalError`] into an [`Error`], using the given syntax tree to provide spans.
    pub fn into_error(self, binary: &dyn BinaryLike) -> Error {
        let spans = binary.spans();
        match self {
            EvalError::InvalidBinaryOperation(e) => Error {
                spans,
                kind: Box::new(e) as Box<dyn ErrorKind>,
            },
            EvalError::BitshiftOverflow(e) => Error {
                spans,
                kind: Box::new(e) as Box<dyn ErrorKind>,
            },
        }
    }
}
