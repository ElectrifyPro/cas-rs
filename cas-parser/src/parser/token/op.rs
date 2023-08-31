//! Structs to help parse binary and unary operators.

use crate::{
    parser::{error::{Error, kind}, Parse, Parser},
    tokenizer::TokenKind,
};
use std::ops::Range;

/// The associativity of a binary or unary operation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    /// The binary / unary operation is left-associative.
    ///
    /// For binary operations, this means `a op b op c` is evaluated as `(a op b) op c`. For unary
    /// operations, this means `a op op` is evaluated as `(a op) op` (the operators appear to the
    /// right of the operand).
    Left,

    /// The binary / unary operation is right-associative.
    ///
    /// For binary operations, this means `a op b op c` is evaluated as `a op (b op c)`. For unary
    /// operations, this means `op op a` is evaluated as `op (op a)` (the operators appear to the
    /// left of the operand).
    Right,
}

/// The precedence of an operation, in order from lowest precedence (evaluated last) to highest
/// precedence (evaluated first).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Precedence {
    /// Any precedence.
    Any,

    /// Precedence of assignment (`=`).
    Assign,

    /// Precedence of logical or (`or`).
    Or,

    /// Precedence of logical and (`and`).
    And,

    /// Precedence of comparisons (`>`, `>=`, `<`, `<=`, `==`, `!=`, `~==`, and `~!=`).
    Compare,

    /// Precedence of bitwise or (`|`).
    BitOr,

    /// Precedence of bitwise and (`&`).
    BitAnd,

    /// Precedence of bitshifts (`<<` and `>>`).
    Shift,

    /// Precedence of addition (`+`) and subtraction (`-`), which separate terms.
    Term,

    /// Precedence of multiplication (`*`), division (`/`), and modulo (`%`), which separate
    /// factors.
    Factor,

    /// Precedence of unary subtraction (`-`).
    Neg,

    /// Precedence of exponentiation (`^`).
    Exp,

    /// Precedence of factorial (`!`).
    Factorial,

    /// Precedence of bitwise not (`~`).
    BitNot,

    /// Precedence of logical not (`not`).
    Not,
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let left = *self as u8;
        let right = *other as u8;
        left.partial_cmp(&right)
    }
}

/// The unary operation that is being performed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOpKind {
    Not,
    BitNot,
    Factorial,
    Neg,
}

impl UnaryOpKind {
    /// Returns the precedence of the unary operation.
    pub fn precedence(&self) -> Precedence {
        match self {
            Self::Not => Precedence::Not,
            Self::BitNot => Precedence::BitNot,
            Self::Factorial => Precedence::Factorial,
            Self::Neg => Precedence::Neg,
        }
    }

    /// Returns the associativity of the unary operation.
    pub fn associativity(&self) -> Associativity {
        match self {
            Self::Neg | Self::BitNot | Self::Not => Associativity::Right,
            Self::Factorial => Associativity::Left,
        }
    }
}

/// A unary operator that takes one operand.
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
    /// The kind of unary operator.
    pub kind: UnaryOpKind,

    /// The region of the source code that this operator was parsed from.
    pub span: Range<usize>,
}

impl UnaryOp {
    /// Returns the precedence of the unary operator.
    pub fn precedence(&self) -> Precedence {
        self.kind.precedence()
    }

    /// Returns the associativity of the unary operator.
    pub fn associativity(&self) -> Associativity {
        self.kind.associativity()
    }
}

impl<'source> Parse<'source> for UnaryOp {
    fn parse(input: &mut Parser<'source>) -> Result<Self, Error> {
        let token = input.next_token()?;
        let kind = match token.kind {
            TokenKind::Not => Ok(UnaryOpKind::Not),
            TokenKind::BitNot => Ok(UnaryOpKind::BitNot),
            TokenKind::Factorial => Ok(UnaryOpKind::Factorial),
            TokenKind::Sub => Ok(UnaryOpKind::Neg),
            _ => Err(Error::new(vec![token.span.clone()], kind::UnexpectedToken {
                expected: &[
                    TokenKind::Not,
                    TokenKind::Factorial,
                    TokenKind::Sub,
                ],
                found: token.kind,
            })),
        }?;

        Ok(Self {
            kind,
            span: token.span,
        })
    }
}

/// The binary operation that is being performed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOpKind {
    Exp,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    BitRight,
    BitLeft,
    BitAnd,
    BitOr,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Eq,
    NotEq,
    ApproxEq,
    ApproxNotEq,
    And,
    Or,
}

impl BinOpKind {
    /// Returns the precedence of the binary operation.
    pub fn precedence(&self) -> Precedence {
        match self {
            Self::Exp => Precedence::Exp,
            Self::Mul | Self::Div | Self::Mod => Precedence::Factor,
            Self::Add | Self::Sub => Precedence::Term,
            Self::BitRight | Self::BitLeft => Precedence::Shift,
            Self::BitAnd => Precedence::BitAnd,
            Self::BitOr => Precedence::BitOr,
            Self::Greater | Self::GreaterEq | Self::Less | Self::LessEq
                | Self::Eq | Self::NotEq | Self::ApproxEq | Self::ApproxNotEq => Precedence::Compare,
            Self::And => Precedence::And,
            Self::Or => Precedence::Or,
        }
    }

    /// Returns the associativity of the binary operation.
    pub fn associativity(&self) -> Associativity {
        match self {
            Self::Exp => Associativity::Right,
            Self::Mul | Self::Div | Self::Mod
                | Self::Add | Self::Sub
                | Self::BitRight | Self::BitLeft
                | Self::BitAnd | Self::BitOr
                | Self::Greater | Self::GreaterEq | Self::Less | Self::LessEq
                | Self::Eq | Self::NotEq | Self::ApproxEq | Self::ApproxNotEq
                | Self::And | Self::Or => Associativity::Left,
        }
    }
}

/// A binary operator that takes two operands.
#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    /// The kind of binary operator.
    pub kind: BinOpKind,

    /// Whether this binary operator was implicitly inserted by the parser.
    pub implicit: bool,

    /// The region of the source code that this operator was parsed from.
    pub span: Range<usize>,
}

impl BinOp {
    /// Returns the precedence of the binary operation.
    pub fn precedence(&self) -> Precedence {
        self.kind.precedence()
    }

    /// Returns the associativity of the binary operation.
    pub fn associativity(&self) -> Associativity {
        self.kind.associativity()
    }
}

impl<'source> Parse<'source> for BinOp {
    fn parse(input: &mut Parser<'source>) -> Result<Self, Error> {
        let token = input.next_token()?;
        let kind = match token.kind {
            TokenKind::Exp => Ok(BinOpKind::Exp),
            TokenKind::Mul => Ok(BinOpKind::Mul),
            TokenKind::Div => Ok(BinOpKind::Div),
            TokenKind::Mod => Ok(BinOpKind::Mod),
            TokenKind::Add => Ok(BinOpKind::Add),
            TokenKind::Sub => Ok(BinOpKind::Sub),
            TokenKind::BitRight => Ok(BinOpKind::BitRight),
            TokenKind::BitLeft => Ok(BinOpKind::BitLeft),
            TokenKind::BitAnd => Ok(BinOpKind::BitAnd),
            TokenKind::BitOr => Ok(BinOpKind::BitOr),
            TokenKind::Greater => Ok(BinOpKind::Greater),
            TokenKind::GreaterEq => Ok(BinOpKind::GreaterEq),
            TokenKind::Less => Ok(BinOpKind::Less),
            TokenKind::LessEq => Ok(BinOpKind::LessEq),
            TokenKind::Eq => Ok(BinOpKind::Eq),
            TokenKind::NotEq => Ok(BinOpKind::NotEq),
            TokenKind::ApproxEq => Ok(BinOpKind::ApproxEq),
            TokenKind::ApproxNotEq => Ok(BinOpKind::ApproxNotEq),
            TokenKind::And => Ok(BinOpKind::And),
            TokenKind::Or => Ok(BinOpKind::Or),
            _ => Err(Error::new(vec![token.span.clone()], kind::UnexpectedToken {
                expected: &[
                    TokenKind::Exp,
                    TokenKind::Mul,
                    TokenKind::Div,
                    TokenKind::Add,
                    TokenKind::Sub,
                ],
                found: token.kind,
            })),
        }?;

        Ok(Self {
            kind,
            implicit: false,
            span: token.span,
        })
    }
}
