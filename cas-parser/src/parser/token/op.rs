//! Structs to help parse binary and unary operators.

use cas_error::Error;
use crate::{
    parser::{error::UnexpectedToken, fmt::Latex, Parse, Parser},
    tokenizer::TokenKind,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// The associativity of a binary or unary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Precedence {
    /// Any precedence.
    Any,

    /// Precedence of assignment (`=`, `+=`, `-=`, `*=`, `/=`, `%=`, `^=`, `&&=`, `||=`, `&=`,
    /// `|=`, `>>=`, and `<<=`).
    Assign,

    /// Precedence of range (`..` and `..=`).
    Range,

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
        Some(self.cmp(other))
    }
}

impl Ord for Precedence {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let left = *self as u8;
        let right = *other as u8;
        left.cmp(&right)
    }
}

/// The unary operation that is being performed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
    fn std_parse(
        input: &mut Parser<'source>,
        _: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let token = input.next_token().map_err(|e| vec![e])?;
        let kind = match token.kind {
            TokenKind::Not => Ok(UnaryOpKind::Not),
            TokenKind::BitNot => Ok(UnaryOpKind::BitNot),
            TokenKind::Factorial => Ok(UnaryOpKind::Factorial),
            TokenKind::Sub => Ok(UnaryOpKind::Neg),
            _ => Err(vec![Error::new(
                vec![token.span.clone()],
                UnexpectedToken {
                    expected: &[
                        TokenKind::Not,
                        TokenKind::BitNot,
                        TokenKind::Factorial,
                        TokenKind::Sub,
                    ],
                    found: token.kind,
                },
            )]),
        }?;

        Ok(Self {
            kind,
            span: token.span,
        })
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            UnaryOpKind::Not => write!(f, "not"),
            UnaryOpKind::BitNot => write!(f, "~"),
            UnaryOpKind::Factorial => write!(f, "!"),
            UnaryOpKind::Neg => write!(f, "-"),
        }
    }
}

impl Latex for UnaryOp {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            UnaryOpKind::Not => write!(f, "\\neg "),
            UnaryOpKind::BitNot => write!(f, "\\sim "),
            UnaryOpKind::Factorial => write!(f, "!"),
            UnaryOpKind::Neg => write!(f, "-"),
        }
    }
}

/// The binary operation that is being performed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
    fn std_parse(
        input: &mut Parser<'source>,
        _: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let token = input.next_token().map_err(|e| vec![e])?;
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
            _ => Err(vec![Error::new(
                vec![token.span.clone()],
                UnexpectedToken {
                    expected: &[
                        TokenKind::Exp,
                        TokenKind::Mul,
                        TokenKind::Div,
                        TokenKind::Add,
                        TokenKind::Sub,
                    ],
                    found: token.kind,
                },
            )]),
        }?;

        Ok(Self {
            kind,
            implicit: false,
            span: token.span,
        })
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.implicit {
            return Ok(());
        }

        match self.kind {
            BinOpKind::Exp => write!(f, "^"),
            BinOpKind::Mul => write!(f, "*"),
            BinOpKind::Div => write!(f, "/"),
            BinOpKind::Mod => write!(f, "%"),
            BinOpKind::Add => write!(f, "+"),
            BinOpKind::Sub => write!(f, "-"),
            BinOpKind::BitRight => write!(f, ">>"),
            BinOpKind::BitLeft => write!(f, "<<"),
            BinOpKind::BitAnd => write!(f, "&"),
            BinOpKind::BitOr => write!(f, "|"),
            BinOpKind::Greater => write!(f, ">"),
            BinOpKind::GreaterEq => write!(f, ">="),
            BinOpKind::Less => write!(f, "<"),
            BinOpKind::LessEq => write!(f, "<="),
            BinOpKind::Eq => write!(f, "=="),
            BinOpKind::NotEq => write!(f, "!="),
            BinOpKind::ApproxEq => write!(f, "~=="),
            BinOpKind::ApproxNotEq => write!(f, "~!="),
            BinOpKind::And => write!(f, "&&"),
            BinOpKind::Or => write!(f, "||"),
        }
    }
}

impl Latex for BinOp {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.implicit {
            return Ok(());
        }

        match self.kind {
            BinOpKind::Exp => write!(f, "^"),
            BinOpKind::Mul => write!(f, "\\cdot "),
            BinOpKind::Div => write!(f, "\\div "),
            BinOpKind::Mod => write!(f, "\\mod "),
            BinOpKind::Add => write!(f, "+"),
            BinOpKind::Sub => write!(f, "-"),
            BinOpKind::BitRight => write!(f, "\\gg "),
            BinOpKind::BitLeft => write!(f, "\\ll "),
            BinOpKind::BitAnd => write!(f, "\\&"),
            BinOpKind::BitOr => write!(f, "\\vert "),
            BinOpKind::Greater => write!(f, ">"),
            BinOpKind::GreaterEq => write!(f, "\\geq "),
            BinOpKind::Less => write!(f, "<"),
            BinOpKind::LessEq => write!(f, "\\leq "),
            BinOpKind::Eq => write!(f, "="),
            BinOpKind::NotEq => write!(f, "\\neq "),
            BinOpKind::ApproxEq => write!(f, "\\approx "),
            BinOpKind::ApproxNotEq => write!(f, "\\not\\approx "),
            BinOpKind::And => write!(f, "\\wedge "),
            BinOpKind::Or => write!(f, "\\vee "),
        }
    }
}

/// The kind of assignment operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum AssignOpKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    And,
    Or,
    BitAnd,
    BitOr,
    BitRight,
    BitLeft,
}

impl From<AssignOpKind> for BinOpKind {
    fn from(value: AssignOpKind) -> Self {
        match value {
            AssignOpKind::Assign => Self::Eq,
            AssignOpKind::Add => Self::Add,
            AssignOpKind::Sub => Self::Sub,
            AssignOpKind::Mul => Self::Mul,
            AssignOpKind::Div => Self::Div,
            AssignOpKind::Mod => Self::Mod,
            AssignOpKind::Exp => Self::Exp,
            AssignOpKind::And => Self::And,
            AssignOpKind::Or => Self::Or,
            AssignOpKind::BitAnd => Self::BitAnd,
            AssignOpKind::BitOr => Self::BitOr,
            AssignOpKind::BitRight => Self::BitRight,
            AssignOpKind::BitLeft => Self::BitLeft,
        }
    }
}

/// An assignment operator that takes two operands, assigning the value of the right operand to the
/// left operand, possibly with an intermediate operation.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct AssignOp {
    /// The kind of assignment operator.
    pub kind: AssignOpKind,

    /// The region of the source code that this operator was parsed from.
    pub span: Range<usize>,
}

impl AssignOp {
    /// Returns true if this assignment operator is the standard assignment operator (i.e. `=`).
    pub fn is_standard(&self) -> bool {
        matches!(self.kind, AssignOpKind::Assign)
    }

    /// Returns true if this assignment operator is a compound assignment operator.
    pub fn is_compound(&self) -> bool {
        !self.is_standard()
    }
}

impl<'source> Parse<'source> for AssignOp {
    fn std_parse(
        input: &mut Parser<'source>,
        _: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let token = input.next_token().map_err(|e| vec![e])?;
        let kind = match token.kind {
            TokenKind::Assign => Ok(AssignOpKind::Assign),
            TokenKind::AddAssign => Ok(AssignOpKind::Add),
            TokenKind::SubAssign => Ok(AssignOpKind::Sub),
            TokenKind::MulAssign => Ok(AssignOpKind::Mul),
            TokenKind::DivAssign => Ok(AssignOpKind::Div),
            TokenKind::ModAssign => Ok(AssignOpKind::Mod),
            TokenKind::ExpAssign => Ok(AssignOpKind::Exp),
            TokenKind::AndAssign => Ok(AssignOpKind::And),
            TokenKind::OrAssign => Ok(AssignOpKind::Or),
            TokenKind::BitAndAssign => Ok(AssignOpKind::BitAnd),
            TokenKind::BitOrAssign => Ok(AssignOpKind::BitOr),
            TokenKind::BitRightAssign => Ok(AssignOpKind::BitRight),
            TokenKind::BitLeftAssign => Ok(AssignOpKind::BitLeft),
            _ => Err(vec![Error::new(
                vec![token.span.clone()],
                UnexpectedToken {
                    expected: &[
                        TokenKind::Assign,
                        TokenKind::AddAssign,
                        TokenKind::SubAssign,
                        TokenKind::MulAssign,
                        TokenKind::DivAssign,
                        TokenKind::ModAssign,
                        TokenKind::ExpAssign,
                        TokenKind::AndAssign,
                        TokenKind::OrAssign,
                        TokenKind::BitAndAssign,
                        TokenKind::BitOrAssign,
                        TokenKind::BitRightAssign,
                        TokenKind::BitLeftAssign,
                    ],
                    found: token.kind,
                },
            )]),
        }?;

        Ok(Self {
            kind,
            span: token.span,
        })
    }
}

impl std::fmt::Display for AssignOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            AssignOpKind::Assign => write!(f, "="),
            AssignOpKind::Add => write!(f, "+="),
            AssignOpKind::Sub => write!(f, "-="),
            AssignOpKind::Mul => write!(f, "*="),
            AssignOpKind::Div => write!(f, "/="),
            AssignOpKind::Mod => write!(f, "%="),
            AssignOpKind::Exp => write!(f, "^="),
            AssignOpKind::And => write!(f, "&&="),
            AssignOpKind::Or => write!(f, "||="),
            AssignOpKind::BitAnd => write!(f, "&="),
            AssignOpKind::BitOr => write!(f, "|="),
            AssignOpKind::BitRight => write!(f, ">>="),
            AssignOpKind::BitLeft => write!(f, "<<="),
        }
    }
}

impl Latex for AssignOp {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            AssignOpKind::Assign => write!(f, "="),
            AssignOpKind::Add => write!(f, "+="),
            AssignOpKind::Sub => write!(f, "-="),
            AssignOpKind::Mul => write!(f, "*="),
            AssignOpKind::Div => write!(f, "/="),
            AssignOpKind::Mod => write!(f, "\\mod="),
            AssignOpKind::Exp => write!(f, "^="),
            AssignOpKind::And => write!(f, "\\wedge="),
            AssignOpKind::Or => write!(f, "\\vee="),
            AssignOpKind::BitAnd => write!(f, "\\&="),
            AssignOpKind::BitOr => write!(f, "\\vert="),
            AssignOpKind::BitRight => write!(f, "\\gg="),
            AssignOpKind::BitLeft => write!(f, "\\ll="),
        }
    }
}
