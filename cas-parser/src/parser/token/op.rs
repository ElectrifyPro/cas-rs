//! Structs to help parse binary and unary operators.

use crate::{
    parser::{
        error::{Error, kind},
        Associativity,
        Parse,
        Parser,
        Precedence,
    },
    tokenizer::TokenKind,
};

/// The unary operation that is being performed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Not,
    BitNot,
    Factorial,
    Neg,
}

impl UnaryOp {
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

impl Parse for UnaryOp {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let token = input.next_token()?;

        match token.kind {
            TokenKind::Not => Ok(Self::Not),
            TokenKind::BitNot => Ok(Self::BitNot),
            TokenKind::Factorial => Ok(Self::Factorial),
            TokenKind::Sub => Ok(Self::Neg),
            _ => Err(Error::new(token.span, kind::UnexpectedToken {
                expected: &[
                    TokenKind::Not,
                    TokenKind::Factorial,
                    TokenKind::Sub,
                ],
                found: token.kind,
            })),
        }
    }
}

/// The binary operation that is being performed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Exp,
    Mul,
    Div,
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

impl BinOp {
    /// Returns the precedence of the binary operation.
    pub fn precedence(&self) -> Precedence {
        match self {
            Self::Exp => Precedence::Exp,
            Self::Mul | Self::Div => Precedence::Factor,
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
            Self::Mul | Self::Div
                | Self::Add | Self::Sub
                | Self::BitRight | Self::BitLeft
                | Self::BitAnd | Self::BitOr
                | Self::Greater | Self::GreaterEq | Self::Less | Self::LessEq
                | Self::Eq | Self::NotEq | Self::ApproxEq | Self::ApproxNotEq
                | Self::And | Self::Or => Associativity::Left,
        }
    }
}

impl Parse for BinOp {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let token = input.next_token()?;

        match token.kind {
            TokenKind::Exp => Ok(Self::Exp),
            TokenKind::Mul => Ok(Self::Mul),
            TokenKind::Div => Ok(Self::Div),
            TokenKind::Add => Ok(Self::Add),
            TokenKind::Sub => Ok(Self::Sub),
            TokenKind::BitRight => Ok(Self::BitRight),
            TokenKind::BitLeft => Ok(Self::BitLeft),
            TokenKind::BitAnd => Ok(Self::BitAnd),
            TokenKind::BitOr => Ok(Self::BitOr),
            TokenKind::Greater => Ok(Self::Greater),
            TokenKind::GreaterEq => Ok(Self::GreaterEq),
            TokenKind::Less => Ok(Self::Less),
            TokenKind::LessEq => Ok(Self::LessEq),
            TokenKind::Eq => Ok(Self::Eq),
            TokenKind::NotEq => Ok(Self::NotEq),
            TokenKind::ApproxEq => Ok(Self::ApproxEq),
            TokenKind::ApproxNotEq => Ok(Self::ApproxNotEq),
            TokenKind::And => Ok(Self::And),
            TokenKind::Or => Ok(Self::Or),
            _ => Err(Error::new(token.span, kind::UnexpectedToken {
                expected: &[
                    TokenKind::Exp,
                    TokenKind::Mul,
                    TokenKind::Div,
                    TokenKind::Add,
                    TokenKind::Sub,
                ],
                found: token.kind,
            })),
        }
    }
}
