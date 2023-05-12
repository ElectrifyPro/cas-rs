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
    Factorial,
    Neg,
}

impl UnaryOp {
    /// Returns the precedence of the unary operation.
    pub fn precedence(&self) -> Precedence {
        match self {
            Self::Not => Precedence::Not,
            Self::Factorial => Precedence::Factorial,
            Self::Neg => Precedence::Neg,
        }
    }

    /// Returns the associativity of the unary operation.
    pub fn associativity(&self) -> Associativity {
        match self {
            Self::Neg | Self::Not => Associativity::Right,
            Self::Factorial => Associativity::Left,
        }
    }
}

impl Parse for UnaryOp {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let token = input.next_token()?;

        match token.kind {
            TokenKind::Not => Ok(Self::Not),
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
}

impl BinOp {
    /// Returns the precedence of the binary operation.
    pub fn precedence(&self) -> Precedence {
        match self {
            Self::Exp => Precedence::Exp,
            Self::Mul | Self::Div => Precedence::Factor,
            Self::Add | Self::Sub => Precedence::Term,
        }
    }

    /// Returns the associativity of the binary operation.
    pub fn associativity(&self) -> Associativity {
        match self {
            Self::Exp => Associativity::Right,
            Self::Mul | Self::Div | Self::Add | Self::Sub => Associativity::Left,
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
