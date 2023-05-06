//! Structs to help parse binary and unary operators.

use crate::{
    parser::{
        error::{Error, ErrorKind},
        Associativity,
        Parse,
        Parser,
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
            _ => Err(Error::new(token.span, ErrorKind::UnexpectedToken {
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
