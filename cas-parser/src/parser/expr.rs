use std::ops::Range;
use crate::{
    parser::{
        binary::Binary,
        error::{Error, ErrorKind},
        literal::Literal,
        paren::Paren,
        unary::Unary,
        Parse,
        Parser,
        Precedence,
    },
    tokenizer::TokenKind,
};

/// Represents a general expression in CalcScript.
///
/// An expression is any valid piece of code that can be evaluated to produce a value. Expressions
/// can be used as the right-hand side of an assignment, or as the argument to a function call.
///
/// In CalcBot, the `c-calculate` command accepts an expression as its argument.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A literal value.
    Literal(Literal),

    /// A parenthesized expression, such as `(1 + 2)`.
    Paren(Paren),

    /// A unary operation, such as `-1` or `!true`.
    Unary(Unary),

    /// A binary operation, such as `1 + 2`.
    Binary(Binary),
}

impl Expr {
    /// Returns the span of the expression.
    pub fn span(&self) -> Range<usize> {
        match self {
            Expr::Literal(literal) => literal.span(),
            Expr::Paren(paren) => paren.span(),
            Expr::Unary(unary) => unary.span(),
            Expr::Binary(binary) => binary.span(),
        }
    }
}

impl Parse for Expr {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let lhs = input.try_parse_with_fn(Unary::parse_or_lower)?;
        Binary::parse_expr(input, lhs, Precedence::Any)
    }
}

/// Represents a primary expression in CalcScript.
///
/// Primary expressions are the simplest expressions, and are the building blocks of more complex
/// expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
    /// A literal value.
    Literal(Literal),

    /// A parenthesized expression, such as `(1 + 2)`.
    Paren(Paren),
}

impl Parse for Primary {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        if let Ok(literal) = input.try_parse() {
            Ok(Self::Literal(literal))
        } else if let Ok(paren) = input.try_parse() {
            Ok(Self::Paren(paren))
        } else {
            match input.current_token() {
                Some(token) => Err(input.error(ErrorKind::UnexpectedToken {
                    expected: &[
                        TokenKind::Int,
                        TokenKind::Float,
                        TokenKind::OpenParen,
                    ],
                    found: token.kind,
                })),
                None => Err(input.error(ErrorKind::UnexpectedEof)),
            }
        }
    }
}

impl From<Primary> for Expr {
    fn from(primary: Primary) -> Self {
        match primary {
            Primary::Literal(literal) => Self::Literal(literal),
            Primary::Paren(paren) => Self::Paren(paren),
        }
    }
}
