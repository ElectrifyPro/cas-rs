use std::ops::Range;
use crate::{
    parser::{
        assign::Assign,
        binary::Binary,
        call::Call,
        error::{kind, Error},
        literal::Literal,
        paren::Paren,
        token::CloseParen,
        unary::Unary,
        Parse,
        Parser,
        Precedence,
    },
    try_parse_catch_fatal,
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

    /// A function call, such as `abs(-1)`.
    Call(Call),

    /// A unary operation, such as `-1` or `!true`.
    Unary(Unary),

    /// A binary operation, such as `1 + 2`.
    Binary(Binary),

    /// An assignment of a variable or function, such as `x = 1` or `f(x) = x^2`.
    Assign(Assign),
}

impl Expr {
    /// Returns the span of the expression.
    pub fn span(&self) -> Range<usize> {
        match self {
            Expr::Literal(literal) => literal.span(),
            Expr::Paren(paren) => paren.span(),
            Expr::Call(call) => call.span(),
            Expr::Unary(unary) => unary.span(),
            Expr::Binary(binary) => binary.span(),
            Expr::Assign(assign) => assign.span(),
        }
    }
}

impl<'source> Parse<'source> for Expr {
    fn parse(input: &mut Parser<'source>) -> Result<Self, Error> {
        if input.clone().try_parse::<CloseParen>().is_ok() {
            return Err(input.error_fatal(kind::UnclosedParenthesis { opening: false }));
        }

        let _ = try_parse_catch_fatal!(input.try_parse::<Assign>().map(|assign| Self::Assign(assign)));
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

    /// A function call, such as `abs(-1)`.
    Call(Call),
}

impl Primary {
    /// Returns the span of the primary expression.
    pub fn span(&self) -> Range<usize> {
        match self {
            Primary::Literal(literal) => literal.span(),
            Primary::Paren(paren) => paren.span(),
            Primary::Call(call) => call.span(),
        }
    }
}

impl<'source> Parse<'source> for Primary {
    fn parse(input: &mut Parser<'source>) -> Result<Self, Error> {
        // function calls can overlap with literals, so we need to try parsing a function call
        // first
        let _ = try_parse_catch_fatal!(input.try_parse::<Call>().map(|call| Self::Call(call)));
        let _ = try_parse_catch_fatal!(input.try_parse::<Literal>().map(|literal| Self::Literal(literal)));

        input.try_parse::<Paren>().map(|paren| Self::Paren(paren))
    }
}

impl From<Primary> for Expr {
    fn from(primary: Primary) -> Self {
        match primary {
            Primary::Literal(literal) => Self::Literal(literal),
            Primary::Paren(paren) => Self::Paren(paren),
            Primary::Call(call) => Self::Call(call),
        }
    }
}
