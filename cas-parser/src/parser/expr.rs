use super::{
    error::Error,
    literal::Literal,
    unary::Unary,
    Parse,
    Parser,
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

    /// A unary operation, such as `-1` or `!true`.
    Unary(Box<Unary>),
}

impl Parse for Expr {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        if let Ok(unary) = input.try_parse() {
            Ok(Self::Unary(Box::new(unary)))
        } else if let Ok(literal) = input.try_parse() {
            Ok(Self::Literal(literal))
        } else {
            Err(input.eof())
        }
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
}

impl Parse for Primary {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let literal = input.try_parse::<Literal>()?;
        Ok(Self::Literal(literal))
    }
}

impl From<Primary> for Expr {
    fn from(primary: Primary) -> Self {
        match primary {
            Primary::Literal(literal) => Self::Literal(literal),
        }
    }
}
