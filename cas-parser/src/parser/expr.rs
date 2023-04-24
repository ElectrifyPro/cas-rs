use super::{error::Error, literal::Literal, Parse, Parser};

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

    // TODO
}

impl Parse for Expr {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let num = input.try_parse::<Literal>()?;
        Ok(Self::Literal(num))
    }
}
