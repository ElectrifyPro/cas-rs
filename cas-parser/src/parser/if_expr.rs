use std::ops::Range;
use super::{
    error::{kind, Error},
    expr::{Expr, Primary},
    token::Keyword,
    Parse,
    Parser,
    ParseResult,
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// An `if` expression, such as `if true 1 else 2`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct If {
    /// The condition of the `if` expression.
    pub condition: Box<Expr>,

    /// The expression to evaluate if the condition is true.
    pub then_expr: Box<Expr>,

    /// The expression to evaluate if the condition is false.
    pub else_expr: Box<Expr>,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,

    /// The span of the `if` keyword.
    pub if_span: Range<usize>,

    /// The span of the `then` keyword.
    pub then_span: Range<usize>,

    /// The span of the `else` keyword.
    pub else_span: Range<usize>,
}

impl If {
    /// Returns the span of the `if` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for If {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let if_token = input.try_parse_then::<Keyword, _>(|name, input| {
            if name.lexeme == "if" {
                ParseResult::Ok(())
            } else {
                ParseResult::Unrecoverable(vec![input.error(kind::NonFatal)])
            }
        }).forward_errors(recoverable_errors)?;
        let condition = input.try_parse::<Primary>().forward_errors(recoverable_errors)?;
        let then_token = input.try_parse_then::<Keyword, _>(|name, input| {
            if name.lexeme == "then" {
                ParseResult::Ok(())
            } else {
                ParseResult::Unrecoverable(vec![input.error(kind::NonFatal)])
            }
        }).forward_errors(recoverable_errors)?;
        let then_expr = input.try_parse::<Primary>().forward_errors(recoverable_errors)?;
        let else_token = input.try_parse_then::<Keyword, _>(|name, input| {
            if name.lexeme == "else" {
                ParseResult::Ok(())
            } else {
                ParseResult::Unrecoverable(vec![input.error(kind::NonFatal)])
            }
        }).forward_errors(recoverable_errors)?;
        let else_expr = input.try_parse::<Primary>().forward_errors(recoverable_errors)?;
        let span = if_token.span.start..else_expr.span().end;

        Ok(Self {
            condition: Box::new(condition.into()),
            then_expr: Box::new(then_expr.into()),
            else_expr: Box::new(else_expr.into()),
            span,
            if_span: if_token.span,
            then_span: then_token.span,
            else_span: else_token.span,
        })
    }
}
