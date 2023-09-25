use crate::parser::{error::Error, Parse, Parser};

/// Represents a value that is surrounded by two tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Surrounded<S, E, T> {
    /// The start token.
    pub start: S,

    /// The value.
    pub value: T,

    /// The end token.
    pub end: E,
}

impl<'source, S, E, T> Parse<'source> for Surrounded<S, E, T>
where
    S: Parse<'source>,
    E: Parse<'source>,
    T: Parse<'source>,
{
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        Ok(Self {
            start: input.try_parse().forward_errors(recoverable_errors)?,
            value: input.try_parse().forward_errors(recoverable_errors)?,
            end: input.try_parse().forward_errors(recoverable_errors)?,
        })
    }
}
