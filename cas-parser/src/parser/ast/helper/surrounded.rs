use cas_error::Error;
use crate::parser::{
    token::pair::Pair,
    Parse,
    Parser,
};
use std::marker::PhantomData;

/// Represents a value that is surrounded by two tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Surrounded<'source, P, T>
where
    P: Pair,
{
    /// The opening token that was parsed.
    pub open: P::Open<'source>,

    /// The value.
    pub value: T,

    /// The closing token that was parsed.
    pub close: P::Close<'source>,

    /// Marker type to allow using `P` as a type parameter.
    pair: PhantomData<P>,
}

impl<'source, P, T> Parse<'source> for Surrounded<'source, P, T>
where
    P: Parse<'source> + Pair,
    T: Parse<'source>,
{
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let open = input.try_parse().forward_errors(recoverable_errors)?;

        // clone the input so we can scan forward without affecting the original input
        let mut input_ahead = input.clone();

        // scan forward for the corresponding end token
        // if we don't find it, do not attempt to parse the inner value
        let mut depth = 1;
        while depth > 0 {
            let token = input_ahead.next_token()
                .map_err(|eof| vec![eof])?;

            if token.kind == P::OPEN {
                depth += 1;
            } else if token.kind == P::CLOSE {
                depth -= 1;
            }

            if depth == 0 {
                break;
            }
        }

        // exiting the loop means that there is indeed a corresponding end token
        let value = input.try_parse().forward_errors(recoverable_errors)?;

        // if this fails, there's probably extraneous tokens between the value and the end token
        let close = input.try_parse().forward_errors(recoverable_errors)?;

        Ok(Self { open, value, close, pair: PhantomData })
    }
}
