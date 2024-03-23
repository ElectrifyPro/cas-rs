use crate::parser::{error::Error, Parse, Parser};
use std::marker::PhantomData;

/// Represents zero or more values that are delimited by a certain token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Delimited<D, T> {
    /// The values.
    pub values: Vec<T>,

    /// Marker type to allow using `D` as a type parameter.
    delimiter: PhantomData<D>,
}

impl<'source, D, T> Parse<'source> for Delimited<D, T>
where
    D: Parse<'source> + std::fmt::Debug,
    T: Parse<'source> + std::fmt::Debug,
{
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let mut values = Vec::new();

        loop {
            // manually catch unrecoverable errors so we can parse zero values
            let Ok(value) = input.try_parse().forward_errors(recoverable_errors) else {
                break;
            };
            values.push(value);

            if input.try_parse::<D>().forward_errors(recoverable_errors).is_err() {
                break;
            }
        }

        Ok(Self { values, delimiter: PhantomData })
    }
}
