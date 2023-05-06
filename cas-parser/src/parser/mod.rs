pub mod error;
pub mod expr;
pub mod literal;
pub mod token;

use error::{Error, ErrorKind};
use super::tokenizer::{tokenize_complete, Token};

/// A high-level parser for the language. This is the type to use to parse an arbitrary piece of
/// code into an abstract syntax tree.
pub struct Parser<'source> {
    /// The tokens that this parser is currently parsing.
    tokens: Box<[Token<'source>]>,

    /// The index of the next token to be parsed.
    cursor: usize,
}

impl<'source> Parser<'source> {
    /// Create a new parser for the given source.
    pub fn new(source: &'source str) -> Self {
        Self {
            tokens: tokenize_complete(source).unwrap(),
            cursor: 0,
        }
    }

    /// Creates an error that points at the current token. Panics if the current token is out of
    /// bounds.
    pub fn error(&self, kind: ErrorKind) -> Error {
        let token = &self.tokens[self.cursor];
        Error::new(token.span.clone(), kind)
    }

    /// Returns the next token to be parsed, or an EOF error if there are no more tokens.
    pub fn next_token(&mut self) -> Result<Token<'source>, Error> {
        if self.cursor < self.tokens.len() {
            // cloning is cheap: only Range<_> is cloned
            let token = self.tokens[self.cursor].clone();
            self.cursor += 1;
            Ok(token)
        } else {
            Err(self.error(ErrorKind::Eof))
        }
    }

    /// Speculatively parses a value from the given stream of tokens. This function should be used
    /// in the [`Parse::parse`] implementation of a type with the given [`Parser`].
    ///
    /// If parsing is successful, the stream is advanced past the consumed tokens and the parsed
    /// value is returned. Otherwise, the stream is left unchanged and an error is returned.
    pub fn try_parse<T: Parse>(&mut self) -> Result<T, Error> {
        let start = self.cursor;
        match T::parse(self) {
            Ok(value) => Ok(value),
            err => {
                self.cursor = start;
                err
            },
        }
    }
}

/// Any type that can be parsed from a source of tokens.
pub trait Parse: Sized {
    /// Parses a value from the given stream of tokens, advancing the stream past the consumed
    /// tokens if parsing is successful.
    fn parse(input: &mut Parser) -> Result<Self, Error>;
}

#[cfg(test)]
mod tests {
    use super::*;

    use expr::Expr;
    use literal::{Literal, LitNum};

    #[test]
    fn literal_int() {
        let mut parser = Parser::new("16");
        let expr = parser.try_parse::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Number(LitNum {
            value: 16.0,
            span: 0..2,
        })));
    }

    #[test]
    fn literal_float() {
        let mut parser = Parser::new("3.14");
        let expr = parser.try_parse::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Number(LitNum {
            value: 3.14,
            span: 0..4,
        })));
    }
}
