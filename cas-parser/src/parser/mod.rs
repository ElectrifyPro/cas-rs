pub mod ast;
pub mod error;
pub mod fmt;
pub mod garbage;
pub mod iter;
pub mod keyword;
pub mod token;

use cas_error::ErrorKind;
use error::{Error, kind};
use super::tokenizer::{tokenize_complete, Token};
use std::{ops::Range, rc::Rc};

/// A high-level parser for the language. This is the type to use to parse an arbitrary piece of
/// code into an abstract syntax tree.
#[derive(Debug, Clone)]
pub struct Parser<'source> {
    /// The tokens that this parser is currently parsing.
    tokens: Rc<[Token<'source>]>,

    /// The index of the **next** token to be parsed.
    cursor: usize,
}

impl<'source> Parser<'source> {
    /// Create a new parser for the given source.
    pub fn new(source: &'source str) -> Self {
        Self {
            tokens: tokenize_complete(source).into(),
            cursor: 0,
        }
    }

    /// Sets the parser to point to the same token as the given parser. It is assumed that both
    /// parsers point to the same source code.
    pub fn set_cursor(&mut self, other: &Self) {
        self.cursor = other.cursor;
    }

    /// Creates an error that points at the current token, or the end of the source code if the
    /// cursor is at the end of the stream.
    pub fn error(&self, kind: impl ErrorKind + 'static) -> Error {
        Error::new(vec![self.span()], kind)
    }

    /// Returns a span pointing at the end of the source code.
    pub fn eof_span(&self) -> Range<usize> {
        self.tokens.last().map_or(0..0, |token| token.span.end..token.span.end)
    }

    /// Returns the span of the current token, or the end of the source code if the cursor is at
    /// the end of the stream.
    pub fn span(&self) -> Range<usize> {
        self.tokens
            .get(self.cursor)
            .map_or(self.eof_span(), |token| token.span.clone())
    }

    /// Move the cursor to the previous token. This function is a no-op if the cursor is at the
    /// beginning of the stream.
    pub fn prev(&mut self) {
        if self.cursor > 0 {
            self.cursor -= 1;
        }
    }

    /// Returns the previous token. The cursor is not moved. Returns [`None`] if the cursor is at
    /// the beginning of the stream.
    pub fn prev_token(&self) -> Option<&Token<'source>> {
        self.tokens.get(self.cursor.checked_sub(1)?)
    }

    /// Returns the current token. The cursor is not moved. Returns [`None`] if the cursor is at
    /// the end of the stream.
    pub fn current_token(&self) -> Option<&Token<'source>> {
        self.tokens.get(self.cursor)
    }

    /// Advances the cursor past whitespace tokens to the next non-whitespace token. The cursor is
    /// not guaranteed to point to a valid token (might be out of bounds), but if the token is
    /// valid, it is guaranteed to be non-whitespace.
    pub fn advance_past_whitespace(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if token.is_whitespace() {
                self.cursor += 1;
                continue;
            } else {
                break;
            }
        }
    }

    /// Returns the current token, then advances the cursor. Whitespace tokens are skipped.
    ///
    /// Returns an EOF error if there are no more tokens.
    pub fn next_token(&mut self) -> Result<Token<'source>, Error> {
        self.advance_past_whitespace();
        self.next_token_raw()
    }

    /// Returns the current token, then advances the cursor. Whitespace tokens **are not** skipped.
    ///
    /// Returns an EOF error if there are no more tokens.
    pub fn next_token_raw(&mut self) -> Result<Token<'source>, Error> {
        let result = self.current_token()
            .cloned() // cloning is cheap; only Range<_> is cloned
            .ok_or_else(|| self.error(kind::UnexpectedEof));
        self.cursor += 1;
        result
    }

    /// Speculatively parses a value from the given stream of tokens. This function can be used
    /// in the [`Parse::parse`] implementation of a type with the given [`Parser`], as it will
    /// automatically backtrack the cursor position if parsing fails.
    ///
    /// If parsing is successful, the stream is advanced past the consumed tokens and the parsed
    /// value is returned. Otherwise, the stream is left unchanged and an error is returned.
    pub fn try_parse<T: Parse<'source>>(&mut self) -> ParseResult<T> {
        self.try_parse_with_fn(T::parse)
    }

    /// Speculatively parses a value from the given stream of tokens, using a custom parsing
    /// function to parse the value. This function can be used in the [`Parse::parse`]
    /// implementation of a type with the given [`Parser`], as it will automatically backtrack the
    /// cursor position if parsing fails.
    ///
    /// If parsing is successful, the stream is advanced past the consumed tokens and the parsed
    /// value is returned. Otherwise, the stream is left unchanged and an error is returned.
    pub fn try_parse_with_fn<T, F>(&mut self, f: F) -> ParseResult<T>
    where
        F: FnOnce(&mut Parser<'source>) -> ParseResult<T>,
    {
        let start = self.cursor;
        f(self).inspect_unrecoverable(|_| self.cursor = start)
    }

    /// Speculatively parses a value from the given stream of tokens, with a validation predicate.
    /// The value must parse successfully, **and** the predicate must return [`Ok`] for this
    /// function to return successfully.
    ///
    /// If parsing is successful, the stream is advanced past the consumed tokens and the parsed
    /// value is returned. Otherwise, the stream is left unchanged and an error is returned.
    pub fn try_parse_then<T: Parse<'source>, F>(&mut self, predicate: F) -> ParseResult<T>
    where
        F: FnOnce(&T, &Parser) -> ParseResult<()>,
    {
        let start = self.cursor;
        let mut errors = Vec::new();

        let inner = || {
            let value = T::parse(self).forward_errors(&mut errors)?;
            predicate(&value, self).forward_errors(&mut errors)?;
            Ok(value)
        };

        inner()
            .map(|value| (value, errors))
            .map_err(|unrecoverable: Vec<Error>| {
                self.cursor = start;
                unrecoverable
            })
            .into()
    }

    /// Attempts to parse a value from the given stream of tokens. All the tokens must be consumed
    /// by the parser; if not, an error is returned.
    pub fn try_parse_full<T: Parse<'source>>(&mut self) -> Result<T, Vec<Error>> {
        let mut errors = Vec::new();
        let value = T::parse(self).forward_errors(&mut errors)?;

        // consume whitespace
        self.advance_past_whitespace();

        if self.cursor < self.tokens.len() {
            errors.push(self.error(kind::ExpectedEof));
        }

        if errors.is_empty() {
            Ok(value)
        } else {
            Err(errors)
        }
    }

    /// Attempts to parse multiple values from the given stream of tokens. All the tokens must be
    /// consumed by the parser; if not, an error is returned.
    pub fn try_parse_full_many<T: std::fmt::Debug + Parse<'source>>(&mut self) -> Result<Vec<T>, Vec<Error>> {
        let mut errors = Vec::new();
        let mut values = Vec::new();

        while self.cursor < self.tokens.len() {
            let Ok(value) = T::parse(self).forward_errors(&mut errors) else {
                break;
            };
            values.push(value);
        }

        // consume whitespace
        self.advance_past_whitespace();

        if self.cursor < self.tokens.len() {
            errors.push(self.error(kind::ExpectedEof));
        }

        if errors.is_empty() {
            Ok(values)
        } else {
            Err(errors)
        }
    }
}

/// Any type that can be parsed from a source of tokens.
pub trait Parse<'source>: Sized {
    /// Parses a value from the given stream of tokens, advancing the stream past the consumed
    /// tokens if parsing is successful.
    ///
    /// If any recoverable errors are encountered, they should be appended to the given [`Vec`],
    /// and parsing should continue as normal. If an unrecoverable error is encountered, parsing
    /// should abort with said error.
    ///
    /// This function should be used by private functions in the library.
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>>;

    /// Parses a value from the given stream of tokens, advancing the stream past the consumed
    /// tokens if parsing is successful.
    ///
    /// This function should be used by users of this parser.
    fn parse(input: &mut Parser<'source>) -> ParseResult<Self> {
        let mut recoverable_errors = Vec::new();
        Self::std_parse(input, &mut recoverable_errors)
            .map(|value| (value, recoverable_errors))
            .into()
    }
}

/// The result of a parsing operation.
#[derive(Debug)]
pub enum ParseResult<T> {
    /// Parsing was successful.
    Ok(T),

    /// An error occurred while parsing, however the parser attempted to recover and has tried to
    /// continue parsing in order to find more errors. The recovered value is returned in addition
    /// to all the errors that were found.
    ///
    /// There are **no** guarantees on whether the recovered value is valid or not. Thus, it should
    /// only be used to allow the parser to continue parsing, and should not be shown to the user.
    Recoverable(T, Vec<Error>),

    /// An error occurred while parsing, and the parser was unable to recover. Parsing is aborted
    /// and the error are returned.
    Unrecoverable(Vec<Error>),
}

impl<T> From<Result<(T, Vec<Error>), Vec<Error>>> for ParseResult<T> {
    fn from(result: Result<(T, Vec<Error>), Vec<Error>>) -> Self {
        match result {
            Ok((value, errors)) => (value, errors).into(),
            Err(unrecoverable_errors) => Self::Unrecoverable(unrecoverable_errors),
        }
    }
}

impl<T> From<(T, Vec<Error>)> for ParseResult<T> {
    fn from((value, errors): (T, Vec<Error>)) -> Self {
        if errors.is_empty() {
            Self::Ok(value)
        } else {
            Self::Recoverable(value, errors)
        }
    }
}

impl<T> From<Vec<Error>> for ParseResult<T> {
    fn from(errors: Vec<Error>) -> Self {
        Self::Unrecoverable(errors)
    }
}

impl<T> ParseResult<T> {
    /// Converts the [`ParseResult<T>`] to a [`Result<T, Vec<Error>>`], using these rules:
    ///
    /// - [`ParseResult::Ok`] is converted to [`Ok`].
    /// - [`ParseResult::Recoverable`] is converted to [`Ok`]. The errors are appended to the given
    /// mutable [`Vec`].
    /// - [`ParseResult::Unrecoverable`] is converted to [`Err`].
    ///
    /// This can be a convenient way to allow utilizing the [`?`] operator in a parsing function,
    /// while still holding onto errors that were found for later reporting.
    pub fn forward_errors(self, errors: &mut Vec<Error>) -> Result<T, Vec<Error>> {
        match self {
            Self::Ok(value) => Ok(value),
            Self::Recoverable(value, mut errs) => {
                errors.append(&mut errs);
                Ok(value)
            },
            Self::Unrecoverable(errs) => Err(errs),
        }
    }

    /// Returns `true` if the result is [`ParseResult::Ok`].
    pub fn is_ok(&self) -> bool {
        matches!(self, ParseResult::Ok(_))
    }

    /// Calls the provided closure with a reference to the contained unrecoverable error.
    ///
    /// This is equivalent to [`Result::inspect_err`].
    pub fn inspect_unrecoverable<F>(self, f: F) -> Self
    where
        F: FnOnce(&Vec<Error>),
    {
        if let Self::Unrecoverable(errs) = &self {
            f(errs);
        }

        self
    }

    /// Maps a `ParseResult<T>` to `ParseResult<U>` by applying a function to a contained
    /// [`ParseResult::Ok`] or [`ParseResult::Recoverable`] value, leaving an
    /// [`ParseResult::Unrecoverable`] value untouched.
    ///
    /// This is equivalent to [`Result::map`].
    pub fn map<U, F>(self, f: F) -> ParseResult<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            ParseResult::Ok(value) => ParseResult::Ok(f(value)),
            ParseResult::Recoverable(value, errors) => ParseResult::Recoverable(f(value), errors),

            // this line cannot be simplified to `err => err`, because then both the left hand side
            // and right hand side are type `ParseResult<T>`!
            ParseResult::Unrecoverable(errors) => ParseResult::Unrecoverable(errors),
        }
    }

    /// Calls `op` if the result is an error, otherwise returns the `Ok` value of `self`.
    ///
    /// This is similar to [`Result::or_else`], but no error value is given to the closure.
    pub fn or_else<F>(self, op: F) -> Self
    where
        F: FnOnce() -> Self,
    {
        match self {
            ParseResult::Recoverable(..) | ParseResult::Unrecoverable(_) => op(),
            ok => ok,
        }
    }
}

/// Convenience macro to short-circuit from a parsing function if the given [`Result`] is [`Ok`],
/// or return the contained error if the result is [`Err`].
///
/// This is essentially the opposite of the [`try!`] macro.
#[macro_export]
macro_rules! return_if_ok {
    ($result:expr) => {
        match $result {
            Ok(value) => return Ok(value),
            Err(errors) => errors,
        }
    };
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use super::*;

    use ast::*;
    use token::op::{AssignOp, AssignOpKind, BinOp, BinOpKind, UnaryOp, UnaryOpKind};

    #[test]
    fn literal_int() {
        let mut parser = Parser::new("16");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Number(LitNum {
            value: "16".to_string(),
            span: 0..2,
        })));
    }

    #[test]
    fn literal_float() {
        let mut parser = Parser::new("3.14");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Number(LitNum {
            value: "3.14".to_string(),
            span: 0..4,
        })));
    }

    #[test]
    fn literal_radix_base_2() {
        let mut parser = Parser::new("2'10000110000");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Radix(LitRadix {
            base: 2,
            value: "10000110000".to_string(),
            span: 0..13,
        })));
    }

    #[test]
    fn literal_radix_base_8() {
        let mut parser = Parser::new("8'2060");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Radix(LitRadix {
            base: 8,
            value: "2060".to_string(),
            span: 0..6,
        })));
    }

    #[test]
    fn literal_radix_base_25() {
        let mut parser = Parser::new("25'1hm");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Radix(LitRadix {
            base: 25,
            value: "1hm".to_string(),
            span: 0..6,
        })));
    }

    #[test]
    fn literal_radix_base_32() {
        let mut parser = Parser::new("32'11g");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Radix(LitRadix {
            base: 32,
            value: "11g".to_string(),
            span: 0..6,
        })));
    }

    #[test]
    fn literal_radix_base_47() {
        let mut parser = Parser::new("47'mC");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Radix(LitRadix {
            base: 47,
            value: "mC".to_string(),
            span: 0..5,
        })));
    }

    #[test]
    fn literal_radix_with_nonword_tokens() {
        let mut parser = Parser::new("64'++/+//");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Radix(LitRadix {
            base: 64,
            value: "++/+//".to_string(),
            span: 0..9,
        })));
    }

    #[test]
    fn literal_symbol() {
        let mut parser = Parser::new("pi");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Literal(Literal::Symbol(LitSym {
            name: "pi".to_string(),
            span: 0..2,
        })));
    }

    #[test]
    fn unary_left_associativity() {
        let mut parser = Parser::new("3!!");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Unary(Unary {
            operand: Box::new(Expr::Unary(Unary {
                operand: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "3".to_string(),
                    span: 0..1,
                }))),
                op: UnaryOp {
                    kind: UnaryOpKind::Factorial,
                    span: 1..2,
                },
                span: 0..2,
            })),
            op: UnaryOp {
                kind: UnaryOpKind::Factorial,
                span: 2..3,
            },
            span: 0..3,
        }));
    }

    #[test]
    fn unary_right_associativity() {
        let mut parser = Parser::new("not not --3");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Unary(Unary {
            operand: Box::new(Expr::Unary(Unary {
                operand: Box::new(Expr::Unary(Unary {
                    operand: Box::new(Expr::Unary(Unary {
                        operand: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "3".to_string(),
                            span: 10..11,
                        }))),
                        op: UnaryOp {
                            kind: UnaryOpKind::Neg,
                            span: 9..10,
                        },
                        span: 9..11,
                    })),
                    op: UnaryOp {
                        kind: UnaryOpKind::Neg,
                        span: 8..9,
                    },
                    span: 8..11,
                })),
                op: UnaryOp {
                    kind: UnaryOpKind::Not,
                    span: 4..7,
                },
                span: 4..11,
            })),
            op: UnaryOp {
                kind: UnaryOpKind::Not,
                span: 0..3,
            },
            span: 0..11,
        }));
    }

    #[test]
    fn binary_left_associativity() {
        let mut parser = Parser::new("3 * x * 5");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "3".to_string(),
                    span: 0..1,
                }))),
                op: BinOp {
                    kind: BinOpKind::Mul,
                    implicit: false,
                    span: 2..3,
                },
                rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                    name: "x".to_string(),
                    span: 4..5,
                }))),
                span: 0..5,
            })),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: false,
                span: 6..7,
            },
            rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "5".to_string(),
                span: 8..9,
            }))),
            span: 0..9,
        }));
    }

    #[test]
    fn binary_left_associativity_mix_precedence() {
        let mut parser = Parser::new("3 + 4 * a + b");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "3".to_string(),
                    span: 0..1,
                }))),
                op: BinOp {
                    kind: BinOpKind::Add,
                    implicit: false,
                    span: 2..3,
                },
                rhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "4".to_string(),
                        span: 4..5,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Mul,
                        implicit: false,
                        span: 6..7,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                        name: "a".to_string(),
                        span: 8..9,
                    }))),
                    span: 4..9,
                })),
                span: 0..9,
            })),
            op: BinOp {
                kind: BinOpKind::Add,
                implicit: false,
                span: 10..11,
            },
            rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                name: "b".to_string(),
                span: 12..13,
            }))),
            span: 0..13,
        }));
    }

    #[test]
    fn binary_right_associativity() {
        let mut parser = Parser::new("1 ^ 2 ^ 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "1".to_string(),
                span: 0..1,
            }))),
            op: BinOp {
                kind: BinOpKind::Exp,
                implicit: false,
                span: 2..3,
            },
            rhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "2".to_string(),
                    span: 4..5,
                }))),
                op: BinOp {
                    kind: BinOpKind::Exp,
                    implicit: false,
                    span: 6..7,
                },
                rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "3".to_string(),
                    span: 8..9,
                }))),
                span: 4..9,
            })),
            span: 0..9,
        }));
    }

    #[test]
    fn binary_complicated() {
        let mut parser = Parser::new("1 + 2 * 3 - 4 / 5 ^ 6");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        // 2 * 3
        let mul = Expr::Binary(Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "2".to_string(),
                span: 4..5,
            }))),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: false,
                span: 6..7,
            },
            rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "3".to_string(),
                span: 8..9,
            }))),
            span: 4..9,
        });

        // 1 + 2 * 3
        let add = Expr::Binary(Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "1".to_string(),
                span: 0..1,
            }))),
            op: BinOp {
                kind: BinOpKind::Add,
                implicit: false,
                span: 2..3,
            },
            rhs: Box::new(mul),
            span: 0..9,
        });

        // 5 ^ 6
        let exp = Expr::Binary(Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "5".to_string(),
                span: 16..17,
            }))),
            op: BinOp {
                kind: BinOpKind::Exp,
                implicit: false,
                span: 18..19,
            },
            rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "6".to_string(),
                span: 20..21,
            }))),
            span: 16..21,
        });

        // 4 / 5 ^ 6
        let div = Expr::Binary(Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "4".to_string(),
                span: 12..13,
            }))),
            op: BinOp {
                kind: BinOpKind::Div,
                implicit: false,
                span: 14..15,
            },
            rhs: Box::new(exp),
            span: 12..21,
        });

        // 1 + 2 * 3 - 4 / 5 ^ 6
        let sub = Expr::Binary(Binary {
            lhs: Box::new(add),
            op: BinOp {
                kind: BinOpKind::Sub,
                implicit: false,
                span: 10..11,
            },
            rhs: Box::new(div),
            span: 0..21,
        });

        assert_eq!(expr, sub);
    }

    #[test]
    fn binary_and_unary() {
        let mut parser = Parser::new("-1 ^ -2 * 3");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Unary(Unary {
                operand: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "1".to_string(),
                        span: 1..2,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Exp,
                        implicit: false,
                        span: 3..4,
                    },
                    rhs: Box::new(Expr::Unary(Unary {
                        operand: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "2".to_string(),
                            span: 6..7,
                        }))),
                        op: UnaryOp {
                            kind: UnaryOpKind::Neg,
                            span: 5..6,
                        },
                        span: 5..7,
                    })),
                    span: 1..7,
                })),
                op: UnaryOp {
                    kind: UnaryOpKind::Neg,
                    span: 0..1,
                },
                span: 0..7,
            })),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: false,
                span: 8..9,
            },
            rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "3".to_string(),
                span: 10..11,
            }))),
            span: 0..11,
        }));
    }

    #[test]
    fn complicated_binary_and_unary() {
        let mut parser = Parser::new("pi^2 * 17! / -4.9 + e");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Binary(Binary {
                        lhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                            name: "pi".to_string(),
                            span: 0..2,
                        }))),
                        op: BinOp {
                            kind: BinOpKind::Exp,
                            implicit: false,
                            span: 2..3,
                        },
                        rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "2".to_string(),
                            span: 3..4,
                        }))),
                        span: 0..4,
                    })),
                    op: BinOp {
                        kind: BinOpKind::Mul,
                        implicit: false,
                        span: 5..6,
                    },
                    rhs: Box::new(Expr::Unary(Unary {
                        operand: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "17".to_string(),
                            span: 7..9,
                        }))),
                        op: UnaryOp {
                            kind: UnaryOpKind::Factorial,
                            span: 9..10,
                        },
                        span: 7..10,
                    })),
                    span: 0..10,
                })),
                op: BinOp {
                    kind: BinOpKind::Div,
                    implicit: false,
                    span: 11..12,
                },
                rhs: Box::new(Expr::Unary(Unary {
                    operand: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "4.9".to_string(),
                        span: 14..17,
                    }))),
                    op: UnaryOp {
                        kind: UnaryOpKind::Neg,
                        span: 13..14,
                    },
                    span: 13..17,
                })),
                span: 0..17,
            })),
            op: BinOp {
                kind: BinOpKind::Add,
                implicit: false,
                span: 18..19,
            },
            rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                name: "e".to_string(),
                span: 20..21,
            }))),
            span: 0..21,
        }));
    }

    #[test]
    fn implicit_multiplication() {
        let mut parser = Parser::new("2(3 + 4)");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "2".to_string(),
                span: 0..1,
            }))),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: true,
                span: 1..1,
            },
            rhs: Box::new(Expr::Paren(Paren {
                expr: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "3".to_string(),
                        span: 2..3,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Add,
                        implicit: false,
                        span: 4..5,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "4".to_string(),
                        span: 6..7,
                    }))),
                    span: 2..7,
                })),
                span: 1..8,
            })),
            span: 0..8,
        }));
    }

    #[test]
    fn implicit_multiplication_2() {
        let mut parser = Parser::new("1 + 2x");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "1".to_string(),
                span: 0..1,
            }))),
            op: BinOp {
                kind: BinOpKind::Add,
                implicit: false,
                span: 2..3,
            },
            rhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "2".to_string(),
                    span: 4..5,
                }))),
                op: BinOp {
                    kind: BinOpKind::Mul,
                    implicit: true,
                    span: 5..5,
                },
                rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                    name: "x".to_string(),
                    span: 5..6,
                }))),
                span: 4..6,
            })),
            span: 0..6,
        }));
    }

    #[test]
    fn implicit_multiplication_extra() {
        let mut parser = Parser::new("4x^2 + 5x + 1");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "4".to_string(),
                        span: 0..1,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Mul,
                        implicit: true,
                        span: 1..1,
                    },
                    rhs: Box::new(Expr::Binary(Binary {
                        lhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                            name: "x".to_string(),
                            span: 1..2,
                        }))),
                        op: BinOp {
                            kind: BinOpKind::Exp,
                            implicit: false,
                            span: 2..3,
                        },
                        rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "2".to_string(),
                            span: 3..4,
                        }))),
                        span: 1..4,
                    })),
                    span: 0..4,
                })),
                op: BinOp {
                    kind: BinOpKind::Add,
                    implicit: false,
                    span: 5..6,
                },
                rhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "5".to_string(),
                        span: 7..8,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Mul,
                        implicit: true,
                        span: 8..8,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                        name: "x".to_string(),
                        span: 8..9,
                    }))),
                    span: 7..9,
                })),
                span: 0..9,
            })),
            op: BinOp {
                kind: BinOpKind::Add,
                implicit: false,
                span: 10..11,
            },
            rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "1".to_string(),
                span: 12..13,
            }))),
            span: 0..13,
        }));
    }

    #[test]
    fn implicit_multiplication_extra_2() {
        let mut parser = Parser::new("3^42.9i");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "3".to_string(),
                    span: 0..1,
                }))),
                op: BinOp {
                    kind: BinOpKind::Exp,
                    implicit: false,
                    span: 1..2,
                },
                rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "42.9".to_string(),
                    span: 2..6,
                }))),
                span: 0..6,
            })),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: true,
                span: 6..6,
            },
            rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                name: "i".to_string(),
                span: 6..7,
            }))),
            span: 0..7,
        }));
    }

    #[test]
    fn implicit_multiplication_nonsensical() {
        let mut parser = Parser::new("2!! 3!!");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Unary(Unary {
                operand: Box::new(Expr::Unary(Unary {
                    operand: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "2".to_string(),
                        span: 0..1,
                    }))),
                    op: UnaryOp {
                        kind: UnaryOpKind::Factorial,
                        span: 1..2,
                    },
                    span: 0..2,
                })),
                op: UnaryOp {
                    kind: UnaryOpKind::Factorial,
                    span: 2..3,
                },
                span: 0..3,
            })),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: true,
                span: 3..4,
            },
            rhs: Box::new(Expr::Unary(Unary {
                operand: Box::new(Expr::Unary(Unary {
                    operand: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "3".to_string(),
                        span: 4..5,
                    }))),
                    op: UnaryOp {
                        kind: UnaryOpKind::Factorial,
                        span: 5..6,
                    },
                    span: 4..6,
                })),
                op: UnaryOp {
                    kind: UnaryOpKind::Factorial,
                    span: 6..7,
                },
                span: 4..7,
            })),
            span: 0..7,
        }));
    }

    #[test]
    fn implicit_multiplication_ambiguous() {
        let mut parser = Parser::new("1 / 2a");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "1".to_string(),
                    span: 0..1,
                }))),
                op: BinOp {
                    kind: BinOpKind::Div,
                    implicit: false,
                    span: 2..3,
                },
                rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "2".to_string(),
                    span: 4..5,
                }))),
                span: 0..5,
            })),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: true,
                span: 5..5,
            },
            rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                name: "a".to_string(),
                span: 5..6,
            }))),
            span: 0..6,
        }));
    }

    #[test]
    fn implicit_multiplication_terms_with_power() {
        let mut parser = Parser::new("-16x y + 2x^2y");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        // -16xy
        let lhs = Expr::Binary(Binary {
            lhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Unary(Unary {
                    operand: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "16".to_string(),
                        span: 1..3,
                    }))),
                    op: UnaryOp {
                        kind: UnaryOpKind::Neg,
                        span: 0..1,
                    },
                    span: 0..3,
                })),
                op: BinOp {
                    kind: BinOpKind::Mul,
                    implicit: true,
                    span: 3..3,
                },
                rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                    name: "x".to_string(),
                    span: 3..4,
                }))),
                span: 0..4,
            })),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: true,
                span: 4..5,
            },
            rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                name: "y".to_string(),
                span: 5..6,
            }))),
            span: 0..6,
        });

        // 2x^2y
        let rhs = Expr::Binary(Binary {
            lhs: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "2".to_string(),
                    span: 9..10,
                }))),
                op: BinOp {
                    kind: BinOpKind::Mul,
                    implicit: true,
                    span: 10..10,
                },
                rhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                        name: "x".to_string(),
                        span: 10..11,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Exp,
                        implicit: false,
                        span: 11..12,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "2".to_string(),
                        span: 12..13,
                    }))),
                    span: 10..13,
                })),
                span: 9..13,
            })),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: true,
                span: 13..13,
            },
            rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                name: "y".to_string(),
                span: 13..14,
            }))),
            span: 9..14,
        });

        // -16xy + 2x^2y
        let add = Expr::Binary(Binary {
            lhs: Box::new(lhs),
            op: BinOp {
                kind: BinOpKind::Add,
                implicit: false,
                span: 7..8,
            },
            rhs: Box::new(rhs),
            span: 0..14,
        });

        assert_eq!(expr, add);
    }

    #[test]
    fn parenthesized() {
        let mut parser = Parser::new("(1 + 2) * __");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Binary(Binary {
            lhs: Box::new(Expr::Paren(Paren {
                expr: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "1".to_string(),
                        span: 1..2,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Add,
                        implicit: false,
                        span: 3..4,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "2".to_string(),
                        span: 5..6,
                    }))),
                    span: 1..6,
                })),
                span: 0..7,
            })),
            op: BinOp {
                kind: BinOpKind::Mul,
                implicit: false,
                span: 8..9,
            },
            rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                name: "__".to_string(),
                span: 10..12,
            }))),
            span: 0..12,
        }));
    }

    #[test]
    fn parenthesized_complicated() {
        let mut parser = Parser::new("(3 * 9 + 4.0 / 11.9 % (6 - 3))");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Paren(Paren {
            expr: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "3".to_string(),
                        span: 1..2,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Mul,
                        implicit: false,
                        span: 3..4,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "9".to_string(),
                        span: 5..6,
                    }))),
                    span: 1..6,
                })),
                op: BinOp {
                    kind: BinOpKind::Add,
                    implicit: false,
                    span: 7..8,
                },
                rhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Binary(Binary {
                        lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "4.0".to_string(),
                            span: 9..12,
                        }))),
                        op: BinOp {
                            kind: BinOpKind::Div,
                            implicit: false,
                            span: 13..14,
                        },
                        rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "11.9".to_string(),
                            span: 15..19,
                        }))),
                        span: 9..19,
                    })),
                    op: BinOp {
                        kind: BinOpKind::Mod,
                        implicit: false,
                        span: 20..21,
                    },
                    rhs: Box::new(Expr::Paren(Paren {
                        expr: Box::new(Expr::Binary(Binary {
                            lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                                value: "6".to_string(),
                                span: 23..24,
                            }))),
                            op: BinOp {
                                kind: BinOpKind::Sub,
                                implicit: false,
                                span: 25..26,
                            },
                            rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                                value: "3".to_string(),
                                span: 27..28,
                            }))),
                            span: 23..28,
                        })),
                        span: 22..29,
                    })),
                    span: 9..29,
                })),
                span: 1..29,
            })),
            span: 0..30,
        }));
    }

    #[test]
    fn block_code() {
        let mut parser = Parser::new("{ x = 5; y = 6; x + y }");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Block(Block {
            stmts: vec![
                Stmt {
                    expr: Expr::Assign(Assign {
                        target: AssignTarget::Symbol(LitSym {
                            name: "x".to_string(),
                            span: 2..3,
                        }),
                        op: AssignOp {
                            kind: AssignOpKind::Assign,
                            span: 4..5,
                        },
                        value: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "5".to_string(),
                            span: 6..7,
                        }))),
                        span: 2..7,
                    }),
                    semicolon: Some(7..8),
                    span: 2..8,
                },
                Stmt {
                    expr: Expr::Assign(Assign {
                        target: AssignTarget::Symbol(LitSym {
                            name: "y".to_string(),
                            span: 9..10,
                        }),
                        op: AssignOp {
                            kind: AssignOpKind::Assign,
                            span: 11..12,
                        },
                        value: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "6".to_string(),
                            span: 13..14,
                        }))),
                        span: 9..14,
                    }),
                    semicolon: Some(14..15),
                    span: 9..15,
                },
                Stmt {
                    expr: Expr::Binary(Binary {
                        lhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                            name: "x".to_string(),
                            span: 16..17,
                        }))),
                        op: BinOp {
                            kind: BinOpKind::Add,
                            implicit: false,
                            span: 18..19,
                        },
                        rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                            name: "y".to_string(),
                            span: 20..21,
                        }))),
                        span: 16..21,
                    }),
                    semicolon: None,
                    span: 16..21,
                },
            ],
            span: 0..23,
        }));
    }

    #[test]
    fn if_block() {
        let mut parser = Parser::new("if d then { abs''(d) } else { f = 1; f }");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::If(If {
            condition: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                name: "d".to_string(),
                span: 3..4,
            }))),
            then_expr: Box::new(Expr::Block(Block {
                stmts: vec![
                    Stmt {
                        expr: Expr::Call(Call {
                            name: LitSym {
                                name: "abs".to_string(),
                                span: 12..15,
                            },
                            derivatives: 2,
                            args: vec![
                                Expr::Literal(Literal::Symbol(LitSym {
                                    name: "d".to_string(),
                                    span: 18..19,
                                })),
                            ],
                            span: 12..20,
                            paren_span: 17..20,
                        }),
                        semicolon: None,
                        span: 12..20,
                    },
                ],
                span: 10..22,
            })),
            else_expr: Some(Box::new(Expr::Block(Block {
                stmts: vec![
                    Stmt {
                        expr: Expr::Assign(Assign {
                            target: AssignTarget::Symbol(LitSym {
                                name: "f".to_string(),
                                span: 30..31,
                            }),
                            op: AssignOp {
                                kind: AssignOpKind::Assign,
                                span: 32..33,
                            },
                            value: Box::new(Expr::Literal(Literal::Number(LitNum {
                                value: "1".to_string(),
                                span: 34..35,
                            }))),
                            span: 30..35,
                        }),
                        semicolon: Some(35..36),
                        span: 30..36,
                    },
                    Stmt {
                        expr: Expr::Literal(Literal::Symbol(LitSym {
                            name: "f".to_string(),
                            span: 37..38,
                        })),
                        semicolon: None,
                        span: 37..38,
                    },
                ],
                span: 28..40,
            }))),
            span: 0..40,
            if_span: 0..2,
            then_span: 5..9,
            else_span: Some(23..27),
        }));
    }

    #[test]
    fn assign_to_var() {
        let mut parser = Parser::new("fx += 1 / pi");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Assign(Assign {
            target: AssignTarget::Symbol(LitSym {
                name: "fx".to_string(),
                span: 0..2,
            }),
            op: AssignOp {
                kind: AssignOpKind::Add,
                span: 3..5,
            },
            value: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "1".to_string(),
                    span: 6..7,
                }))),
                op: BinOp {
                    kind: BinOpKind::Div,
                    implicit: false,
                    span: 8..9,
                },
                rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                    name: "pi".to_string(),
                    span: 10..12,
                }))),
                span: 6..12,
            })),
            span: 0..12,
        }));
    }

    #[test]
    fn assign_to_function() {
        let mut parser = Parser::new("f(x) = x^2 + 5x");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Assign(Assign {
            target: AssignTarget::Func(FuncHeader {
                name: LitSym {
                    name: "f".to_string(),
                    span: 0..1,
                },
                params: vec![
                    Param::Symbol(LitSym {
                        name: "x".to_string(),
                        span: 2..3,
                    }),
                ],
                span: 0..4,
            }),
            op: AssignOp {
                kind: AssignOpKind::Assign,
                span: 5..6,
            },
            value: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                        name: "x".to_string(),
                        span: 7..8,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Exp,
                        implicit: false,
                        span: 8..9,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "2".to_string(),
                        span: 9..10,
                    }))),
                    span: 7..10,
                })),
                op: BinOp {
                    kind: BinOpKind::Add,
                    implicit: false,
                    span: 11..12,
                },
                rhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "5".to_string(),
                        span: 13..14,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Mul,
                        implicit: true,
                        span: 14..14,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                        name: "x".to_string(),
                        span: 14..15,
                    }))),
                    span: 13..15,
                })),
                span: 7..15,
            })),
            span: 0..15,
        }));
    }

    #[test]
    fn assign_to_complicated_function() {
        let mut parser = Parser::new("discrim(a = 1, b = 5, c) = b^2 - 4a * c");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Assign(Assign {
            target: AssignTarget::Func(FuncHeader {
                name: LitSym {
                    name: "discrim".to_string(),
                    span: 0..7,
                },
                params: vec![
                    Param::Default(
                        LitSym {
                            name: "a".to_string(),
                            span: 8..9,
                        },
                        Expr::Literal(Literal::Number(LitNum {
                            value: "1".to_string(),
                            span: 12..13,
                        })),
                    ),
                    Param::Default(
                        LitSym {
                            name: "b".to_string(),
                            span: 15..16,
                        },
                        Expr::Literal(Literal::Number(LitNum {
                            value: "5".to_string(),
                            span: 19..20,
                        })),
                    ),
                    Param::Symbol(
                        LitSym {
                            name: "c".to_string(),
                            span: 22..23,
                        },
                    ),
                ],
                span: 0..24,
            }),
            op: AssignOp {
                kind: AssignOpKind::Assign,
                span: 25..26,
            },
            value: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                        name: "b".to_string(),
                        span: 27..28,
                    }))),
                    op: BinOp {
                        kind: BinOpKind::Exp,
                        implicit: false,
                        span: 28..29,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                        value: "2".to_string(),
                        span: 29..30,
                    }))),
                    span: 27..30,
                })),
                op: BinOp {
                    kind: BinOpKind::Sub,
                    implicit: false,
                    span: 31..32,
                },
                rhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Binary(Binary {
                        lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                            value: "4".to_string(),
                            span: 33..34,
                        }))),
                        op: BinOp {
                            kind: BinOpKind::Mul,
                            implicit: true,
                            span: 34..34,
                        },
                        rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                            name: "a".to_string(),
                            span: 34..35,
                        }))),
                        span: 33..35,
                    })),
                    op: BinOp {
                        kind: BinOpKind::Mul,
                        implicit: false,
                        span: 36..37,
                    },
                    rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                        name: "c".to_string(),
                        span: 38..39,
                    }))),
                    span: 33..39,
                })),
                span: 27..39,
            })),
            span: 0..39,
        }));
    }

    #[test]
    fn function_call() {
        let mut parser = Parser::new("f(x)");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Call(Call {
            name: LitSym {
                name: "f".to_string(),
                span: 0..1,
            },
            derivatives: 0,
            args: vec![
                Expr::Literal(Literal::Symbol(LitSym {
                    name: "x".to_string(),
                    span: 2..3,
                })),
            ],
            span: 0..4,
            paren_span: 1..4,
        }));
    }

    #[test]
    fn function_call_whitespace() {
        let mut parser = Parser::new("ncr  ' ' ' '' ' ( 8 , 5 )");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Call(Call {
            name: LitSym {
                name: "ncr".to_string(),
                span: 0..3,
            },
            derivatives: 6,
            args: vec![
                Expr::Literal(Literal::Number(LitNum {
                    value: "8".to_string(),
                    span: 18..19,
                })),
                Expr::Literal(Literal::Number(LitNum {
                    value: "5".to_string(),
                    span: 22..23,
                })),
            ],
            span: 0..25,
            paren_span: 16..25,
        }));
    }

    #[test]
    fn blank_function_header() {
        let mut parser = Parser::new("f() = 5");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Assign(Assign {
            target: AssignTarget::Func(FuncHeader {
                name: LitSym {
                    name: "f".to_string(),
                    span: 0..1,
                },
                params: vec![],
                span: 0..3,
            }),
            op: AssignOp {
                kind: AssignOpKind::Assign,
                span: 4..5,
            },
            value: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "5".to_string(),
                span: 6..7,
            }))),
            span: 0..7,
        }));
    }

    #[test]
    fn oneline_while_loop() {
        let mut parser = Parser::new("while x < 5 then x += 1");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::While(While {
            condition: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                    name: "x".to_string(),
                    span: 6..7,
                }))),
                op: BinOp {
                    kind: BinOpKind::Less,
                    implicit: false,
                    span: 8..9,
                },
                rhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "5".to_string(),
                    span: 10..11,
                }))),
                span: 6..11,
            })),
            body: Box::new(Expr::Assign(Assign {
                target: AssignTarget::Symbol(LitSym {
                    name: "x".to_string(),
                    span: 17..18,
                }),
                op: AssignOp {
                    kind: AssignOpKind::Add,
                    span: 19..21,
                },
                value: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "1".to_string(),
                    span: 22..23,
                }))),
                span: 17..23,
            })),
            span: 0..23,
            while_span: 0..5,
            then_span: 12..16,
        }));
    }

    #[test]
    fn source_code() {
        let mut parser = Parser::new("x = 5;
iseven(n) = n % 2 == 0;
if iseven(x) then {
    x^2 + 5x + 6
} else if (bool(x) && false) then {
    exp(x)
} else {
    log(x, 2)
}");
        assert!(parser.try_parse_full_many::<Stmt>().is_ok());
    }
}
