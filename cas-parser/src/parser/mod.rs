pub mod assign;
pub mod binary;
pub mod call;
pub mod error;
pub mod expr;
pub mod literal;
pub mod paren;
pub mod token;
pub mod unary;

use cas_error::ErrorKind;
use error::{Error, kind};
use super::tokenizer::{tokenize_complete, Token, TokenKind};
use std::ops::Range;

/// Attempts to parse a value from the given stream of tokens, using multiple parsing functions
/// in order. **This function panics if the given slice is empty.** The first function that
/// succeeds is used to parse the value.
///
/// This function can also catch fatal errors and immediately short-circuit the parsing
/// process.
///
/// If parsing is successful, the stream is advanced past the consumed tokens and the parsed
/// value is returned. Otherwise, the stream is left unchanged and the error of the last
/// attempted parsing function is returned.
#[macro_export]
macro_rules! try_parse_catch_fatal {
    ($($expr:expr),+ $(,)?) => {{
        $(
            match $expr {
                Ok(value) => return Ok(value),
                Err(err) if err.fatal => return Err(err),
                // ignore this error and try the next parser, or return it
                err => err,
            }
        )+
    }};
}

/// A high-level parser for the language. This is the type to use to parse an arbitrary piece of
/// code into an abstract syntax tree.
#[derive(Debug, Clone)]
pub struct Parser<'source> {
    /// The tokens that this parser is currently parsing.
    tokens: Box<[Token<'source>]>,

    /// The index of the **next** token to be parsed.
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

    /// Creates an error that points at the current token, or the end of the source code if the
    /// cursor is at the end of the stream.
    pub fn error(&self, kind: impl ErrorKind + 'static) -> Error {
        Error::new(vec![self.span()], kind)
    }

    /// Creates a fatal error that points at the current token, or the end of the source code if
    /// the cursor is at the end of the stream.
    pub fn error_fatal(&self, kind: impl ErrorKind + 'static) -> Error {
        Error::new_fatal(vec![self.span()], kind)
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
    pub fn try_parse<T: Parse>(&mut self) -> Result<T, Error> {
        self.try_parse_with_fn(T::parse)
    }

    /// Speculatively parses multiple values (zero or more) from the given stream of tokens, each
    /// delimited by a certain token. This function can be used in the [`Parse::parse`]
    /// implementation of a type with the given [`Parser`], as it will automatically backtrack the
    /// cursor position if parsing fails.
    ///
    /// If parsing is successful, the stream is advanced past the consumed tokens and the parsed
    /// values are returned. Otherwise, the stream is left unchanged and an error is returned.
    pub fn try_parse_delimited<T: Parse>(
        &mut self,
        delimiter: TokenKind,
    ) -> Result<Vec<T>, Error> {
        let start = self.cursor;
        let mut values = Vec::new();

        loop {
            match self.try_parse::<T>() {
                Ok(value) => values.push(value),
                Err(_) => {
                    if values.is_empty() {
                        self.cursor = start;
                    }

                    return Ok(values);
                },
            }

            match self.current_token() {
                Some(token) if token.kind == delimiter => {
                    self.cursor += 1;
                },
                _ => return Ok(values),
            }
        }
    }

    /// Speculatively parses a value from the given stream of tokens, using a custom parsing
    /// function to parse the value. This function can be used in the [`Parse::parse`]
    /// implementation of a type with the given [`Parser`], as it will automatically backtrack the
    /// cursor position if parsing fails.
    ///
    /// If parsing is successful, the stream is advanced past the consumed tokens and the parsed
    /// value is returned. Otherwise, the stream is left unchanged and an error is returned.
    pub fn try_parse_with_fn<T, F>(&mut self, f: F) -> Result<T, Error>
    where
        F: FnOnce(&mut Parser) -> Result<T, Error>,
    {
        let start = self.cursor;
        match f(self) {
            Ok(value) => Ok(value),
            err => {
                self.cursor = start;
                err
            },
        }
    }

    /// Speculatively parses a value from the given stream of tokens, with a validation predicate.
    /// The value must parse successfully, **and** the predicate must return [`Ok`] for this
    /// function to return successfully.
    ///
    /// If parsing is successful, the stream is advanced past the consumed tokens and the parsed
    /// value is returned. Otherwise, the stream is left unchanged and an error is returned.
    pub fn try_parse_then<T: Parse, F>(&mut self, predicate: F) -> Result<T, Error>
    where
        F: FnOnce(&T, &Parser) -> Result<(), Error>,
    {
        let start = self.cursor;

        // closure workaround allows us to use `?` in the closure
        let compute = || {
            let value = T::parse(self)?;
            predicate(&value, self)?;
            Ok(value)
        };

        match compute() {
            Ok(value) => Ok(value),
            err => {
                self.cursor = start;
                err
            },
        }
    }

    /// Attempts to parse a value from the given stream of tokens. All the tokens must be consumed
    /// by the parser; if not, an error is returned.
    pub fn try_parse_full<T: Parse>(&mut self) -> Result<T, Error> {
        let value = T::parse(self)?;

        // consume whitespace
        self.advance_past_whitespace();

        if self.cursor >= self.tokens.len() {
            Ok(value)
        } else {
            Err(self.error(kind::ExpectedEof))
        }
    }
}

/// Any type that can be parsed from a source of tokens.
pub trait Parse: Sized {
    /// Parses a value from the given stream of tokens, advancing the stream past the consumed
    /// tokens if parsing is successful.
    ///
    /// This function should be used by consumers of the library.
    fn parse(input: &mut Parser) -> Result<Self, Error>;
}

/// The associativity of a binary or unary operation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    /// The binary / unary operation is left-associative.
    ///
    /// For binary operations, this means `a op b op c` is evaluated as `(a op b) op c`. For unary
    /// operations, this means `a op op` is evaluated as `(a op) op` (the operators appear to the
    /// right of the operand).
    Left,

    /// The binary / unary operation is right-associative.
    ///
    /// For binary operations, this means `a op b op c` is evaluated as `a op (b op c)`. For unary
    /// operations, this means `op op a` is evaluated as `op (op a)` (the operators appear to the
    /// left of the operand).
    Right,
}

/// The precedence of an operation, in order from lowest precedence (evaluated last) to highest
/// precedence (evaluated first).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Precedence {
    /// Any precedence.
    Any,

    /// Precedence of logical or (`or`).
    Or,

    /// Precedence of logical and (`and`).
    And,

    /// Precedence of comparisons (`>`, `>=`, `<`, `<=`, `==`, `!=`, `~==`, and `~!=`).
    Compare,

    /// Precedence of bitwise or (`|`).
    BitOr,

    /// Precedence of bitwise and (`&`).
    BitAnd,

    /// Precedence of bitshifts (`<<` and `>>`).
    Shift,

    /// Precedence of addition (`+`) and subtraction (`-`), which separate terms.
    Term,

    /// Precedence of multiplication (`*`), division (`/`), and modulo (`%`), which separate
    /// factors.
    Factor,

    /// Precedence of unary subtraction (`-`).
    Neg,

    /// Precedence of exponentiation (`^`).
    Exp,

    /// Precedence of factorial (`!`).
    Factorial,

    /// Precedence of bitwise not (`~`).
    BitNot,

    /// Precedence of logical not (`not`).
    Not,
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let left = *self as u8;
        let right = *other as u8;
        left.partial_cmp(&right)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use super::*;

    use assign::{Assign, AssignTarget, FuncHeader, Param};
    use binary::Binary;
    use call::Call;
    use expr::Expr;
    use literal::{Literal, LitNum, LitRadix, LitSym};
    use paren::Paren;
    use token::op::{BinOp, BinOpKind, UnaryOp, UnaryOpKind};
    use unary::Unary;

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
    fn assign_to_var() {
        let mut parser = Parser::new("fx = 1 / pi");
        let expr = parser.try_parse_full::<Expr>().unwrap();

        assert_eq!(expr, Expr::Assign(Assign {
            target: AssignTarget::Symbol(LitSym {
                name: "fx".to_string(),
                span: 0..2,
            }),
            value: Box::new(Expr::Binary(Binary {
                lhs: Box::new(Expr::Literal(Literal::Number(LitNum {
                    value: "1".to_string(),
                    span: 5..6,
                }))),
                op: BinOp {
                    kind: BinOpKind::Div,
                    implicit: false,
                    span: 7..8,
                },
                rhs: Box::new(Expr::Literal(Literal::Symbol(LitSym {
                    name: "pi".to_string(),
                    span: 9..11,
                }))),
                span: 5..11,
            })),
            span: 0..11,
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
            value: Box::new(Expr::Literal(Literal::Number(LitNum {
                value: "5".to_string(),
                span: 6..7,
            }))),
            span: 0..7,
        }));
    }
}
