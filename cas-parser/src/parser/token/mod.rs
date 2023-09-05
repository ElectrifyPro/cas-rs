pub mod op;

use crate::{
    parser::{error::{kind, Error}, Parser, Parse},
    tokenizer::TokenKind,
};
use std::ops::Range;

/// Generates a unit struct for each token kind, as well as a simple [`Parse`] implementation for
/// each token kind. This enables the parser to use and request token kinds as a type, which is
/// much more ergonomic than using a string.
macro_rules! token_kinds {
    ($($name:ident)*) => {
        $(
            #[derive(Clone, Debug, PartialEq)]
            pub struct $name<'source> {
                pub lexeme: &'source str,
                pub span: Range<usize>,
            }

            impl<'source> Parse<'source> for $name<'source> {
                fn std_parse(
                    input: &mut Parser<'source>,
                    _: &mut Vec<Error>
                ) -> Result<Self, Vec<Error>> {
                    let token = input.next_token().map_err(|e| vec![e])?;

                    if token.kind == TokenKind::$name {
                        Ok(Self {
                            lexeme: token.lexeme,
                            span: token.span,
                        })
                    } else {
                        Err(vec![Error::new(vec![token.span], kind::UnexpectedToken {
                            expected: &[TokenKind::$name],
                            found: token.kind,
                        })])
                    }
                }
            }
        )*
    };
}

token_kinds!(
    NewLine
    Whitespace
    Eq
    NotEq
    ApproxEq
    ApproxNotEq
    Add
    Sub
    Mul
    Div
    Mod
    Exp
    Greater
    GreaterEq
    Less
    LessEq
    Not
    Factorial
    And
    Or
    BitAnd
    BitOr
    BitNot
    BitRight
    BitLeft
    Assign
    Bin
    Oct
    Hex
    Name
    Keyword
    Comma
    OpenParen
    CloseParen
    Quote
    Int
    Float
    Dot
    Symbol
);
