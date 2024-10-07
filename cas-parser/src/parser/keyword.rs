use cas_error::Error;
use crate::{
    parser::{error::UnexpectedToken, garbage::Garbage, Parser, Parse},
    tokenizer::TokenKind,
};
use std::ops::Range;

/// Generates a unit struct for each keyword, as well as a simple [`Parse`] implementation for each
/// keyword. This enables the parser to use and request keywords as a type, which is much more
/// ergonomic than using a string.
macro_rules! keywords {
    ($(($name:ident, $lexeme:tt))*) => {
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

                    if token.kind == TokenKind::Keyword {
                        if token.lexeme != stringify!($lexeme) {
                            // return Err(vec![Error::new(vec![token.span], UnexpectedToken {
                            //     expected: &[stringify!($lexeme)],
                            //     found: token.kind,
                            // })]);
                            // TODO
                            return Err(vec![Error::new(vec![token.span], UnexpectedToken {
                                expected: &[TokenKind::Keyword],
                                found: token.kind,
                            })]);
                        }
                        Ok(Self {
                            lexeme: token.lexeme,
                            span: token.span,
                        })
                    } else {
                        Err(vec![Error::new(vec![token.span], UnexpectedToken {
                            expected: &[TokenKind::Keyword],
                            found: token.kind,
                        })])
                    }
                }
            }

            impl<'source> Garbage for $name<'source> {
                fn garbage() -> Self {
                    Self { lexeme: "", span: 0..0 }
                }
            }
        )*
    };
}

keywords!(
    (Let, let)
    (If, if)
    (Then, then)
    (Else, else)
    (For, for)
    (Sum, sum)
    (Product, product)
    (In, in)
    (Of, of)
    (Loop, loop)
    (While, while)
    (Break, break)
    (Continue, continue)
    (Return, return)
);
