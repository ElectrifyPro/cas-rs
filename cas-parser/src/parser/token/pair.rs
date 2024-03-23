use crate::{
    parser::{
        token::{
            CloseParen,
            OpenParen,
            CloseCurly,
            OpenCurly,
            CloseSquare,
            OpenSquare,
        },
        Parse,
    },
    tokenizer::TokenKind,
};

/// A trait for token kinds that have a logical pairing with another token kind. Only tokens that
/// implement [`Pair`] can be used with the [`Surrounded`] helper.
///
/// This includes tokens like parentheses, square brackets, and curly braces.
///
/// [`Surrounded`]: crate::parser::ast::helper::surrounded::Surrounded
pub trait Pair {
    /// The type of the opening token, defined in the [`token`] module.
    ///
    /// [`token`]: crate::parser::token
    type Open<'source>: Parse<'source>;

    /// The type of the closing token, defined in the [`token`] module.
    ///
    /// [`token`]: crate::parser::token
    type Close<'source>: Parse<'source>;

    /// The corresponding opening [`TokenKind`].
    const OPEN: TokenKind;

    /// The corresponding closing [`TokenKind`].
    const CLOSE: TokenKind;
}

/// Generates implementations of [`Pair`] for each pair of token kinds.
macro_rules! pairs {
    ($($open:ident $close:ident),* $(,)?) => {
        $(
            impl Pair for $open<'_> {
                type Open<'source> = $open<'source>;
                type Close<'source> = $close<'source>;
                const OPEN: TokenKind = TokenKind::$open;
                const CLOSE: TokenKind = TokenKind::$close;
            }

            impl Pair for $close<'_> {
                type Open<'source> = $open<'source>;
                type Close<'source> = $close<'source>;
                const OPEN: TokenKind = TokenKind::$open;
                const CLOSE: TokenKind = TokenKind::$close;
            }
        )*
    }
}

pairs!(
    OpenParen CloseParen,
    OpenCurly CloseCurly,
    OpenSquare CloseSquare,
);
