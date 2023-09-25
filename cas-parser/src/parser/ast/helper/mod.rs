//! Helper `struct`s for parsing AST nodes.

pub mod delimited;
pub mod surrounded;

pub use delimited::Delimited;
pub use surrounded::Surrounded;

/// Type alias for a comma-separated list of values, surrounded by parentheses.
pub type ParenDelimited<'source, T> = surrounded::Surrounded<
    crate::parser::token::OpenParen<'source>,
    crate::parser::token::CloseParen<'source>,
    delimited::Delimited<crate::parser::token::Comma<'source>, T>,
>;
