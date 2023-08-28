pub mod token;

use logos::{Lexer, Logos};
pub use token::{Token, TokenKind};

/// Returns an iterator over the token kinds produced by the tokenizer.
pub fn tokenize(input: &str) -> Lexer<TokenKind> {
    TokenKind::lexer(input)
}

/// Returns an owned array containing all of the tokens produced by the tokenizer. This allows us
/// to backtrack in case of an error.
pub fn tokenize_complete(input: &str) -> Box<[Token]> {
    let mut lexer = tokenize(input);
    let mut tokens = Vec::new();

    while let Some(Ok(kind)) = lexer.next() {
        tokens.push(Token {
            span: lexer.span(),
            kind,
            lexeme: lexer.slice(),
        });
    }

    tokens.into_boxed_slice()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Compares the tokens produced by the tokenizer to the raw expected tokens.
    fn compare_tokens<'source, const N: usize>(input: &'source str, expected: [(TokenKind, &'source str); N]) {
        let mut lexer = tokenize(input);

        for (expected_kind, expected_lexeme) in expected.into_iter() {
            assert_eq!(lexer.next(), Some(Ok(expected_kind)));
            assert_eq!(lexer.slice(), expected_lexeme);
        }

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn basic_expr() {
        compare_tokens(
            "1 + 2",
            [
                (TokenKind::Int, "1"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Add, "+"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Int, "2"),
            ],
        );
    }

    #[test]
    fn complex_expr() {
        compare_tokens(
            "3      x - 0xff + 0b101 * $",
            [
                (TokenKind::Int, "3"),
                (TokenKind::Whitespace, "      "),
                (TokenKind::Name, "x"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Sub, "-"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Hex, "0x"),
                (TokenKind::Name, "ff"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Add, "+"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Bin, "0b"),
                (TokenKind::Int, "101"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Mul, "*"),
                (TokenKind::Whitespace, " "),
                (TokenKind::Symbol, "$"),
            ],
        );
    }
}
