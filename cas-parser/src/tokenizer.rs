use logos::{Lexer, Logos};

/// The different kinds of tokens that can be produced by the tokenizer.
#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[regex(r"[\n\r]+")]
    NewLine,

    #[regex(r"[ \t]+")]
    Whitespace,

    #[token("==")]
    Eq,

    #[token("!=")]
    NotEq,

    #[token("~==")]
    ApproxEq,

    #[token("~!=")]
    ApproxNotEq,

    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("%")]
    Mod,

    #[token("^")]
    Exp,

    #[token(">")]
    Greater,

    #[token(">=")]
    GreaterEq,

    #[token("<")]
    Less,

    #[token("<=")]
    LessEq,

    #[token("!")]
    Bang, // used for both factorial and not operation

    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("&")]
    BitAnd,

    #[token("|")]
    BitOr,

    #[token("~")]
    BitNot,

    #[token(">>")]
    BitRight,

    #[token("<<")]
    BitLeft,

    #[token("=")]
    Assign,

    #[token("0b")]
    Bin,

    #[token("0o")]
    Oct,

    #[token("0x")]
    Hex,

    #[regex(r"[a-zA-Z_]+")]
    Name,

    #[token(",")]
    Comma,

    #[token("(")]
    OpenParen,

    #[token(")")]
    CloseParen,

    #[regex(r"[0-9]+")]
    Number,

    #[token(".")]
    Dot,

    #[regex(r".", priority = 0)]
    Symbol,
}

/// Returns an iterator over the tokens in the given string.
pub fn tokenize(input: &str) -> Lexer<Token> {
    Token::lexer(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Compares the tokens produced by the tokenizer to the raw expected tokens.
    fn compare_tokens<const N: usize>(input: &str, expected: [(Token, &str); N]) {
        let mut lexer = tokenize(input);

        for (token, lexeme) in expected.into_iter() {
            assert_eq!(lexer.next(), Some(Ok(token)));
            assert_eq!(lexer.slice(), lexeme);
        }

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn basic_expr() {
        compare_tokens(
            "1 + 2",
            [
                (Token::Number, "1"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Number, "2"),
            ],
        );
    }

    #[test]
    fn complex_expr() {
        compare_tokens(
            "3      x - 0xff + 0b101 * $",
            [
                (Token::Number, "3"),
                (Token::Whitespace, "      "),
                (Token::Name, "x"),
                (Token::Whitespace, " "),
                (Token::Sub, "-"),
                (Token::Whitespace, " "),
                (Token::Hex, "0x"),
                (Token::Name, "ff"),
                (Token::Whitespace, " "),
                (Token::Add, "+"),
                (Token::Whitespace, " "),
                (Token::Bin, "0b"),
                (Token::Number, "101"),
                (Token::Whitespace, " "),
                (Token::Mul, "*"),
                (Token::Whitespace, " "),
                (Token::Symbol, "$"),
            ],
        );
    }
}
