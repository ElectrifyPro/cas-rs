use logos::Logos;
use std::ops::Range;

/// The different kinds of tokens that can be produced by the tokenizer.
#[derive(Logos, Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
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

    #[token("not")]
    Not,

    #[token("!")]
    Factorial,

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

    #[regex(r"[0-9]+\.?")]
    Int,

    #[regex(r"[0-9]+\.[0-9]*")]
    Float,

    #[token(".")]
    Dot,

    #[regex(r".", priority = 0)]
    Symbol,
}

impl TokenKind {
    /// Returns true if the token represents whitespace.
    pub fn is_whitespace(self) -> bool {
        matches!(self, TokenKind::Whitespace | TokenKind::NewLine)
    }
}

/// A token produced by the tokenizer.
#[derive(Debug, Clone, PartialEq)]
pub struct Token<'source> {
    /// The region of the source code that this token originated from.
    pub span: Range<usize>,

    /// The kind of token.
    pub kind: TokenKind,

    /// The raw lexeme that was parsed into this token.
    pub lexeme: &'source str,
}

impl Token<'_> {
    /// Returns true if the token represents whitespace.
    pub fn is_whitespace(&self) -> bool {
        self.kind.is_whitespace()
    }
}
