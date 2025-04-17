use logos::Logos;
use std::ops::Range;

/// The different kinds of tokens that can be produced by the tokenizer.
#[derive(Logos, Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    #[regex(r"[\n\r]+")]
    NewLine,

    #[regex(r"[ \t]+")]
    Whitespace,

    #[regex(r"//.*")]
    Comment,

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

    #[token("+=")]
    AddAssign,

    #[token("-=")]
    SubAssign,

    #[token("*=")]
    MulAssign,

    #[token("/=")]
    DivAssign,

    #[token("%=")]
    ModAssign,

    #[token("^=")]
    ExpAssign,

    #[token("&&=")]
    AndAssign,

    #[token("||=")]
    OrAssign,

    #[token("&=")]
    BitAndAssign,

    #[token("|=")]
    BitOrAssign,

    #[token(">>=")]
    BitRightAssign,

    #[token("<<=")]
    BitLeftAssign,

    #[token("0b")]
    Bin,

    #[token("0o")]
    Oct,

    #[token("0x")]
    Hex,

    #[regex(r"[a-zA-Z_]+|atan2")] // TODO: includes horrible hard-coded test for atan2
    Name,

    #[regex(r"let|if|then|else|for|sum|product|in|of|loop|while|break|continue|return")]
    Keyword,

    #[token(",")]
    Comma,

    #[token("(")]
    OpenParen,

    #[token(")")]
    CloseParen,

    #[token("{")]
    OpenCurly,

    #[token("}")]
    CloseCurly,

    #[token("[")]
    OpenSquare,

    #[token("]")]
    CloseSquare,

    #[token("'")]
    Quote,

    #[token(";")]
    Semicolon,

    #[regex(r"\d+")]
    Int,

    #[regex(r"(true|false)")]
    Boolean,

    #[token(".")]
    Dot,

    #[token("..")]
    RangeHalfOpen,

    #[token("..=")]
    RangeClosed,

    #[regex(r".", priority = 0)]
    Symbol,
}

impl TokenKind {
    /// Returns true if the token represents a token that should be ignored by the parser.
    pub fn is_ignore(self) -> bool {
        matches!(self, TokenKind::Whitespace | TokenKind::NewLine | TokenKind::Comment)
    }

    /// Returns true if the token represents significant whitespace.
    pub fn is_significant_whitespace(self) -> bool {
        matches!(self, TokenKind::NewLine)
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
    /// Returns true if the token represents a token that should be ignored by the parser.
    pub fn is_ignore(&self) -> bool {
        self.kind.is_ignore()
    }

    /// Returns true if the token represents significant whitespace, i.e., the token is a newline.
    pub fn is_significant_whitespace(&self) -> bool {
        self.kind.is_significant_whitespace()
    }
}
