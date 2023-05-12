use std::ops::Range;
use crate::{
    parser::{
        error::{kind, Error},
        token::{Float, Name, Int, Quote},
        Parse,
        Parser,
    },
    tokenizer::TokenKind,
    try_parse_catch_fatal,
};

/// A number literal. Integers and floating-point numbers are both supported and represented here
/// as `f64`.
#[derive(Debug, Clone, PartialEq)]
pub struct LitNum {
    /// The value of the number literal.
    pub value: f64,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl Parse for LitNum {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let (lexeme, span) = input
            .try_parse::<Int>()
            .map(|num| (num.lexeme, num.span))
            .or_else(|_| input.try_parse::<Float>().map(|num| (num.lexeme, num.span)))?;
        Ok(Self {
            value: lexeme.parse().unwrap(),
            span,
        })
    }
}

/// The digits in base 64, in order of increasing value.
pub const DIGITS: [char; 64] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '+', '/',
];

/// Helper struct to parse the digits used in various bases.
#[derive(Debug, Clone, PartialEq)]
struct RadixWord {
    /// The parsed digits.
    pub value: String,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl Parse for RadixWord {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let mut value = String::new();
        let mut span = 0..0;
        while let Ok(token) = input.next_token() {
            match token.kind {
                TokenKind::Add
                    | TokenKind::Name
                    | TokenKind::Int
                    | TokenKind::Div => value.push_str(token.lexeme),
                _ => {
                    input.prev();
                    break;
                },
            }

            if span.start == 0 {
                span.start = token.span.start;
            }
            span.end = token.span.end;
        }
        Ok(Self {
            value,
            span,
        })
    }
}

/// A number written in radix notation. Radix notation allows users to express integers in a base
/// other than base 10.
#[derive(Debug, Clone, PartialEq)]
pub struct LitRadix {
    /// The radix of the literal. This value must be between 2 and 64, inclusive.
    pub base: u8,

    /// The number, expressed in the given radix.
    pub value: String,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl Parse for LitRadix {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let num = input.try_parse::<Int>()?;
        let _ = input.try_parse::<Quote>()?;

        let base = match num.lexeme.parse::<u8>() {
            Ok(base) if base >= 2 && base <= 64 => base,
            Ok(base) if base < 2 => return Err(Error::new_fatal(num.span, kind::InvalidRadixBase { too_large: false })),
            _ => return Err(Error::new_fatal(num.span, kind::InvalidRadixBase { too_large: true })),
        };
        let word = input.try_parse::<RadixWord>()?;

        // ensure that the number is valid for this radix
        let allowed_digits = &DIGITS[..base as usize];
        for (i, c) in word.value.chars().enumerate() {
            if !allowed_digits.contains(&c) {
                let char_span_start = word.span.start + i;
                return Err(Error::new_fatal(char_span_start..char_span_start + 1, kind::InvalidRadixDigit {
                    radix: base,
                    allowed: allowed_digits,
                    digit: c,
                }));
            }
        }

        Ok(Self {
            base,
            value: word.value,
            span: num.span.start..word.span.end,
        })
    }
}

/// A symbol / identifier literal. Symbols are used to represent variables and functions.
#[derive(Debug, Clone, PartialEq)]
pub struct LitSym {
    /// The name of the symbol.
    pub name: String,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl Parse for LitSym {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let token = input.try_parse::<Name>()?;
        Ok(Self {
            name: token.lexeme,
            span: token.span,
        })
    }
}

/// Represents a literal value in CalcScript.
///
/// A literal is any value that can is written directly into the source code. For example, the
/// number `1` is a literal (it is currently the only literal type supported by CalcScript).
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// A number literal. Integers and floating-point numbers are both supported and represented
    /// here as `f64`.
    Number(LitNum),

    /// A number written in radix notation. Radix notation allows users to express integers in a
    /// base other than base 10.
    Radix(LitRadix),

    /// A symbol / identifier literal. Symbols are used to represent variables and functions.
    Symbol(LitSym),
}

impl Literal {
    /// Returns the span of the literal.
    pub fn span(&self) -> Range<usize> {
        match self {
            Literal::Number(num) => num.span.clone(),
            Literal::Radix(radix) => radix.span.clone(),
            Literal::Symbol(name) => name.span.clone(),
        }
    }
}

impl Parse for Literal {
    fn parse(input: &mut Parser) -> Result<Self, Error> {
        let _ = try_parse_catch_fatal!(input.try_parse::<LitRadix>().map(Literal::Radix));
        let _ = try_parse_catch_fatal!(input.try_parse::<LitNum>().map(Literal::Number));
        try_parse_catch_fatal!(input.try_parse::<LitSym>().map(Literal::Symbol))
    }
}
