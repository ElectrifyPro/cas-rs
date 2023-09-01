use crate::{
    parser::{
        error::{kind, Error},
        token::{Float, Name, Int, Quote},
        Parse,
        Parser,
        ParseResult,
    },
    tokenizer::TokenKind,
    return_if_ok,
};
use std::{collections::HashSet, ops::Range};

/// A number literal. Integers and floating-point numbers are both supported and represented here
/// as a [`String`].
#[derive(Debug, Clone, PartialEq)]
pub struct LitNum {
    /// The value of the number literal as a string.
    pub value: String,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl<'source> Parse<'source> for LitNum {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let (lexeme, span) = input
            .try_parse::<Int>()
            .map(|num| (num.lexeme, num.span))
            .or_else(|| input.try_parse::<Float>().map(|num| (num.lexeme, num.span)))
            .forward_errors(recoverable_errors)?;
        Ok(Self {
            value: lexeme.to_owned(),
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

impl RadixWord {
    fn parse(input: &mut Parser) -> Self {
        let mut value = String::new();
        let mut span = 0..0;
        while let Ok(token) = input.next_token_raw() {
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

        Self {
            value,
            span,
        }
    }
}

/// Helper function to ensure the given string represents a valid base for radix notation.
fn validate_radix_base(num: &Int) -> ParseResult<u8> {
    match num.lexeme.parse::<u8>() {
        Ok(base) if (2..=64).contains(&base) => ParseResult::Ok(base),
        Ok(base) if base < 2 => ParseResult::Recoverable(
            64, // use base 64 to limit invalid radix digit errors
            vec![Error::new(vec![num.span.clone()], kind::InvalidRadixBase { too_large: false })],
        ),
        _ => ParseResult::Recoverable(
            64,
            vec![Error::new(vec![num.span.clone()], kind::InvalidRadixBase { too_large: true })],
        ),
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

impl<'source> Parse<'source> for LitRadix {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let num = input.try_parse::<Int>().forward_errors(recoverable_errors)?;
        let quote = input.try_parse::<Quote>().forward_errors(recoverable_errors)?;

        let base = validate_radix_base(&num).forward_errors(recoverable_errors)?;
        let word = RadixWord::parse(input);
        if word.value.is_empty() {
            recoverable_errors.push(Error::new(vec![quote.span], kind::EmptyRadixLiteral {
                radix: base,
                allowed: &DIGITS[..base as usize],
            }));
        }

        // ensure that the number is valid for this radix
        let allowed_digits = &DIGITS[..base as usize];
        let mut bad_digits = HashSet::new();
        let mut bad_digit_spans: Vec<Range<usize>> = Vec::new();
        for (i, c) in word.value.chars().enumerate() {
            // if we find a digit that isn't allowed, that is fatal
            // but continue to find all the bad digits so we can report them all at once
            if !allowed_digits.contains(&c) {
                let char_start = word.span.start + i;
                if let Some(last_span) = bad_digit_spans.last_mut() {
                    // merge adjacent spans
                    if last_span.end == char_start {
                        last_span.end += 1;
                    } else {
                        bad_digit_spans.push(char_start..char_start + 1);
                    }
                } else {
                    bad_digit_spans.push(char_start..char_start + 1);
                }

                bad_digits.insert(c);
                continue;
            }
        }

        if !bad_digit_spans.is_empty() {
            recoverable_errors.push(Error::new(bad_digit_spans, kind::InvalidRadixDigit {
                radix: base,
                allowed: allowed_digits,
                digits: bad_digits,
                last_op_digit: {
                    if let Some(ch) = word.value.chars().last() {
                        ['+', '/'].into_iter()
                            .find(|&op| op == ch)
                            .map(|op| (op, word.span.end - 1..word.span.end))
                    } else {
                        None
                    }
                },
            }));
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

impl<'source> Parse<'source> for LitSym {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        input.try_parse::<Name>()
            .map(|name| {
                if matches!(name.lexeme, "if" | "then" | "else") {
                    recoverable_errors.push(Error::new(vec![name.span.clone()], kind::ExpectedSymbolName {
                        keyword: name.lexeme.to_owned(),
                    }));
                }
                Self {
                    name: name.lexeme.to_owned(),
                    span: name.span,
                }
            })
            .forward_errors(recoverable_errors)
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

impl<'source> Parse<'source> for Literal {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let _ = return_if_ok!(input.try_parse::<LitRadix>().map(Literal::Radix).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse::<LitNum>().map(Literal::Number).forward_errors(recoverable_errors));
        input.try_parse::<LitSym>().map(Literal::Symbol).forward_errors(recoverable_errors)
    }
}
