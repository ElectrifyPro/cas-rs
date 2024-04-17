use crate::{
    parser::{
        ast::{expr::Expr, helper::{SquareDelimited, Surrounded}},
        error::{kind, Error},
        fmt::Latex,
        token::{
            Boolean,
            CloseParen,
            Float,
            Name,
            Int,
            OpenParen,
            OpenSquare,
            Semicolon,
            Quote,
        },
        Parse,
        Parser,
        ParseResult,
    },
    tokenizer::TokenKind,
    return_if_ok,
};
use std::{collections::HashSet, fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// An integer literal, representing as a [`String`].
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LitInt {
    /// The value of the integer literal as a string.
    pub value: String,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl<'source> Parse<'source> for LitInt {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        input
            .try_parse::<Int>()
            .map(|int| Self {
                value: int.lexeme.to_owned(),
                span: int.span,
            })
            .forward_errors(recoverable_errors)
    }
}

impl std::fmt::Display for LitInt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Latex for LitInt {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// A floating-point literal, represented as a [`String`].
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LitFloat {
    /// The value of the floating-point literal as a string.
    pub value: String,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl<'source> Parse<'source> for LitFloat {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        input
            .try_parse::<Float>()
            .map(|float| Self {
                value: float.lexeme.to_owned(),
                span: float.span,
            })
            .forward_errors(recoverable_errors)
    }
}

impl std::fmt::Display for LitFloat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Latex for LitFloat {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
    match num.lexeme.parse() {
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
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
        let num = input.try_parse().forward_errors(recoverable_errors)?;
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

impl std::fmt::Display for LitRadix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}'{}", self.base, self.value)
    }
}

impl Latex for LitRadix {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}'{}", self.base, self.value)
    }
}

/// A boolean literal, either `true` or `false`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LitBool {
    /// The value of the boolean literal.
    pub value: bool,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl<'source> Parse<'source> for LitBool {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        input.try_parse::<Boolean>()
            .map(|boolean| Self {
                value: match boolean.lexeme {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                },
                span: boolean.span,
            })
            .forward_errors(recoverable_errors)
    }
}

impl std::fmt::Display for LitBool {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Latex for LitBool {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// A symbol / identifier literal. Symbols are used to represent variables and functions.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
        // TODO: it would be nice if we could report an error if the symbol is a keyword
        //
        // for example:
        // break(x) = x
        // ^^^^^ error: `break` is a keyword and cannot be used as a symbol
        //
        // unfortunately this is hard since CalcScript is context-sensitive and we would have to
        // to parse further ahead to determine if this error should be reported
        // maybe we should require a `let` keyword to declare variables?
        input.try_parse::<Name>()
            .map(|name| Self {
                name: name.lexeme.to_owned(),
                span: name.span,
            })
            .forward_errors(recoverable_errors)
    }
}

impl std::fmt::Display for LitSym {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Latex for LitSym {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.name.as_str() {
            "tau" | "pi" | "phi" | "theta" => write!(f, "\\{} ", self.name),
            _ => write!(f, "{}", self.name),
        }
    }
}

/// The unit type, written as `()`. The unit type is by-default returned by functions that do not
/// return a value.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LitUnit {
    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl<'source> Parse<'source> for LitUnit {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let open = input.try_parse::<OpenParen>().forward_errors(recoverable_errors)?;
        let close = input.try_parse::<CloseParen>().forward_errors(recoverable_errors)?;
        Ok(Self {
            span: open.span.start..close.span.end,
        })
    }
}

impl std::fmt::Display for LitUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "()")
    }
}

impl Latex for LitUnit {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "()")
    }
}

/// The list type, consisting of a list of expressions surrounded by square brackets and delimited by
/// commas: `[expr1, expr2, ...]`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LitList {
    /// The list of expressions.
    pub values: Vec<Expr>,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl<'source> Parse<'source> for LitList {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let surrounded = input.try_parse::<SquareDelimited<_>>().forward_errors(recoverable_errors)?;

        Ok(Self {
            values: surrounded.value.values,
            span: surrounded.open.span.start..surrounded.close.span.end,
        })
    }
}

impl std::fmt::Display for LitList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, value) in self.values.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", value)?;
        }
        write!(f, "]")
    }
}

impl Latex for LitList {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, value) in self.values.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            value.fmt_latex(f)?;
        }
        write!(f, "]")
    }
}

/// The list type, formed by repeating the given expression `n` times: `[expr; n]`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LitListRepeat {
    /// The expression to repeat.
    pub value: Box<Expr>,

    /// The number of times to repeat the expression.
    pub count: Box<Expr>,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,
}

impl<'source> Parse<'source> for LitListRepeat {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        /// Inner struct representing the contents of a repeated list so that we can use the
        /// [`Surrounded`] helper with it.
        #[derive(Debug)]
        struct LitListRepeatInner {
            /// The expression to repeat.
            value: Expr,

            /// The number of times to repeat the expression.
            count: Expr,
        }

        impl<'source> Parse<'source> for LitListRepeatInner {
            fn std_parse(
                input: &mut Parser<'source>,
                recoverable_errors: &mut Vec<Error>
            ) -> Result<Self, Vec<Error>> {
                let value = input.try_parse().forward_errors(recoverable_errors)?;
                input.try_parse::<Semicolon>().forward_errors(recoverable_errors)?;
                let count = input.try_parse().forward_errors(recoverable_errors)?;
                Ok(Self { value, count })
            }
        }

        let inner = input.try_parse::<Surrounded<OpenSquare, LitListRepeatInner>>().forward_errors(recoverable_errors)?;

        Ok(Self {
            value: Box::new(inner.value.value),
            count: Box::new(inner.value.count),
            span: inner.open.span.start..inner.close.span.end,
        })
    }
}

impl std::fmt::Display for LitListRepeat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}; {}]", self.value, self.count)
    }
}

impl Latex for LitListRepeat {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}; {}]", self.value, self.count)
    }
}

/// Represents a literal value in CalcScript.
///
/// A literal is any value that can is written directly into the source code. For example, the
/// number `1` is a literal (it is currently the only literal type supported by CalcScript).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Literal {
    /// An integer literal.
    Integer(LitInt),

    /// A floating-point literal.
    Float(LitFloat),

    /// A number written in radix notation. Radix notation allows users to express integers in a
    /// base other than base 10.
    Radix(LitRadix),

    /// A boolean literal, either `true` or `false`.
    Boolean(LitBool),

    /// A symbol / identifier literal. Symbols are used to represent variables and functions.
    Symbol(LitSym),

    /// The unit type, written as `()`. The unit type is by-default returned by functions that do
    /// not return a value.
    Unit(LitUnit),

    /// The list type, consisting of a list of expressions surrounded by square brackets and
    /// delimited by commas: `[expr1, expr2, ...]`.
    List(LitList),

    /// The list type, formed by repeating the given expression `n` times: `[expr; n]`.
    ListRepeat(LitListRepeat),
}

impl Literal {
    /// Returns the span of the literal.
    pub fn span(&self) -> Range<usize> {
        match self {
            Literal::Integer(int) => int.span.clone(),
            Literal::Float(float) => float.span.clone(),
            Literal::Radix(radix) => radix.span.clone(),
            Literal::Boolean(boolean) => boolean.span.clone(),
            Literal::Symbol(name) => name.span.clone(),
            Literal::Unit(unit) => unit.span.clone(),
            Literal::List(list) => list.span.clone(),
            Literal::ListRepeat(repeat) => repeat.span.clone(),
        }
    }
}

impl<'source> Parse<'source> for Literal {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let _ = return_if_ok!(input.try_parse().map(Literal::Boolean).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Literal::Radix).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Literal::Integer).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Literal::Float).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Literal::Symbol).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Literal::Unit).forward_errors(recoverable_errors));
        let _ = return_if_ok!(input.try_parse().map(Literal::List).forward_errors(recoverable_errors));
        input.try_parse().map(Literal::ListRepeat).forward_errors(recoverable_errors)
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Integer(int) => int.fmt(f),
            Literal::Float(float) => float.fmt(f),
            Literal::Radix(radix) => radix.fmt(f),
            Literal::Boolean(boolean) => boolean.fmt(f),
            Literal::Symbol(name) => name.fmt(f),
            Literal::Unit(unit) => unit.fmt(f),
            Literal::List(list) => list.fmt(f),
            Literal::ListRepeat(repeat) => repeat.fmt(f),
        }
    }
}

impl Latex for Literal {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Integer(int) => int.fmt_latex(f),
            Literal::Float(float) => float.fmt_latex(f),
            Literal::Radix(radix) => radix.fmt_latex(f),
            Literal::Boolean(boolean) => boolean.fmt_latex(f),
            Literal::Symbol(name) => name.fmt_latex(f),
            Literal::Unit(unit) => unit.fmt_latex(f),
            Literal::List(list) => list.fmt_latex(f),
            Literal::ListRepeat(repeat) => repeat.fmt_latex(f),
        }
    }
}
