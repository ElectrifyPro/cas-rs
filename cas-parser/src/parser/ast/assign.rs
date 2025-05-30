use cas_error::Error;
use crate::{
    parser::{
        ast::{
            expr::{Atom, Expr, Primary},
            helper::Surrounded,
            index::Index,
            literal::{Literal, LitSym},
        },
        error::{
            CompoundAssignmentInHeader,
            DefaultArgumentNotLast,
            ExpectedExpr,
            InvalidAssignmentLhs,
            InvalidCompoundAssignmentLhs,
        },
        fmt::Latex,
        garbage::Garbage,
        token::{op::AssignOp, Comma, OpenParen},
        Parse,
        Parser,
        ParseResult,
    },
    tokenizer::TokenKind,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A parameter of a function declaration, such as `x` or `y = 1` in the declaration `f(x, y = 1) =
/// x^y`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Param {
    /// A parameter with no default value, such as `x` in `f(x) = x^2`.
    Symbol(LitSym),

    /// A parameter with a default value, such as `y = 1` in `f(x, y = 1) = x^y`.
    Default(LitSym, Expr),
}

impl Param {
    /// Returns the span of the parameter.
    pub fn span(&self) -> Range<usize> {
        match self {
            Param::Symbol(symbol) => symbol.span.clone(),
            Param::Default(symbol, default) => symbol.span.start..default.span().end,
        }
    }

    /// Returns the symbol of the parameter.
    pub fn symbol(&self) -> &LitSym {
        match self {
            Param::Symbol(symbol) => symbol,
            Param::Default(symbol, _) => symbol,
        }
    }

    /// Returns true if the parameter has a default value.
    pub fn has_default(&self) -> bool {
        matches!(self, Param::Default(_, _))
    }
}

impl<'source> Parse<'source> for Param {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let symbol = input.try_parse().forward_errors(recoverable_errors)?;

        if let Ok(assign) = input.try_parse::<AssignOp>().forward_errors(recoverable_errors) {
            if assign.is_compound() {
                recoverable_errors.push(Error::new(
                    vec![assign.span.clone()],
                    CompoundAssignmentInHeader,
                ));
            }
            let default = input.try_parse().forward_errors(recoverable_errors)?;
            Ok(Param::Default(symbol, default))
        } else {
            Ok(Param::Symbol(symbol))
        }
    }
}

impl std::fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Param::Symbol(symbol) => write!(f, "{}", symbol),
            Param::Default(symbol, default) => write!(f, "{} = {}", symbol, default),
        }
    }
}

impl Latex for Param {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Param::Symbol(symbol) => symbol.fmt_latex(f),
            Param::Default(symbol, default) => write!(f, "{} = {}", symbol.as_display(), default.as_display()),
        }
    }
}

/// A function header, **not including the body**. Functions can have multiple parameters with
/// optional default values, like in `f(x, y = 1)`. When a function with this header is called, the
/// default values are used (i.e. `y = 1`), unless the caller provides their own values (`f(2,
/// 3)`).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct FuncHeader {
    /// The name of the function.
    pub name: LitSym,

    /// The parameters of the function.
    pub params: Vec<Param>,

    /// The region of the source code that this function header was parsed from.
    pub span: Range<usize>,
}

impl FuncHeader {
    /// Returns the span of the function header.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    /// Attempts to parse a [`FuncHeader`], where the function name has already been parsed.
    fn parse_or_lower(
        input: &mut Parser,
        recoverable_errors: &mut Vec<Error>,
        name: LitSym,
    ) -> Result<Self, Vec<Error>> {
        /// Helper duplicate of the `Delimited` helper struct with additional state to ensure
        /// default parameters in the correct position.
        struct FuncHeaderInner {
            values: Vec<Param>,
        }

        impl<'source> Parse<'source> for FuncHeaderInner {
            fn std_parse(
                input: &mut Parser<'source>,
                recoverable_errors: &mut Vec<Error>
            ) -> Result<Self, Vec<Error>> {
                let mut bad_default_position = false;
                let mut default_params = Vec::new();
                let mut values = Vec::new();

                loop {
                    let Ok(value) = input.try_parse().forward_errors(recoverable_errors) else {
                        break;
                    };

                    // default parameters must be at the end of the list, i.e., no required
                    // parameters should come after
                    if !default_params.is_empty() && !bad_default_position {
                        if let Param::Symbol(_) = value {
                            bad_default_position = true;
                        }
                    }

                    if let Param::Default(_, _) = value {
                        default_params.push(value.span());
                    }

                    values.push(value);

                    if input.try_parse::<Comma>().forward_errors(recoverable_errors).is_err() {
                        break;
                    }
                }

                if bad_default_position {
                    recoverable_errors.push(Error::new(
                        default_params,
                        DefaultArgumentNotLast,
                    ));
                }

                Ok(Self { values })
            }
        }

        let surrounded = input.try_parse::<Surrounded<OpenParen, FuncHeaderInner>>()
            .forward_errors(recoverable_errors)?;

        let span = name.span.start..surrounded.close.span.end;
        Ok(Self { name, params: surrounded.value.values, span })
    }
}

impl std::fmt::Display for FuncHeader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        if let Some((last, rest)) = self.params.split_last() {
            for param in rest {
                write!(f, "{}, ", param)?;
            }
            write!(f, "{}", last)?;
        }
        write!(f, ")")
    }
}

impl Latex for FuncHeader {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\mathrm{{ {} }} \\left(", self.name.as_display())?;
        if let Some((last, rest)) = self.params.split_last() {
            for param in rest {
                param.fmt_latex(f)?;
                write!(f, ", ")?;
            }
            last.fmt_latex(f)?;
        }
        write!(f, "\\right)")
    }
}

/// An assignment target, such as `x`, `list[0]`, or `f(x)`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum AssignTarget {
    /// A symbol, such as `x`.
    Symbol(LitSym),

    /// A list index, such as `list[0]`.
    Index(Index),

    /// A function header, such as `f(x)`.
    Func(FuncHeader),
}

impl AssignTarget {
    /// Returns the span of the assignment target.
    pub fn span(&self) -> Range<usize> {
        match self {
            AssignTarget::Symbol(symbol) => symbol.span.clone(),
            AssignTarget::Index(index) => index.span(),
            AssignTarget::Func(func) => func.span(),
        }
    }

    /// Returns true if the assignment target is a function.
    pub fn is_func(&self) -> bool {
        matches!(self, AssignTarget::Func(_))
    }

    /// Tries to convert a general [`Expr`] into an [`AssignTarget`]. This is used when parsing
    /// assignment expressions, such as `x = 1` or `f(x) = x^2`.
    pub fn try_from_with_op(expr: Expr, op: &AssignOp) -> ParseResult<Self> {
        let op_span = op.span.clone();
        match expr {
            Expr::Literal(Literal::Symbol(symbol)) => ParseResult::Ok(AssignTarget::Symbol(symbol)),
            Expr::Index(index) => ParseResult::Ok(AssignTarget::Index(index)),
            Expr::Call(call) => {
                let spans = vec![call.span.clone(), op_span.clone()];
                let error = if op.is_compound() {
                    Error::new(spans, InvalidCompoundAssignmentLhs)
                } else {
                    Error::new(spans, InvalidAssignmentLhs { is_call: true })
                };

                ParseResult::Recoverable(Garbage::garbage(), vec![error])
            },
            expr => {
                let spans = vec![expr.span(), op_span.clone()];
                let error = if op.is_compound() {
                    Error::new(spans, InvalidCompoundAssignmentLhs)
                } else {
                    Error::new(spans, InvalidAssignmentLhs { is_call: false })
                };

                ParseResult::Recoverable(
                    Garbage::garbage(),
                    vec![error]
                )
            },
        }
    }
}

impl From<LitSym> for AssignTarget {
    fn from(symbol: LitSym) -> Self {
        AssignTarget::Symbol(symbol)
    }
}

impl From<Index> for AssignTarget {
    fn from(index: Index) -> Self {
        AssignTarget::Index(index)
    }
}

impl From<FuncHeader> for AssignTarget {
    fn from(func: FuncHeader) -> Self {
        AssignTarget::Func(func)
    }
}

impl<'source> Parse<'source> for AssignTarget {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        // this uses a similar approach to Primary::parse, where we try to parse an Atom first
        // and then check if it's followed by an open parenthesis or open square bracket
        // to determine if the target is a function or index
        let atom = input.try_parse::<Atom>().forward_errors(recoverable_errors)?;

        let mut fork = input.clone();
        match fork.next_token() {
            Ok(next) if next.kind == TokenKind::OpenParen => {
                if let Atom::Literal(Literal::Symbol(symbol)) = atom {
                    Ok(FuncHeader::parse_or_lower(input, recoverable_errors, symbol)
                        .map(Into::into)?)
                } else {
                    Err(vec![input.error(ExpectedExpr { expected: "a symbol" })])
                }
            },
            Ok(next) if next.kind == TokenKind::OpenSquare => {
                match Index::parse_or_lower(input, recoverable_errors, atom.into()) {
                    (new_primary, true) => match new_primary {
                        Primary::Index(index) => Ok(AssignTarget::Index(index)),
                        _ => unreachable!(),
                    },
                    (unchanged_primary, false) => match unchanged_primary {
                        Primary::Literal(Literal::Symbol(symbol)) => Ok(AssignTarget::Symbol(symbol)),
                        _ => unreachable!(),
                    },
                }
            },
            _ => if let Atom::Literal(Literal::Symbol(symbol)) = atom {
                Ok(AssignTarget::Symbol(symbol))
            } else {
                Err(vec![input.error(ExpectedExpr { expected: "a symbol" })])
            },
        }
    }
}

impl std::fmt::Display for AssignTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignTarget::Symbol(symbol) => write!(f, "{}", symbol),
            AssignTarget::Index(index) => write!(f, "{}", index),
            AssignTarget::Func(func) => write!(f, "{}", func),
        }
    }
}

impl Latex for AssignTarget {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignTarget::Symbol(symbol) => symbol.fmt_latex(f),
            AssignTarget::Index(index) => index.fmt_latex(f),
            AssignTarget::Func(func) => func.fmt_latex(f),
        }
    }
}

/// An assignment of a variable or function, such as `x = 1` or `f(x) = x^2`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Assign {
    /// The target to assign to.
    pub target: AssignTarget,

    /// The operator used to assign to the target.
    pub op: AssignOp,

    /// The expression to assign to the target.
    pub value: Box<Expr>,

    /// The region of the source code that this assignment expression was parsed from.
    pub span: Range<usize>,
}

impl Assign {
    /// Returns the span of the assignment expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Assign {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let target = input.try_parse().forward_errors(recoverable_errors)?;
        let op = input.try_parse::<AssignOp>().forward_errors(recoverable_errors)?;

        let value = if matches!(target, AssignTarget::Func(_)) {
            if op.is_compound() {
                // can't compound assignment to function, for example:
                //
                // f(x) += 5
                //      ^^
                recoverable_errors.push(Error::new(
                    vec![target.span(), op.span.clone()],
                    InvalidCompoundAssignmentLhs,
                ));
            }

            input.try_parse_with_state::<_, Expr>(|state| {
                // loop control not allowed inside a function definition inside a loop, for example:
                //
                // loop {
                //     f(x) = break x <-- illegal break
                //     f(5)
                // }
                state.allow_loop_control = false;
                state.allow_return = true;
            }).forward_errors(recoverable_errors)?
        } else {
            input.try_parse::<Expr>().forward_errors(recoverable_errors)?
        };

        let span = target.span().start..value.span().end;
        Ok(Self {
            target,
            op,
            value: Box::new(value),
            span,
        })
    }
}

impl std::fmt::Display for Assign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.target,
            self.op,
            self.value,
        )
    }
}

impl Latex for Assign {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.target.as_display(),
            self.op.as_display(),
            self.value.as_display(),
        )
    }
}
