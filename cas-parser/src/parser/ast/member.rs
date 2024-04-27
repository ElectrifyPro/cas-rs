use cas_error::Error;
use crate::parser::{
    ast::expr::{Atom, Expr, Primary},
    fmt::Latex,
    token::Dot,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Accessing a member of a structure, such as the `list.len` in `list.len()`, or `a.b.c`.
///
/// [`Parse`]: crate::parser::Parse
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Member {
    /// The path to the member.
    pub target: Vec<Expr>,

    /// The region of the source code that this member access was parsed from.
    pub span: Range<usize>,
}

impl Member {
    /// Returns the span of the member access.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    /// Attempts to parse an [`Member`], where the first path member has already been parsed.
    pub fn parse_or_lower(
        input: &mut Parser,
        recoverable_errors: &mut Vec<Error>,
        mut target: Primary,
    ) -> Primary {
        // parse the dot first
        while input.try_parse::<Dot>().forward_errors(recoverable_errors).is_ok() {
            let member: Expr = match input.try_parse::<Atom>().forward_errors(recoverable_errors) {
                Ok(atom) => atom.into(),
                Err(_) => {
                    // TODO: report error: required atom
                    break;
                },
            };
            let span = target.span().start..member.span().end;

            // iteratively search for adjacent members in the path
            target = match target {
                Primary::Member(Member { mut target, .. }) => {
                    target.push(member);
                    Primary::Member(Member { target, span })
                },
                _ => {
                    let target = vec![target.into(), member];
                    Primary::Member(Member { target, span })
                },
            };
        }

        target
    }
}

impl std::fmt::Display for Member {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (last, chain) = self.target.split_last().unwrap();
        for expr in chain {
            write!(f, "{}.", expr)?;
        }
        write!(f, "{}", last)
    }
}

impl Latex for Member {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (last, chain) = self.target.split_last().unwrap();
        for expr in chain {
            expr.fmt_latex(f)?;
            write!(f, ".")?;
        }
        last.fmt_latex(f)
    }
}
