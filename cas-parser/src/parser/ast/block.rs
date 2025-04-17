use cas_error::Error;
use crate::parser::{
    ast::stmt::Stmt,
    error::UnclosedParenthesis,
    fmt::Latex,
    garbage::Garbage,
    token::{CloseCurly, OpenCurly},
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A blocked expression. A [`Block`] can contain multiple expressions in the form of statements.
/// The last statement in the block is the return value of the block.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Block {
    /// The inner statements.
    pub stmts: Vec<Stmt>,

    /// The region of the source code that this [`Block`] was parsed from.
    pub span: Range<usize>,
}

impl Block {
    /// Returns the span of the [`Block`].
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for Block {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let open_curly = input.try_parse::<OpenCurly>().forward_errors(recoverable_errors)?;
        let mut stmts = Vec::new();
        while let Ok(stmt) = input.try_parse().forward_errors(recoverable_errors) {
            stmts.push(stmt);
        }
        let close_curly = input.try_parse::<CloseCurly>()
            .forward_errors(recoverable_errors)
            .unwrap_or_else(|_| {
                recoverable_errors.push(Error::new(
                    vec![open_curly.span.clone()],
                    UnclosedParenthesis { opening: true },
                ));

                // fake a close paren for recovery purposes
                Garbage::garbage()
            });
        Ok(Self {
            stmts,
            span: open_curly.span.start..close_curly.span.end,
        })
    }
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{")?;
        for stmt in &self.stmts {
            stmt.fmt(f)?;
        }
        write!(f, "}}")
    }
}

impl Latex for Block {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        for stmt in &self.stmts {
            stmt.fmt_latex(f)?;
        }
        write!(f, "}}")
    }
}
