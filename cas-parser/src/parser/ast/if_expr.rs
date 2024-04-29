use crate::parser::{
    ast::expr::{Atom, Expr},
    error::{kind, Error},
    fmt::Latex,
    garbage::Garbage,
    keyword::{Else, If as IfToken},
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// An `if` expression, such as `if true 1 else 2`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct If {
    /// The condition of the `if` expression.
    pub condition: Box<Expr>,

    /// The expression to evaluate if the condition is true.
    pub then_expr: Box<Expr>,

    /// The expression to evaluate if the condition is false.
    pub else_expr: Option<Box<Expr>>,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,

    /// The span of the `if` keyword.
    pub if_span: Range<usize>,

    /// The span of the `else` keyword.
    pub else_span: Option<Range<usize>>,
}

impl If {
    /// Returns the span of the `if` expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for If {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let if_token = input.try_parse::<IfToken>().forward_errors(recoverable_errors)?;
        let condition = input.try_parse().forward_errors(recoverable_errors)?;
        let then_expr = input.try_parse_with_state::<_, Atom>(|input| {
            input.allow_then = true;
        })
            .map(Expr::from)
            .forward_errors(recoverable_errors)?;
        let (else_token, else_expr) = 'else_branch: {
            let Ok(else_token) = input.try_parse::<Else>().forward_errors(recoverable_errors) else {
                break 'else_branch (None, None);
            };
            input.try_parse::<Expr>()
                .forward_errors(recoverable_errors)
                .map(|expr| (Some(else_token), Some(expr)))
                .unwrap_or_else(|_| {
                    recoverable_errors.push(Error::new(
                        vec![if_token.span.clone(), input.span()],
                        kind::MissingIfBranch {
                            keyword: "else",
                        },
                    ));
                    Garbage::garbage()
                })
        };
        let span = if let Some(else_expr) = &else_expr {
            if_token.span.start..else_expr.span().end
        } else {
            if_token.span.start..then_expr.span().end
        };

        Ok(Self {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: else_expr.map(Box::new),
            span,
            if_span: if_token.span,
            else_span: else_token.map(|token| token.span),
        })
    }
}

impl std::fmt::Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "if ")?;
        self.condition.fmt(f)?;
        self.then_expr.fmt(f)?;
        if let Some(else_expr) = &self.else_expr {
            write!(f, " else ")?;
            else_expr.fmt(f)?;
        }
        Ok(())
    }
}

impl Latex for If {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{if }}")?;
        self.condition.fmt_latex(f)?;
        self.then_expr.fmt_latex(f)?;
        if let Some(else_expr) = &self.else_expr {
            write!(f, "\\text{{ else }}")?;
            else_expr.fmt_latex(f)?;
        }
        Ok(())
    }
}
