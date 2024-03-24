use crate::parser::{
    ast::expr::Expr,
    error::{kind, Error},
    fmt::Latex,
    garbage::Garbage,
    keyword::{Then, While as WhileToken},
    Parse,
    Parser,
};
use std::{fmt, ops::Range};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A `while` loop expression, such as `while x < 10 then x += 1`. The loop body is executed
/// repeatedly as long as the outer condition is true.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct While {
    /// The condition that must be true for the loop body to be executed.
    pub condition: Box<Expr>,

    /// The body of the loop.
    pub body: Box<Expr>,

    /// The region of the source code that this expression was parsed from.
    pub span: Range<usize>,

    /// The span of the `while` keyword.
    pub while_span: Range<usize>,

    /// The span of the `then` keyword.
    pub then_span: Range<usize>,
}

impl While {
    /// Returns the span of the `while` loop expression.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }
}

impl<'source> Parse<'source> for While {
    fn std_parse(
        input: &mut Parser<'source>,
        recoverable_errors: &mut Vec<Error>
    ) -> Result<Self, Vec<Error>> {
        let while_token = input.try_parse::<WhileToken>().forward_errors(recoverable_errors)?;
        let condition = input.try_parse().forward_errors(recoverable_errors)?;
        let (then_token, body) = 'then: {
            let then_token = match input.try_parse::<Then>().forward_errors(recoverable_errors) {
                Ok(token) => token,
                Err(_) => {
                    // TODO: add error for missing `then` keyword
                    recoverable_errors.push(Error::new(
                        vec![while_token.span.clone(), input.span()],
                        kind::MissingIfKeyword {
                            keyword: "then",
                        },
                    ));
                    break 'then Garbage::garbage();
                },
            };
            input.try_parse_with_state::<_, Expr>(|state| {
                state.loop_depth += 1;
            })
                .forward_errors(recoverable_errors)
                .map(|expr| (then_token, expr))
                .unwrap_or_else(|_| {
                    recoverable_errors.push(Error::new(
                        vec![while_token.span.clone(), input.span()],
                        kind::MissingIfBranch {
                            keyword: "then",
                        },
                    ));
                    Garbage::garbage()
                })
        };
        let span = while_token.span.start..body.span().end;

        Ok(Self {
            condition: Box::new(condition),
            body: Box::new(body),
            span,
            while_span: while_token.span,
            then_span: then_token.span,
        })
    }
}

impl std::fmt::Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "while {} then {}", self.condition, self.body)
    }
}

impl Latex for While {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{while }}")?;
        self.condition.fmt_latex(f)?;
        write!(f, "\\text{{ then }}")?;
        self.body.fmt_latex(f)?;
        Ok(())
    }
}
