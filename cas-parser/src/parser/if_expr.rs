use std::{fmt, ops::Range};
use super::{
    error::{kind, Error},
    expr::Expr,
    fmt::Latex,
    literal::{Literal, LitNum},
    keyword::{Else, If as IfToken, Then},
    Parse,
    Parser,
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// An `if` expression, such as `if true 1 else 2`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct If {
    /// The condition of the `if` expression.
    pub condition: Box<Expr>,

    /// The expression to evaluate if the condition is true.
    pub then_expr: Box<Expr>,

    /// The expression to evaluate if the condition is false.
    pub else_expr: Box<Expr>,

    /// The region of the source code that this literal was parsed from.
    pub span: Range<usize>,

    /// The span of the `if` keyword.
    pub if_span: Range<usize>,

    /// The span of the `then` keyword.
    pub then_span: Range<usize>,

    /// The span of the `else` keyword.
    pub else_span: Range<usize>,
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
        let condition = input.try_parse::<Expr>().forward_errors(recoverable_errors)?;
        let (then_token, then_expr) = 'then: {
            let then_token = match input.try_parse::<Then>().forward_errors(recoverable_errors) {
                Ok(token) => token,
                Err(_) => {
                    recoverable_errors.push(Error::new(
                        vec![if_token.span.clone(), input.current_token().unwrap().span.clone()],
                        kind::MissingIfKeyword {
                            keyword: "then",
                        },
                    ));
                    break 'then (Then {
                        lexeme: "",
                        span: 0..0,
                    }, Expr::Literal(Literal::Number(LitNum {
                        value: String::new(),
                        span: 0..0,
                    })));
                },
            };
            let then_expr = match input.try_parse::<Expr>().forward_errors(recoverable_errors) {
                Ok(expr) => expr,
                Err(_) => {
                    recoverable_errors.push(Error::new(
                        vec![if_token.span.clone(), input.current_token().unwrap().span.clone()],
                        kind::MissingIfBranch {
                            keyword: "then",
                        },
                    ));
                    Expr::Literal(Literal::Number(LitNum {
                        value: String::new(),
                        span: 0..0,
                    }))
                },
            };
            (then_token, then_expr)
        };
        let (else_token, else_expr) = 'else_branch: {
            let else_token = match input.try_parse::<Else>().forward_errors(recoverable_errors) {
                Ok(token) => token,
                Err(_) => {
                    recoverable_errors.push(Error::new(
                        vec![if_token.span.clone(), input.current_token().unwrap().span.clone()],
                        kind::MissingIfKeyword {
                            keyword: "else",
                        },
                    ));
                    break 'else_branch (Else {
                        lexeme: "",
                        span: 0..0,
                    }, Expr::Literal(Literal::Number(LitNum {
                        value: String::new(),
                        span: 0..0,
                    })));
                },
            };
            let else_expr = match input.try_parse::<Expr>().forward_errors(recoverable_errors) {
                Ok(expr) => expr,
                Err(_) => {
                    recoverable_errors.push(Error::new(
                        vec![if_token.span.clone(), input.current_token().unwrap().span.clone()],
                        kind::MissingIfBranch {
                            keyword: "else",
                        },
                    ));
                    Expr::Literal(Literal::Number(LitNum {
                        value: String::new(),
                        span: 0..0,
                    }))
                },
            };
            (else_token, else_expr)
        };
        let span = if_token.span.start..else_expr.span().end;

        Ok(Self {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
            span,
            if_span: if_token.span,
            then_span: then_token.span,
            else_span: else_token.span,
        })
    }
}

impl Latex for If {
    fn fmt_latex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\\text{{if }}")?;
        self.condition.fmt_latex(f)?;
        write!(f, "\\text{{ then }}")?;
        self.then_expr.fmt_latex(f)?;
        write!(f, "\\text{{ else }}")?;
        self.else_expr.fmt_latex(f)?;
        Ok(())
    }
}
