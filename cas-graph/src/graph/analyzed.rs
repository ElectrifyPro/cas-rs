use cas_error::Error;
use cas_parser::parser::{
    ast::{binary::Binary, expr::Expr, literal::Literal},
    token::op::{BinOp, BinOpKind},
    Parser,
};
use std::collections::HashSet;

/// Predicts and extracts the independent variable from the given expression.
pub fn predict_independent(expr: Expr) -> (Variable, Expr) {
    let expr = if let Expr::Binary(Binary {
        lhs,
        op: BinOp {
            kind: BinOpKind::Eq,
            implicit,
            span: op_span,
        },
        rhs,
        span,
    }) = expr {
        // variable by itself on left-hand-side: y=..., x=..., etc.
        // could indicate: y=x^2, x=y^2, etc.
        if let Expr::Literal(Literal::Symbol(sym)) = lhs.as_ref() {
            return match sym.name.as_str() {
                "x" => (Variable::Y, *rhs),
                "y" => (Variable::X, *rhs),
                "theta" => (Variable::Theta, *rhs),
                _ => todo!("unknown special variable"),
            };
        } else if let Expr::Literal(Literal::Symbol(sym)) = rhs.as_ref() {
            return match sym.name.as_str() {
                "x" => (Variable::Y, *lhs),
                "y" => (Variable::X, *lhs),
                "theta" => (Variable::Theta, *lhs),
                _ => todo!("unknown special variable"),
            };
        }

        // recreate the binary to allow fallling back to basic symbol counting
        Expr::Binary(Binary {
            lhs,
            op: BinOp {
                kind: BinOpKind::Eq,
                implicit,
                span: op_span,
            },
            rhs,
            span,
        })
    } else {
        expr
    };

    let vars = expr.post_order_iter()
        .filter_map(|node| match node {
            Expr::Literal(Literal::Symbol(sym)) => match sym.name.as_str() {
                "x" => Some(Variable::X),
                "y" => Some(Variable::Y),
                "theta" => Some(Variable::Theta),
                _ => None,
            },
            _ => None,
        })
        .collect::<HashSet<_>>();

    if vars.len() > 1 {
        todo!("multiple special variables")
    } else if let Some(var) = vars.into_iter().next() {
        (var, expr)
    } else {
        todo!("no special variables")
    }
}

/// Special variable names that are used in expressions to be graphed.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Variable {
    X,
    Y,
    Theta,
}

impl Variable {
    /// Returns the string slice representation of the variable.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::X => "x",
            Self::Y => "y",
            Self::Theta => "theta",
        }
    }
}

/// An expression that has been analyzed and ready to be drawn.
#[derive(Clone, Debug)]
pub struct AnalyzedExpr {
    /// The expression to draw.
    pub expr: Expr,

    /// The independent variable.
    pub independent: Variable,

    /// The color of the expression, given as an RGB tuple with each value in the range 0.0 to 1.0.
    ///
    /// The default color is a solid red.
    pub color: (f64, f64, f64),
}

impl AnalyzedExpr {
    /// Analyze the given expression and return a new [`AnalyzedExpr`].
    pub fn new(expr: Expr) -> Self {
        let (independent, expr) = predict_independent(expr);
        Self {
            expr,
            independent,
            color: (1.0, 0.0, 0.0),
        }
    }

    /// Parses and analyzes the given expression and returns a new [`AnalyzedExpr`].
    pub fn parse(expr: &str) -> Result<Self, Vec<Error>> {
        Parser::new(expr)
            .try_parse_full()
            .map(Self::new)
    }

    /// Sets the color of the expression.
    ///
    /// Returns the expression itself to allow chaining.
    pub fn with_color(mut self, color: (f64, f64, f64)) -> Self {
        self.color = color;
        self
    }
}
