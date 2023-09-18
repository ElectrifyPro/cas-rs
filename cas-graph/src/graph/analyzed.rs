use cas_parser::parser::{binary::Binary, expr::Expr, literal::Literal, token::op::{BinOp, BinOpKind}};
use std::collections::HashSet;

/// Predicts the independent variable of the given expression.
pub fn predict_independent(expr: &Expr) -> Variable {
    if let Expr::Binary(Binary {
        lhs,
        op: BinOp {
            kind: BinOpKind::Eq,
            ..
        },
        rhs,
        ..
    }) = expr {
        // variable by itself on left-hand-side: y=..., x=..., etc.
        // could indicate: y=x^2, x=y^2, etc.
        if let Expr::Literal(Literal::Symbol(sym)) = lhs.as_ref() {
            return match sym.name.as_str() {
                "x" => Variable::Y,
                "y" => Variable::X,
                "theta" => todo!("polar coordinates"),
                _ => todo!("unknown special variable"),
            };
        } else if let Expr::Literal(Literal::Symbol(sym)) = rhs.as_ref() { // TODO: please stablize let chains!
            return match sym.name.as_str() {
                "x" => Variable::Y,
                "y" => Variable::X,
                "theta" => todo!("polar coordinates"),
                _ => todo!("unknown special variable"),
            };
        }

        // fall back to basic symbol counting
    }

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

    if vars.len() == 1 {
        vars.into_iter().next().unwrap()
    } else {
        todo!("multiple special variables")
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
}

impl AnalyzedExpr {
    /// Analyze the given expression and return a new [`AnalyzedExpr`].
    pub fn new(expr: Expr) -> Self {
        let independent = predict_independent(&expr);
        Self {
            expr,
            independent,
        }
    }
}
