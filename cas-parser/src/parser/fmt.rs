use std::fmt::{Display, Formatter, Result};
use super::{ast::expr::Expr, token::op::BinOpKind};

/// A trait for types that can be formatted as LaTeX.
pub trait Latex {
    /// Format the value as LaTeX.
    fn fmt_latex(&self, f: &mut Formatter) -> Result;

    /// Wraps the value in a [`LatexFormatter`], which implements [`Display`].
    fn as_display(&self) -> LatexFormatter<'_, Self> {
        LatexFormatter(self)
    }
}

/// A wrapper type that implements [`Display`] for any type that implements [`Latex`].
pub struct LatexFormatter<'a, T: ?Sized>(&'a T);

impl<T: ?Sized> Display for LatexFormatter<'_, T>
where
    T: Latex,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.0.fmt_latex(f)
    }
}

/// Helper to format powers.
pub fn fmt_pow(f: &mut Formatter, left: Option<&Expr>, right: Option<&Expr>) -> Result {
    if let Some(left) = left {
        let left = left.innermost();
        let mut insert_with_paren = || {
            write!(f, "\\left(")?;
            left.fmt_latex(f)?;
            write!(f, "\\right)")
        };

        // all of these are separate match arms instead of a single match arm with multiple
        // patterns, because apparently that can't be parsed correctly
        match left {
            Expr::Unary(unary)
                if unary.op.precedence() <= BinOpKind::Exp.precedence() => insert_with_paren(),
            // NOTE: exp is the highest precedence binary operator, so this check is not necessary,
            // but is just here for completeness
            Expr::Binary(binary)
                if binary.op.precedence() <= BinOpKind::Exp.precedence() => insert_with_paren(),
            Expr::Call(call) if call.name.name == "pow" => insert_with_paren(),
            _ => left.fmt_latex(f),
        }?
    }
    write!(f, "^{{")?;
    if let Some(right) = right {
        right.innermost().fmt_latex(f)?;
    }
    write!(f, "}}")
}
