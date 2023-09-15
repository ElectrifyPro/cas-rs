use super::{
    assign::AssignTarget,
    expr::Expr,
    literal::{LitSym, Literal},
    paren::Paren,
};

/// A trait for producing garbage values, useful for recovering from parsing errors.
///
/// We could've implemented [`Default`] on types instead, but garbage values are not useful to the
/// end user, and we don't want to encourage its use due to [`Default`] being implemented.
pub(crate) trait Garbage {
    /// Produces a garbage value.
    fn garbage() -> Self;
}

/// Implements [`Garbage`] for tuples.
macro_rules! garbage_tuple {
    ($($ty:ident),*) => {
        impl<$($ty: Garbage),*> Garbage for ($($ty,)*) {
            fn garbage() -> Self {
                ($($ty::garbage(),)*)
            }
        }
    };
}

garbage_tuple!(A, B);

impl<T: Garbage, E> Garbage for Result<T, E> {
    fn garbage() -> Self {
        Ok(T::garbage())
    }
}

impl Garbage for AssignTarget {
    fn garbage() -> Self {
        AssignTarget::Symbol(LitSym::garbage())
    }
}

impl Garbage for Expr {
    fn garbage() -> Self {
        Expr::Literal(Literal::Symbol(LitSym::garbage()))
    }
}

impl Garbage for LitSym {
    fn garbage() -> Self {
        Self { name: String::new(), span: 0..0 }
    }
}

impl Garbage for Paren {
    fn garbage() -> Self {
        Self { expr: Box::new(Expr::garbage()), span: 0..0 }
    }
}
