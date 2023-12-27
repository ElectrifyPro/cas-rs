use super::Expr;

/// An iterator that iteratively traverses the tree of expressions in left-to-right post-order
/// (i.e. depth-first).
///
/// This iterator is created by [`Expr::post_order_iter`].
pub struct ExprIter<'a> {
    stack: Vec<&'a Expr>,
    last_visited: Option<&'a Expr>,
}

impl<'a> ExprIter<'a> {
    /// Creates a new iterator that traverses the tree of expressions in left-to-right post-order
    /// (i.e. depth-first).
    pub fn new(expr: &'a Expr) -> Self {
        Self {
            stack: vec![expr],
            last_visited: None,
        }
    }

    /// Pops the current expression in the stack and marks it as the last visited expression.
    fn visit(&mut self) -> Option<&'a Expr> {
        self.last_visited = Some(self.stack.pop()?);
        self.last_visited
    }

    /// Returns true if the given expression matches the last visited expression.
    fn is_last_visited(&self, expr: &'a Expr) -> bool {
        match self.last_visited {
            Some(last_visited) => std::ptr::eq(last_visited, expr),
            None => false,
        }
    }
}

impl<'a> Iterator for ExprIter<'a> {
    type Item = &'a Expr;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let expr = self.stack.last()?;
            match expr {
                Expr::Primary(_) => return self.visit(),
                Expr::Add(terms) => {
                    if terms.is_empty() || self.is_last_visited(terms.last().unwrap()) {
                        return self.visit();
                    }
                    for term in terms.iter().rev() {
                        self.stack.push(term);
                    }
                },
                Expr::Mul(factors) => {
                    if factors.is_empty() || self.is_last_visited(factors.last().unwrap()) {
                        return self.visit();
                    }
                    for factor in factors.iter().rev() {
                        self.stack.push(factor);
                    }
                },
                Expr::Exp(lhs, rhs) => {
                    if self.is_last_visited(rhs) {
                        return self.visit();
                    }
                    self.stack.push(rhs);
                    self.stack.push(lhs);
                },
            }
        }
    }
}
