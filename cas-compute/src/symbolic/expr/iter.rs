use super::SymExpr;

/// An iterator that iteratively traverses the tree of expressions in left-to-right post-order
/// (i.e. depth-first).
///
/// This iterator is created by [`Expr::post_order_iter`].
pub struct ExprIter<'a> {
    stack: Vec<&'a SymExpr>,
    last_visited: Option<&'a SymExpr>,
}

impl<'a> ExprIter<'a> {
    /// Creates a new iterator that traverses the tree of expressions in left-to-right post-order
    /// (i.e. depth-first).
    pub fn new(expr: &'a SymExpr) -> Self {
        Self {
            stack: vec![expr],
            last_visited: None,
        }
    }

    /// Pops the current expression in the stack and marks it as the last visited expression.
    fn visit(&mut self) -> Option<&'a SymExpr> {
        self.last_visited = Some(self.stack.pop()?);
        self.last_visited
    }

    /// Returns true if the given expression matches the last visited expression.
    fn is_last_visited(&self, expr: &'a SymExpr) -> bool {
        match self.last_visited {
            Some(last_visited) => std::ptr::eq(last_visited, expr),
            None => false,
        }
    }
}

impl<'a> Iterator for ExprIter<'a> {
    type Item = &'a SymExpr;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let expr = self.stack.last()?;
            match expr {
                SymExpr::Primary(_) => return self.visit(),
                SymExpr::Add(terms) => {
                    if terms.is_empty() || self.is_last_visited(terms.last().unwrap()) {
                        return self.visit();
                    }
                    for term in terms.iter().rev() {
                        self.stack.push(term);
                    }
                },
                SymExpr::Mul(factors) => {
                    if factors.is_empty() || self.is_last_visited(factors.last().unwrap()) {
                        return self.visit();
                    }
                    for factor in factors.iter().rev() {
                        self.stack.push(factor);
                    }
                },
                SymExpr::Exp(lhs, rhs) => {
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
