use super::expr::Expr;

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
                Expr::Literal(_) => return self.visit(),
                Expr::Paren(paren) => {
                    if self.is_last_visited(&paren.expr) {
                        return self.visit();
                    }
                    self.stack.push(&paren.expr);
                },
                Expr::Block(_) => return self.visit(), // NOTE: inner statements are not visited
                Expr::If(if_expr) => {
                    if self.is_last_visited(&if_expr.else_expr) {
                        return self.visit();
                    }
                    self.stack.push(&if_expr.else_expr);
                    self.stack.push(&if_expr.then_expr);
                    self.stack.push(&if_expr.condition);
                },
                Expr::Loop(loop_expr) => {
                    if self.is_last_visited(&loop_expr.body) {
                        return self.visit();
                    }
                    self.stack.push(&loop_expr.body);
                },
                Expr::Break(break_expr) => {
                    if let Some(value) = &break_expr.value {
                        if self.is_last_visited(value) {
                            return self.visit();
                        }
                        self.stack.push(value);
                    } else {
                        return self.visit();
                    }
                },
                Expr::Continue(_) => return self.visit(),
                Expr::Call(call) => {
                    if call.args.is_empty() || self.is_last_visited(call.args.last().unwrap()) {
                        return self.visit();
                    }
                    for arg in call.args.iter().rev() {
                        self.stack.push(arg);
                    }
                },
                Expr::Unary(unary) => {
                    if self.is_last_visited(&unary.operand) {
                        return self.visit();
                    }
                    self.stack.push(&unary.operand);
                },
                Expr::Binary(binary) => {
                    if self.is_last_visited(&binary.rhs) {
                        return self.visit();
                    }
                    self.stack.push(&binary.rhs);
                    self.stack.push(&binary.lhs);
                },
                Expr::Assign(assign) => {
                    if self.is_last_visited(&assign.value) {
                        return self.visit();
                    }
                    self.stack.push(&assign.value);
                },
            }
        }
    }
}
