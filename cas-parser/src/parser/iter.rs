use super::ast::expr::Expr;

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
                Expr::Block(_) => return self.visit(), // NOTE: inner statements are not visited; me in 2024-27-09: why?
                Expr::Sum(sum) => {
                    if self.is_last_visited(&sum.body) {
                        return self.visit();
                    }
                    self.stack.push(&sum.body);
                    // TODO: should the range and variable be visited?
                },
                Expr::Product(product) => {
                    if self.is_last_visited(&product.body) {
                        return self.visit();
                    }
                    self.stack.push(&product.body);
                    // TODO: should the range and variable be visited?
                },
                Expr::If(if_expr) => {
                    if let Some(else_expr) = &if_expr.else_expr {
                        if self.is_last_visited(else_expr) {
                            return self.visit();
                        }
                        self.stack.push(else_expr);
                        self.stack.push(&if_expr.then_expr);
                        self.stack.push(&if_expr.condition);
                    } else {
                        if self.is_last_visited(&if_expr.then_expr) {
                            return self.visit();
                        }
                        self.stack.push(&if_expr.then_expr);
                        self.stack.push(&if_expr.condition);
                    }
                },
                Expr::Loop(loop_expr) => {
                    if self.is_last_visited(&loop_expr.body) {
                        return self.visit();
                    }
                    self.stack.push(&loop_expr.body);
                },
                Expr::While(while_expr) => {
                    if self.is_last_visited(&while_expr.body) {
                        return self.visit();
                    }
                    self.stack.push(&while_expr.body);
                    self.stack.push(&while_expr.condition);
                },
                Expr::Then(then) => {
                    if self.is_last_visited(&then.expr) {
                        return self.visit();
                    }
                    self.stack.push(&then.expr);
                },
                Expr::Of(of) => {
                    if self.is_last_visited(&of.expr) {
                        return self.visit();
                    }
                    self.stack.push(&of.expr);
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
                Expr::Return(_) => return self.visit(),
                Expr::Call(call) => {
                    if call.args.is_empty() || self.is_last_visited(call.args.last().unwrap()) {
                        return self.visit();
                    }
                    for arg in call.args.iter().rev() {
                        self.stack.push(arg);
                    }
                },
                Expr::Index(index) => {
                    if self.is_last_visited(&index.index) {
                        return self.visit();
                    }
                    self.stack.push(&index.index);
                    self.stack.push(&index.target);
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
                Expr::Range(range) => {
                    if self.is_last_visited(&range.end) {
                        return self.visit();
                    }
                    self.stack.push(&range.end);
                    self.stack.push(&range.start);
                },
            }
        }
    }
}
