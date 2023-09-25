use cas_parser::parser::ast::{expr::Expr, loop_expr::Loop, while_expr::While};
use crate::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

/// Trait for loop expressions that utilize a boolean condition to determine whether they should
/// terminate.
trait ConditionalLoop {
    fn condition(&self, ctxt: &mut Ctxt) -> Result<bool, Error>;
    fn body(&self) -> &Expr;

    /// Evaluates the loop expression.
    fn eval_loop(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        ctxt.loop_depth += 1;
        while self.condition(ctxt)? {
            let value = match &*self.body() {
                Expr::Literal(literal) => literal.eval(ctxt),
                Expr::Paren(paren) => paren.expr.eval(ctxt),
                Expr::Block(block) => block.eval(ctxt),
                Expr::If(if_expr) => if_expr.eval(ctxt),
                Expr::Loop(loop_expr) => loop_expr.eval(ctxt),
                Expr::While(while_expr) => while_expr.eval(ctxt),
                Expr::Break(break_expr) => break_expr.eval(ctxt),
                Expr::Continue(_) => continue,
                Expr::Call(call) => call.eval(ctxt),
                Expr::Unary(unary) => unary.eval(ctxt),
                Expr::Binary(binary) => binary.eval(ctxt),
                Expr::Assign(assign) => assign.eval(ctxt),
            }?;

            if ctxt.break_loop {
                ctxt.break_loop = false;
                ctxt.loop_depth -= 1;
                return Ok(value);
            }
        }

        ctxt.loop_depth -= 1;
        Ok(Value::Unit)
    }
}

impl ConditionalLoop for Loop {
    fn condition(&self, _: &mut Ctxt) -> Result<bool, Error> {
        Ok(true)
    }

    fn body(&self) -> &Expr {
        &self.body
    }
}

impl ConditionalLoop for While {
    fn condition(&self, ctxt: &mut Ctxt) -> Result<bool, Error> {
        Ok(self.condition.eval(ctxt)?.is_truthy())
    }

    fn body(&self) -> &Expr {
        &self.body
    }
}

impl Eval for Loop {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        self.eval_loop(ctxt)
    }
}

impl Eval for While {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        self.eval_loop(ctxt)
    }
}
