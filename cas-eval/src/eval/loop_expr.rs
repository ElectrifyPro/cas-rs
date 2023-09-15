use cas_parser::parser::{expr::Expr, loop_expr::Loop};
use crate::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for Loop {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        ctxt.loop_depth += 1;
        loop {
            let value = match &*self.body {
                Expr::Literal(literal) => literal.eval(ctxt),
                Expr::Paren(paren) => paren.expr.eval(ctxt),
                Expr::Block(block) => block.eval(ctxt),
                Expr::If(if_expr) => if_expr.eval(ctxt),
                Expr::Loop(loop_expr) => loop_expr.eval(ctxt),
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
    }
}
