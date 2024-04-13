use cas_parser::parser::ast::expr::Expr;
use crate::numerical::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for Expr {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match self {
            Expr::Literal(literal) => literal.eval(ctxt),
            Expr::Paren(paren) => paren.expr.eval(ctxt),
            Expr::Block(block) => block.eval(ctxt),
            Expr::If(if_expr) => if_expr.eval(ctxt),
            Expr::Loop(loop_expr) => loop_expr.eval(ctxt),
            Expr::While(while_expr) => while_expr.eval(ctxt),
            Expr::Break(break_expr) => break_expr.eval(ctxt),
            Expr::Continue(continue_expr) => continue_expr.eval(ctxt),
            Expr::Return(_) => todo!(),
            Expr::Call(call) => call.eval(ctxt),
            Expr::Index(index) => index.eval(ctxt),
            Expr::Unary(unary) => unary.eval(ctxt),
            Expr::Binary(binary) => binary.eval(ctxt),
            Expr::Assign(assign) => assign.eval(ctxt),
        }
    }
}
