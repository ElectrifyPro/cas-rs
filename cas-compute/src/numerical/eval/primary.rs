use cas_parser::parser::ast::expr::Primary;
use crate::numerical::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for Primary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match self {
            Primary::Literal(literal) => literal.eval(ctxt),
            Primary::Paren(paren) => paren.expr.eval(ctxt),
            Primary::Block(block) => block.eval(ctxt),
            Primary::If(if_expr) => if_expr.eval(ctxt),
            Primary::Loop(loop_expr) => loop_expr.eval(ctxt),
            Primary::While(while_expr) => while_expr.eval(ctxt),
            Primary::Break(break_expr) => break_expr.eval(ctxt),
            Primary::Continue(continue_expr) => continue_expr.eval(ctxt),
            Primary::Return(_) => todo!(),
            Primary::Call(call) => call.eval(ctxt),
        }
    }
}
