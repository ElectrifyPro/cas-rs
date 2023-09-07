use cas_parser::parser::expr::Primary;
use crate::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for Primary {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match self {
            Primary::Literal(literal) => literal.eval(ctxt),
            Primary::Paren(paren) => paren.expr.eval(ctxt),
            Primary::Block(block) => block.eval(ctxt),
            Primary::If(if_expr) => if_expr.eval(ctxt),
            Primary::Call(call) => call.eval(ctxt),
        }
    }
}
