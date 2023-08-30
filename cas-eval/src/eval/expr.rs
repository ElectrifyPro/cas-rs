use cas_parser::parser::{assign::AssignTarget, expr::Expr};
use crate::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

impl Eval for Expr {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        match self {
            Expr::Literal(literal) => literal.eval(ctxt),
            Expr::Paren(paren) => paren.expr.eval(ctxt),
            Expr::Call(call) => call.eval(ctxt),
            Expr::Unary(unary) => unary.eval(ctxt),
            Expr::Binary(binary) => binary.eval(ctxt),
            Expr::Assign(assign) => {
                match &assign.target {
                    AssignTarget::Symbol(symbol) => {
                        // variable assignment
                        let value = assign.value.eval(ctxt)?;
                        ctxt.add_var(&symbol.name, value.clone());
                        Ok(value)
                    },
                    AssignTarget::Func(header) => {
                        // function assignment
                        ctxt.add_func(
                            header.clone(),
                            *assign.value.clone(),
                            assign.is_recursive(),
                        );
                        Ok(Value::Unit)
                    },
                }
            },
        }
    }
}
