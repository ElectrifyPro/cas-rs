use cas_parser::parser::{block::Block, stmt::Stmt};
use crate::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

/// Evaluates multiple statements, returning the value of the last one.
pub fn eval_stmts(stmts: &[Stmt], ctxt: &mut Ctxt) -> Result<Value, Error> {
    if stmts.is_empty() {
        return Ok(Value::Unit);
    }

    for stmt in stmts.iter().take(stmts.len() - 1) {
        let mut ctxt_old = ctxt.clone();
        let value = stmt.eval(&mut ctxt_old);

        if let Err(e) = value {
            return Err(e);
        } else {
            *ctxt = ctxt_old;
        }
    }

    stmts.last().unwrap().eval(ctxt)
}

impl Eval for Block {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        eval_stmts(&self.stmts, ctxt)
    }
}
