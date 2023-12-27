use cas_parser::parser::ast::{block::Block, stmt::Stmt};
use crate::eval_break;
use crate::numerical::{ctxt::Ctxt, error::Error, eval::Eval, value::Value};

/// Evaluates multiple statements, returning the value of the last one.
pub fn eval_stmts(stmts: &[Stmt], ctxt: &mut Ctxt) -> Result<Value, Error> {
    if stmts.is_empty() {
        return Ok(Value::Unit);
    }

    for stmt in stmts.iter().take(stmts.len() - 1) {
        let mut ctxt_old = ctxt.clone();
        eval_break!(stmt, &mut ctxt_old);
        *ctxt = ctxt_old;
    }

    stmts.last().unwrap().eval(ctxt)
}

impl Eval for Block {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        eval_stmts(&self.stmts, ctxt)
    }
}
