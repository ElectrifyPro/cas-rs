use cas_compute::numerical::value::Value;
use cas_error::Error;
use cas_parser::parser::ast::{stmt::Stmt, AssignTarget, Expr};
use crate::{Compile, Compiler, InstructionKind};

/// Helper function to compile multiple statements.
pub fn compile_stmts(stmts: &[Stmt], compiler: &mut Compiler) -> Result<(), Error> {
    let Some((last, stmts)) = stmts.split_last() else {
        // nothing to compile
        compiler.add_instr(InstructionKind::LoadConst(Value::Unit));
        return Ok(());
    };

    compiler.with_state(|state| {
        state.last_stmt = false;
    }, |compiler| {
        stmts.iter()
            .try_for_each(|stmt| stmt.compile(compiler))
    })?;

    compiler.with_state(|state| {
        state.last_stmt = true;
    }, |compiler| {
        last.compile(compiler)
    })
}

impl Compile for Stmt {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.expr.compile(compiler)?;

        if let Expr::Assign(assign) = &self.expr && !matches!(assign.target, AssignTarget::Index(_)) {
            // for the most part, `Expr::Assign` will correctly determine if it should `StoreVar`
            // the value, leaving it on the stack, or `AssignVar` it, taking ownership of the value
            // to clone one time less. if `AssignVar` is used, we must not add an extra `Drop` like
            // in the `else` branch at the bottom of this function
            //
            // but `Expr::Assign` doesn't have enough information to know about this specific case
            // below, specifically, whether the last statement has a semicolon or not
            if compiler.state.last_stmt && self.semicolon.is_some() {
                // `t = 5` will always compile with a `StoreVar`
                //
                // {
                //     a = 2
                //     t = 5; <-- replace `5` on the stack with `()`
                // }
                compiler.add_instr(InstructionKind::Drop);
                compiler.add_instr(InstructionKind::LoadConst(Value::Unit));
            }

            // TODO: there is currently only one instruction for assigning to an `Index`
            // (`StoreIndexed`) and no `Assign` variant of it, so assigning to an index always
            // leaves the value on the stack which will have to be dropped. once we add
            // `AssignIndexed` we can get rid of `!matches!(assign.target, Index)`
            //
            // {
            //     a = [0]
            //     a[0] = 8 <-- `StoreIndexed`
            //     a[0] = 24; <-- `StoreIndexed`
            //     a[0] = 72 <-- `StoreIndexed`
            // }
        } else {
            // expressions that ALWAYS produces a value
            if self.semicolon.is_some() {
                compiler.add_instr(InstructionKind::Drop);
                compiler.add_instr(InstructionKind::LoadConst(Value::Unit));
            }

            if !compiler.state.last_stmt {
                compiler.add_instr(InstructionKind::Drop);
            }
        }
        Ok(())
    }
}
