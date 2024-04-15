use cas_compute::numerical::value::Value;
use cas_parser::parser::ast::stmt::Stmt;
use crate::{error::Error, Compile, Compiler, InstructionKind};

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
        if !compiler.state.last_stmt {
            compiler.add_instr(InstructionKind::Drop);
        }
        Ok(())
    }
}
