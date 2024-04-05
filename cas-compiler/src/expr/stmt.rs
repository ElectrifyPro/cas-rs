use cas_parser::parser::ast::stmt::Stmt;
use crate::{Compiler, Compile, Instruction};

/// Helper function to compile multiple statements.
pub fn compile_stmts(stmts: &[Stmt], compiler: &mut Compiler) {
    let Some((last, stmts)) = stmts.split_last() else {
        return;
    };

    compiler.with_state(|state| {
        state.last_stmt = false;
    }, |compiler| {
        stmts.iter()
            .for_each(|stmt| stmt.compile(compiler));
    });

    compiler.with_state(|state| {
        state.last_stmt = true;
    }, |compiler| {
        last.compile(compiler);
    });
}

impl Compile for Stmt {
    fn compile(&self, compiler: &mut Compiler) {
        self.expr.compile(compiler);
        if !compiler.state.last_stmt {
            compiler.add_instr(Instruction::Drop);
        }
    }
}
