use cas_compute::numerical::value::Value;
use cas_error::Error;
use cas_parser::parser::ast::{loop_expr::Loop, while_expr::While};
use crate::{Compile, Compiler, InstructionKind};

impl Compile for Loop {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        let loop_start = compiler.new_end_label();
        let loop_end = compiler.new_unassociated_label();

        compiler.with_state(|state| {
            // in case `continue` and `break` expressions are inside, we need the loop start and
            // end labels for their jumps
            state.loop_start = Some(loop_start);
            state.loop_end = Some(loop_end);
        }, |compiler| {
            self.body.compile(compiler)?;
            compiler.add_instr(InstructionKind::Drop);
            Ok(())
        })?;

        compiler.add_instr(InstructionKind::Jump(loop_start));

        // NOTE: we don't have to do the same "hack" as in `while` loops, since `loop`s cannot
        // terminate without a `break` expression

        compiler.set_end_label(loop_end);
        Ok(())
    }
}

impl Compile for While {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        let condition_start = compiler.new_end_label();
        self.condition.compile(compiler)?;

        let end_with_no_break = compiler.new_unassociated_label();
        let loop_end = compiler.new_unassociated_label();
        compiler.add_instr(InstructionKind::JumpIfFalse(end_with_no_break));
        compiler.with_state(|state| {
            // in case `continue` and `break` expressions are inside, we need the loop start and
            // end labels for their jumps
            state.loop_start = Some(condition_start);
            state.loop_end = Some(loop_end);
        }, |compiler| {
            self.body.compile(compiler)?;
            compiler.add_instr(InstructionKind::Drop);
            Ok(())
        })?;

        compiler.add_instr(InstructionKind::Jump(condition_start));

        // if the loop doesn't terminate through a `break` expression, we need to load something to
        // the stack so that the automatically generated `Drop` instruction has something to drop
        compiler.set_end_label(end_with_no_break);
        compiler.add_instr(InstructionKind::LoadConst(Value::Unit));

        compiler.set_end_label(loop_end);
        Ok(())
    }
}
