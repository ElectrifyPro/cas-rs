use cas_compute::numerical::value::Value;
use cas_parser::parser::ast::{loop_expr::Loop, while_expr::While};
use crate::{error::Error, Compile, Compiler, Instruction};

impl Compile for Loop {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        // TODO: this is a hack to ensure the loop always returns something so that the
        // automatically generated `Drop` instruction drops this value if the loop doesn't end up
        // producing anything
        //
        // the ideal solution would be to not emit this `LoadConst` if the loop body contains a
        // `break` expression
        compiler.add_instr(Instruction::LoadConst(Value::Unit));

        let loop_start = compiler.new_end_label();
        let loop_end = compiler.new_unassociated_label();

        compiler.with_state(|state| {
            // in case `continue` and `break` expressions are inside, we need the loop start and
            // end labels for their jumps
            state.loop_start = Some(loop_start);
            state.loop_end = Some(loop_end);
        }, |compiler| {
            self.body.compile(compiler)?;
            compiler.add_instr(Instruction::Drop);
            Ok(())
        })?;

        compiler.add_instr(Instruction::Jump(loop_start));
        compiler.set_end_label(loop_end);
        Ok(())
    }
}

impl Compile for While {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        // TODO: see above
        compiler.add_instr(Instruction::LoadConst(Value::Unit));

        let condition_start = compiler.new_end_label();
        self.condition.compile(compiler)?;

        let loop_end = compiler.new_unassociated_label();
        compiler.add_instr(Instruction::JumpIfFalse(loop_end));
        compiler.with_state(|state| {
            // in case `continue` and `break` expressions are inside, we need the loop start and
            // end labels for their jumps
            state.loop_start = Some(condition_start);
            state.loop_end = Some(loop_end);
        }, |compiler| {
            self.body.compile(compiler)?;
            compiler.add_instr(Instruction::Drop);
            Ok(())
        })?;

        compiler.add_instr(Instruction::Jump(condition_start));
        compiler.set_end_label(loop_end);
        Ok(())
    }
}

// ex:
// i = 0
// while i < 10 then {
//    i = i + 1
// }
//
// becomes
//
// LoadConst 0
// StoreVar i
// Label condition_start
//   LoadVar i
//   LoadConst 10
//   LessThan
//   JumpIfFalse loop_end
//
//   LoadVar i
//   LoadConst 1
//   Add
//   StoreVar i
//   Jump condition_start
// Label loop_end
