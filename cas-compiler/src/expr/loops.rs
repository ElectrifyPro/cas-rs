use cas_parser::parser::ast::{loop_expr::Loop, while_expr::While};
use crate::{Compiler, Compile, Instruction};

impl Compile for Loop {
    fn compile(&self, compiler: &mut Compiler) {
        let loop_start = compiler.new_end_label();
        let loop_end = compiler.new_unassociated_label();
        compiler.with_state(|state| {
            // in case `continue` and `break` expressions are inside, we need the loop start and
            // end labels for their jumps
            state.loop_start = Some(loop_start);
            state.loop_end = Some(loop_end);
        }, |compiler| {
            self.body.compile(compiler);
            compiler.add_instr(Instruction::Drop);
        });
        compiler.add_instr(Instruction::Jump(loop_start));
        compiler.set_end_label(loop_end);
    }
}

impl Compile for While {
    fn compile(&self, compiler: &mut Compiler) {
        let condition_start = compiler.new_end_label();
        self.condition.compile(compiler);

        let loop_end = compiler.new_unassociated_label();
        compiler.add_instr(Instruction::JumpIfFalse(loop_end));
        compiler.with_state(|state| {
            // in case `continue` and `break` expressions are inside, we need the loop start and
            // end labels for their jumps
            state.loop_start = Some(condition_start);
            state.loop_end = Some(loop_end);
        }, |compiler| {
            self.body.compile(compiler);
            compiler.add_instr(Instruction::Drop);
        });

        compiler.add_instr(Instruction::Jump(condition_start));
        compiler.set_end_label(loop_end);
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
